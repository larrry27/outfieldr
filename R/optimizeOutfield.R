#' optimizeOutfield.R
#' 
#' @title "Generates three plots that show optimal outfielder positioning given three fielders and a batter."
#' @param batterId The MLB player ID for which batted ball data is grabbed
#' @param leftfieldId The MLB player ID for the left fielder to optimize. Input -7 to use a league-average LF.
#' @param centerfieldId The MLB player ID for the center fielder to optimize. Input -8 to use a league-average CF.
#' @param rightfieldId The MLB player ID for the right fielder to optimize. Input -9 to use a league-average RF.
#' @param balls The maximum number of batted balls that are taken, to be split across fielders.
#' @param year The season for which data is grabbed.
#' @param parkid The integer ID for park for which data is grabbed.
#' @import ggplot2
#' @import ggforce
#' @import dplyr
#' @import baseballr
#' @import sf
#' @rawNamespace import(data.table, except = c(first, last, between))
#' @import lattice
#' @export
#' @examples {optimizeOutfield(691718,803011,671976,695731,50,2026,11)}

optimizeOutfield <- function(batterId, leftfieldId, centerfieldId, rightfieldId, n, year, parkid) {

  ## Error handling
  if(is.numeric(batterId) == FALSE) { stop("batterId must be a positive integer.") }
  if(batterId <= 0 | batterId %% 1 != 0) { stop("batterId must be a positive integer.") }
  if(is.numeric(leftfieldId) == FALSE) { stop("(LF) playerid must be a positive integer, or within (-7,-8,-9).") }
  if((leftfieldId < 0 & !(leftfieldId %in% c(-7,-8,-9))) | leftfieldId %% 1 != 0) { stop("(LF) playerid must be a positive integer, or within (-7,-8,-9).") }
  if(is.numeric(centerfieldId) == FALSE) { stop("(CF) playerid must be a positive integer, or within (-7,-8,-9).") }
  if((centerfieldId < 0 & !(centerfieldId %in% c(-7,-8,-9))) | centerfieldId %% 1 != 0) { stop("(CF) playerid must be a positive integer, or within (-7,-8,-9).") }
  if(is.numeric(rightfieldId) == FALSE) { stop("(RF) playerid must be a positive integer, or within (-7,-8,-9).") }
  if((rightfieldId < 0 & !(rightfieldId %in% c(-7,-8,-9))) | rightfieldId %% 1 != 0) { stop("(RF) playerid must be a positive integer, or within (-7,-8,-9).") }
  if(is.numeric(year) == FALSE) { stop("Year value must be an integer greater than or equal to 2020.") }
  if(year <= 2019 | year %% 1 != 0) { stop("Year value must be an integer greater than or equal to 2020.") }
  if(is.numeric(n) == FALSE) { stop("n must be a positive integer.") }
  if(n <= 0 | n %% 1 != 0) { stop("n must be a positive integer.") }
  if(is.numeric(parkid) == FALSE) { stop("Invalid parkid. Must be a positive integer between 1 and 37.") }
  if((parkid < 1 | parkid > 37) | parkid %% 1 != 0) { stop("Invalid parkid. Must be a positive integer between 1 and 37.") }
  
  ## Get list of batted balls by region
  # LF: 1/4, CF: 2/5, RF: 3/6
  
  ballsToAssess <- getBattedBallData(batterId, year, n, parkid)
  
  ballsToAssessLF <- ballsToAssess %>% dplyr::filter(region %in% c(1,4))
  ballsToAssessCF <- ballsToAssess %>% dplyr::filter(region %in% c(2,5))
  ballsToAssessRF <- ballsToAssess %>% dplyr::filter(region %in% c(3,6))
  
  ## Methodology
  # If outfielder can reach batted ball location with .4 seconds to spare, it's a catch
  # The circle in which an outfielder can start and still reach the ball is that ball's "competitive area"
  # Each "competitive area" is a list of (x,y) points, resolution of .25 feet, that are within the circle
  # After all batted balls are assessed, the points with the greatest number of instances constitute the "ideal range"
  # The centroid of the ideal range is the ideal position for the fielder versus the batter
  
  ## Left fielder: Initialize competitive areas and get statistics
  leftFielder <- getFielderStats(leftfieldId,year)
  compAreasLF <- data.frame(x=NULL,y=NULL)
  
  ## Center fielder: Initialize competitive areas and get statistics
  cntrFielder <- getFielderStats(centerfieldId, year)
  compAreasCF <- data.frame(x=NULL,y=NULL)
  
  ## Right fielder: Initialize competitive areas and get statistics
  rghtFielder <- getFielderStats(rightfieldId, year)
  compAreasRF <- data.frame(x=NULL,y=NULL)
  
  ## Iterate over applicable batted balls for each fielder
  ballsToAssessLF <- ballsToAssessLF %>% rowwise() %>% mutate(compAreasLF = list(getCompetitiveArea(t_x, t_y, (hangTime-.4) * leftFielder$sprint_speed + leftFielder$jump))) %>% ungroup()
  ballsToAssessCF <- ballsToAssessCF %>% rowwise() %>% mutate(compAreasCF = list(getCompetitiveArea(t_x, t_y, (hangTime-.4) * cntrFielder$sprint_speed + cntrFielder$jump))) %>% ungroup()
  ballsToAssessRF <- ballsToAssessRF %>% rowwise() %>% mutate(compAreasRF = list(getCompetitiveArea(t_x, t_y, (hangTime-.4) * rghtFielder$sprint_speed + rghtFielder$jump))) %>% ungroup()
  
  ## Combine competitive areas for each batted ball and stage as data tables
  compAreasLF <- bind_rows(ballsToAssessLF$compAreasLF)
  compAreasCF <- bind_rows(ballsToAssessCF$compAreasCF)
  compAreasRF <- bind_rows(ballsToAssessRF$compAreasRF)
  
  setDT(compAreasLF)
  setDT(compAreasCF)
  setDT(compAreasRF)

  ## Group by count and find most populated areas while eliminating all others, per fielder
  optimalAreaLF <-compAreasLF[, .N, by = .(x,y)] %>% filter(N == max(N, na.rm = TRUE))
  optimalAreaCF <-compAreasCF[, .N, by = .(x,y)] %>% filter(N == max(N, na.rm = TRUE))
  optimalAreaRF <-compAreasRF[, .N, by = .(x,y)] %>% filter(N == max(N, na.rm = TRUE))
  
  ## Find centroid of most populated area to determine pointwise optimal position
  optimalHullLF <- subset(optimalAreaLF %>% slice(chull(x, y)), select=-N)
  optimalHullCF <- subset(optimalAreaCF %>% slice(chull(x, y)), select=-N)
  optimalHullRF <- subset(optimalAreaRF %>% slice(chull(x, y)), select=-N)
  optimalPointLF <- c(x = mean(optimalHullLF$x), y = mean(optimalHullLF$y))
  optimalPointCF <- c(x = mean(optimalHullCF$x), y = mean(optimalHullCF$y))
  optimalPointRF <- c(x = mean(optimalHullRF$x), y = mean(optimalHullRF$y))
  
  ## Plot (Left Field)
  outfieldPlotLF <- ggplot(ballsToAssessLF) +
    geom_circle(aes(x0=t_x, y0=t_y, r= (hangTime - .4) * leftFielder$sprint_speed + leftFielder$jump), alpha=0.05, fill="#EEBBBB", linetype="dotted") +
    scale_size_identity() +
    labs(title = paste(leftFielder$last_name..first_name, "vs", ballsToAssessLF[8]$player_name,"| n =",nrow(ballsToAssessLF))) +
    coord_cartesian(xlim = c(-350,350), ylim = c(-50,475)) +
    annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "black",linetype="dotted",linewidth=1) +
    annotate(geom="polygon", x=parks$path[[parkid]]$x, y=parks$path[[parkid]]$y,fill=NA,color="black",linewidth=1) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    guides(size = "none")
  
  outfieldPlotLF <- outfieldPlotLF + geom_polygon(data = optimalHullLF, aes(x=x,y=y,size=1), color="yellow",fill="yellow", linewidth=.5, inherit.aes = FALSE)
  outfieldPlotLF <- outfieldPlotLF + geom_point(data=as.data.frame(list(x=optimalPointLF[1],y=optimalPointLF[2])), aes(x=x,y=y,size=1),color="yellow")
  print(outfieldPlotLF)
  
  ## Plot (Center Field)
  outfieldPlotCF <- ggplot(ballsToAssessCF) +
    geom_circle(aes(x0=t_x, y0=t_y, r= (hangTime - .4) * cntrFielder$sprint_speed + cntrFielder$jump), alpha=0.05, fill="#BBEEBB", linetype="dotted") +
    scale_size_identity() +
    labs(title = paste(cntrFielder$last_name..first_name, "vs", ballsToAssessLF[8]$player_name,"| n =",nrow(ballsToAssessCF))) +
    coord_cartesian(xlim = c(-350,350), ylim = c(-50,475)) +
    annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "black",linetype="dotted",linewidth=1) +
    annotate(geom="polygon", x=parks$path[[parkid]]$x, y=parks$path[[parkid]]$y,fill=NA,color="black",linewidth=1) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    guides(size = "none")
  
  outfieldPlotCF <- outfieldPlotCF + geom_polygon(data = optimalHullCF, aes(x=x,y=y,size=1), color="yellow",fill="yellow", linewidth=.5, inherit.aes = FALSE)
  outfieldPlotCF <- outfieldPlotCF + geom_point(data=as.data.frame(list(x=optimalPointCF[1],y=optimalPointCF[2])), aes(x=x,y=y,size=1),color="yellow")
  print(outfieldPlotCF)
  
  ## Plot (Right Field)
  outfieldPlotRF <- ggplot(ballsToAssessRF) +
    geom_circle(aes(x0=t_x, y0=t_y, r= (hangTime - .4) * rghtFielder$sprint_speed + rghtFielder$jump), alpha=0.05, fill="#BBBBEE", linetype="dotted") +
    scale_size_identity() +
    labs(title = paste(rghtFielder$last_name..first_name, "vs", ballsToAssessLF[8]$player_name,"| n =",nrow(ballsToAssessRF))) +
    coord_cartesian(xlim = c(-350,350), ylim = c(-50,475)) +
    annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "black",linetype="dotted",linewidth=1) +
    annotate(geom="polygon", x=parks$path[[parkid]]$x, y=parks$path[[parkid]]$y,fill=NA,color="black",linewidth=1) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    guides(size = "none")
  
  outfieldPlotRF <- outfieldPlotRF + geom_polygon(data = optimalHullRF, aes(x=x,y=y,size=1), color="yellow",fill="yellow", linewidth=.5, inherit.aes = FALSE)
  outfieldPlotRF <- outfieldPlotRF + geom_point(data=as.data.frame(list(x=optimalPointRF[1],y=optimalPointRF[2])), aes(x=x,y=y,size=1),color="yellow")
  print(outfieldPlotRF)
  
  cat(paste("Optimal Positioning vs.",ballsToAssessLF[8]$player_name[1]),"\n",
  paste(n,"most recent batted balls analyzed -",nrow(ballsToAssessLF),"/",nrow(ballsToAssessCF),"/",nrow(ballsToAssessRF),"split"),"\n",
  paste("LF -",leftFielder$last_name..first_name,"| x =",optimalPointLF[1],"| y =",optimalPointLF[2]),"\n",
  paste("CF -",cntrFielder$last_name..first_name,"| x =",optimalPointCF[1],"| y =",optimalPointCF[2]),"\n",
  paste("RF -",rghtFielder$last_name..first_name,"| x =",optimalPointRF[1],"| y =",optimalPointRF[2]))
}