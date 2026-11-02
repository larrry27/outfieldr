#' optimizeOutfield.R
#' 
#' @title "Generates three plots that show optimal outfielder positioning given three fielders and a batter."
#' @param batterId The MLB player ID for which batted ball data is grabbed
#' @param leftfieldId The MLB player ID for the left fielder to optimize. Input -7 to use a league-average LF.
#' @param centerfieldId The MLB player ID for the center fielder to optimize. Input -8 to use a league-average CF.
#' @param rightfieldId The MLB player ID for the right fielder to optimize. Input -9 to use a league-average RF.
#' @param n The maximum number of batted balls that are taken.
#' @param year The season for which data is grabbed
#' @export
#' @examples {optimizeOutfield(691718,803011,671976,695731,50,2026)}

optimizeOutfield <- function(batterId, leftfieldId, centerfieldId, rightfieldId, n, year) {

  ### Get list of batted balls by region
  ## LF: 1/4, CF: 2/5, RF: 3/6
  
  ballsToAssessLF <- getBattedBallData(batterId, year, n) %>% dplyr::filter(region %in% c(1,4))
  ballsToAssessCF <- getBattedBallData(batterId, year, n) %>% dplyr::filter(region %in% c(2,5))
  ballsToAssessRF <- getBattedBallData(batterId, year, n) %>% dplyr::filter(region %in% c(3,6))
  
  ## If outfielder can reach batted ball location with .4 seconds to spare, it's a catch
  ## The circle in which an outfielder can start and still reach the ball is that ball's "competitive area"
  ## Each "competitive area" is a list of (x,y) points, resolution of .25 feet, that are within the circle
  ## After all batted balls are assessed, the points with the greatest number of instances constitute the "ideal range"
  ## The centroid of the ideal range is the ideal position for the fielder versus the batter
  
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
  ballsToAssessLF <- ballsToAssessLF %>% rowwise() %>% mutate(compAreasLF = list(getCompetitiveArea(t_x, t_y, (hangTime-.4) * leftFielder$sprint_speed))) %>% ungroup()
  ballsToAssessCF <- ballsToAssessCF %>% rowwise() %>% mutate(compAreasCF = list(getCompetitiveArea(t_x, t_y, (hangTime-.4) * cntrFielder$sprint_speed))) %>% ungroup()
  ballsToAssessRF <- ballsToAssessRF %>% rowwise() %>% mutate(compAreasRF = list(getCompetitiveArea(t_x, t_y, (hangTime-.4) * rghtFielder$sprint_speed))) %>% ungroup()
  
  ## Combine competitive areas for each batted ball
  compAreasLF <- bind_rows(ballsToAssessLF$compAreasLF)
  compAreasCF <- bind_rows(ballsToAssessCF$compAreasCF)
  compAreasRF <- bind_rows(ballsToAssessRF$compAreasRF)
  
  ## Group by count and find most populated areas while eliminating all others, per fielder
  setDT(compAreasLF)
  optimalAreaLF <-compAreasLF[, .N, by = .(x,y)] %>% filter(N == max(N, na.rm = TRUE))
  setDT(compAreasCF)
  optimalAreaCF <-compAreasCF[, .N, by = .(x,y)] %>% filter(N == max(N, na.rm = TRUE))
  setDT(compAreasRF)
  optimalAreaRF <-compAreasRF[, .N, by = .(x,y)] %>% filter(N == max(N, na.rm = TRUE))
  
  ## Find centroid of most populated area to determine pointwise optimal position
  optimalHullLF <- subset(optimalAreaLF %>% slice(chull(x, y)), select=-N)
  optimalHullCF <- subset(optimalAreaCF %>% slice(chull(x, y)), select=-N)
  optimalHullRF <- subset(optimalAreaRF %>% slice(chull(x, y)), select=-N)
  optimalPointLF <- c(x = mean(optimalHullLF$x), y = mean(optimalHullLF$y))
  optimalPointCF <- c(x = mean(optimalHullCF$x), y = mean(optimalHullCF$y))
  optimalPointRF <- c(x = mean(optimalHullRF$x), y = mean(optimalHullRF$y))
  
  ### Plot (Left Field)
  outfieldPlotLF <- ggplot(ballsToAssessLF) +
    geom_circle(aes(x0=t_x, y0=t_y, r= (hangTime - .4) * leftFielder$sprint_speed), alpha=0.05, fill="#EEBBBB", linetype="dotted") +
    scale_size_identity() +
    labs(title = paste(leftFielder$last_name..first_name, "vs", ballsToAssessLF[8]$player_name,"| n=",nrow(ballstoAssessLF))) +
    coord_cartesian(xlim = c(-350,350), ylim = c(-50,475)) +
    annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "black",linetype="dotted",linewidth=1) +
    #annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "tan",fill="tan",linewidth=1) +
    annotate(geom="segment",x = 0, y = 0, xend = -230, yend = 230, color = "black", linewidth=1) +
    annotate(geom="segment",x = 0, y = 0, xend = 230, yend = 230, color = "black", linewidth=1) +
    annotate(geom="curve",x=230,xend=-230,y=230,yend=230,curvature=0.65,color = "black",linewidth=1) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    guides(size = "none")
  
  outfieldPlotLF <- outfieldPlotLF + geom_polygon(data = optimalHullLF, aes(x=x,y=y,size=1), color="yellow",fill="yellow", linewidth=.5, inherit.aes = FALSE)
  outfieldPlotLF <- outfieldPlotLF + geom_point(data=as.data.frame(list(x=optimalPointLF[1],y=optimalPointLF[2])), aes(x=x,y=y,size=1),color="yellow")
  print(outfieldPlotLF)
  
  ### Plot (Center Field)
  outfieldPlotCF <- ggplot(ballsToAssessCF) +
    geom_circle(aes(x0=t_x, y0=t_y, r= (hangTime - .4) * cntrFielder$sprint_speed), alpha=0.05, fill="#BBEEBB", linetype="dotted") +
    scale_size_identity() +
    labs(title = paste(cntrFielder$last_name..first_name, "vs", ballsToAssessLF[8]$player_name,"| n=",nrow(ballstoAssessCF))) +
    coord_cartesian(xlim = c(-350,350), ylim = c(-50,475)) +
    annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "black",linetype="dotted",linewidth=1) +
    #annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "tan",fill="tan",linewidth=1) +
    annotate(geom="segment",x = 0, y = 0, xend = -230, yend = 230, color = "black", linewidth=1) +
    annotate(geom="segment",x = 0, y = 0, xend = 230, yend = 230, color = "black", linewidth=1) +
    annotate(geom="curve",x=230,xend=-230,y=230,yend=230,curvature=0.65,color = "black",linewidth=1) +
    theme(
      panel.background = element_blank(),
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank()
    ) +
    guides(size = "none")
  
  outfieldPlotCF <- outfieldPlotCF + geom_polygon(data = optimalHullCF, aes(x=x,y=y,size=1), color="yellow",fill="yellow", linewidth=.5, inherit.aes = FALSE)
  outfieldPlotCF <- outfieldPlotCF + geom_point(data=as.data.frame(list(x=optimalPointCF[1],y=optimalPointCF[2])), aes(x=x,y=y,size=1),color="yellow")
  print(outfieldPlotCF)
  
  ### Plot (Right Field)
  outfieldPlotRF <- ggplot(ballsToAssessRF) +
    geom_circle(aes(x0=t_x, y0=t_y, r= (hangTime - .4) * rghtFielder$sprint_speed), alpha=0.05, fill="#BBBBEE", linetype="dotted") +
    scale_size_identity() +
    labs(title = paste(rghtFielder$last_name..first_name, "vs", ballsToAssessLF[8]$player_name,"| n=",nrow(ballstoAssessRF))) +
    coord_cartesian(xlim = c(-350,350), ylim = c(-50,475)) +
    annotate(geom="curve",x=90.23,xend=-90.23,y=90.23,yend=90.23,curvature=.722,color = "black",linetype="dotted",linewidth=1) +
    annotate(geom="segment",x = 0, y = 0, xend = -230, yend = 230, color = "black", linewidth=1) +
    annotate(geom="segment",x = 0, y = 0, xend = 230, yend = 230, color = "black", linewidth=1) +
    annotate(geom="curve",x=230,xend=-230,y=230,yend=230,curvature=0.65,color = "black",linewidth=1) +
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
  paste(n,"most recent batted balls analyzed -",nrow(ballstoAssessLF),"/",nrow(ballstoAssessCF),"/",nrow(ballstoAssessRF),"split"),"\n",
  paste("LF -",leftFielder$last_name..first_name,"| x =",optimalPointLF[1],"| y = ",optimalPointLF[2]),"\n",
  paste("CF -",cntrFielder$last_name..first_name,"| x =",optimalPointCF[1],"| y = ",optimalPointCF[2]),"\n",
  paste("RF -",rghtFielder$last_name..first_name,"| x =",optimalPointRF[1],"| y = ",optimalPointRF[2]))
}