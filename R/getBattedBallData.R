#' getBattedBallData.R
#' 
#' Generates a list of the last n batted balls by a given player, in a given season
#' @title "Generate a list of n batted balls for a player in a season."
#' @param playerid The MLB player ID for which the batted ball data is grabbed
#' @param year The season for which batted ball data is grabbed
#' @param n The number of batted balls grabbed, sorted by date descending / recency
#' @param parkid The integer ID of the park in which batted balls are grabbed.
#' @returns A dataframe containing batted ball information: location, region, hang time, launch speed/angle
#' @import baseballr
#' @import dplyr
#' @import tidyr
#' @import sp
#' @export
#' @examples {getBattedBallData(805367, 2026, 200, 11)}

getBattedBallData <- function(playerid, year, n, parkid) {

  ## Error handling
  if(is.numeric(playerid) == FALSE) { stop("playerid must be a positive integer.") }
  if(playerid <= 0 | playerid %% 1 != 0) { stop("playerid must be a positive integer.") }
  if(is.numeric(year) == FALSE) { stop("Year value must be an integer greater than or equal to 2020.") }
  if(year <= 2019 | year %% 1 != 0) { stop("Year value must be an integer greater than or equal to 2020.") }
  if(is.numeric(n) == FALSE) { stop("n must be a positive integer.") }
  if(n <= 0 | n %% 1 != 0) { stop("n must be a positive integer.") }
  if(is.numeric(parkid) == FALSE) { stop("Invalid parkid. Must be a positive integer between 1 and 37.") }
  if((parkid < 1 | parkid > 37) | parkid %% 1 != 0) { stop("Invalid parkid. Must be a positive integer between 1 and 37.") }
  
  ## Load parks data and get Statcast data
  battedBallData <- statcast_search(paste0(year,"-03-01"),paste0(year,"-12-01"),playerid = playerid,player_type = 'batter')

  filteredData <- battedBallData %>%
    dplyr::filter(!is.na(hc_x)) %>% # Make sure that ball has hit location data
    dplyr::filter(hit_distance_sc >= 127) %>% # Make sure ball gets past infield
    select(hc_x, hc_y, hit_distance_sc, game_date, launch_speed, launch_angle, events, player_name)

  ## Get distance data based on hc coordinates and spray angle, and place within region
  # https://beanumber.github.io/abdwr3e/C_statcast.html#sec-spray-angle
  filteredData$sprayAngle <- atan((filteredData$hc_x- 125.42)/(198.27 - filteredData$hc_y))
  filteredData$t_x <- sin(filteredData$sprayAngle) * filteredData$hit_distance_sc
  filteredData$t_y <- cos(filteredData$sprayAngle) * filteredData$hit_distance_sc
  filteredData$region <- ifelse(filteredData$hit_distance_sc >= 240,3,0) + ifelse(abs(filteredData$sprayAngle) < (pi/9),2,ifelse(filteredData$sprayAngle > 0,3,1))

  ## Filter out balls that are outside the polygon constructed by the outfield wall
  filteredData$inPark <- point.in.polygon(filteredData$t_x, filteredData$t_y, parks[[6]][[parkid]]$x, parks[[6]][[parkid]]$y)
  filteredData <- filteredData %>% dplyr::filter(inPark == TRUE)
  
  ## If no batted ball data exists, stop function and warn user.
  if(nrow(filteredData) == 0) { stop("No data was found for specified player, year, and park.") }
  
  ## Get estimated hang time for ball using launch angle and exit velocity
  # 22/15 - MPH to ft/s conversion
  # 32.174 - g in ft/s
  filteredData$hangTime <- 2 * filteredData$launch_speed * (22/15) * sin(filteredData$launch_angle * (pi/180)) / 32.174

  head(filteredData,n)
}
