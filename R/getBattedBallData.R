#' getBattedBallData.R
#' 
#' Generates a list of the last n batted balls by a given player, in a given season
#' @title "Generate a list of n batted balls for a player in a season."
#' @param playerid The MLB player ID for which the batted ball data is grabbed
#' @param year The season for which batted ball data is grabbed
#' @param n The number of batted balls grabbed, sorted by date descending / recency
#' @returns A dataframe containing batted ball information: location, region, hang time, launch speed/angle
#' @import baseballr
#' @import dplyr
#' @export
#' @examples {getBattedBallData(805367, 2026, 200)}

getBattedBallData <- function(playerid, year, n) {

  battedBallData <- statcast_search(paste0(year,"-03-01"),paste0(year,"-12-01"),playerid = playerid,player_type = 'batter')

  filteredData <- battedBallData %>%
    dplyr::filter(!is.na(hc_x)) %>% # Make sure that ball has hit location data
    dplyr::filter(hit_distance_sc >= 127) %>% # Make sure ball gets past infield
    dplyr::filter(events != "home_run") %>% # Ignore home runs
    select(hc_x, hc_y, hit_distance_sc, game_date, launch_speed, launch_angle, events, player_name)

  ## Get distance data based on hc coordinates and spray angle, and place within region
  # https://beanumber.github.io/abdwr3e/C_statcast.html#sec-spray-angle
  filteredData$sprayAngle <- atan((filteredData$hc_x- 125.42)/(198.27 - filteredData$hc_y))
  filteredData$t_x <- sin(filteredData$sprayAngle) * filteredData$hit_distance_sc
  filteredData$t_y <- cos(filteredData$sprayAngle) * filteredData$hit_distance_sc
  filteredData$region <- ifelse(filteredData$hit_distance_sc >= 240,3,0) + ifelse(abs(filteredData$sprayAngle) < (pi/9),2,ifelse(filteredData$sprayAngle > 0,3,1))

  ## Get estimated hang time for ball using launch angle and exit velocity
  # 22/15 - MPH to ft/s conversion
  # 32.174 - g in ft/s
  filteredData$hangTime <- 2 * filteredData$launch_speed * (22/15) * sin(filteredData$launch_angle * (pi/180)) / 32.174

  head(filteredData,n)
}
