#' getFielderStats.R
#' 
#' Generates a statistical profile for a fielder from Statcast data
#' @title "Generate a statistical profile for a fielder from Statcast data."
#' @param playerid The MLB player ID for which data is grabbed
#' @param year The season for which data is grabbed
#' @returns A dataframe containing fielder statistics: position, reaction/burst/route outfield jump components, sprint speed
#' @import baseballr
#' @export
#' @examples {getFielderStats(671976, 2026)}

getFielderStats <- function(playerid, year) {
  # Get sprint speed
  sprintData <- statcast_leaderboards(leaderboard="sprint_speed", year=year,min_pa=10)
  
  # Get outfielder statistics
  fieldData <- read.csv(paste0("https://baseballsavant.mlb.com/jump?year=",year,"&position=&team=&min=10&csv=true"))

  # Join by playerID and return result. If playerID is -7, -8, or -9, return average stats instead
  if (playerid == -7) { # Average LF
    joinedData <- data.frame(year.x = year, last_name..first_name = "Average, LF", player_id = -7, position = "LF", sprint_speed = 28, rel_league_burst_distance = 0, rel_league_reaction_distance = 0, rel_league_routing_distance = 0)
  } else if (playerid == -8) { # Average CF
    joinedData <- data.frame(year.x = year, last_name..first_name = "Average, CF", player_id = -8, position = "CF", sprint_speed = 28.7, rel_league_burst_distance = 0, rel_league_reaction_distance = 0, rel_league_routing_distance = 0)
  } else if (playerid == -9) { # Average RF
    joinedData <- data.frame(year.x = year, last_name..first_name = "Average, RF", player_id = -9, position = "RF", sprint_speed = 28, rel_league_burst_distance = 0, rel_league_reaction_distance = 0, rel_league_routing_distance = 0)
  } else {
    joinedData <- inner_join(sprintData, fieldData, by = join_by(player_id == resp_fielder_id)) %>%
      dplyr::filter(player_id == playerid) %>%
      select(year.x, last_name..first_name, player_id, position, sprint_speed, rel_league_burst_distance, rel_league_reaction_distance, rel_league_routing_distance)
  }
  
  joinedData$jump <- joinedData$rel_league_burst_distance + joinedData$rel_league_reaction_distance + joinedData$rel_league_routing_distance
  joinedData
}