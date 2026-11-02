#' getCompetitiveArea.R
#' 
#' Finds the competitive area for a batted ball
#' @title "Finds the competitive area for a batted ball."
#' @param startingX X coordinate for the original batted ball location
#' @param startingY Y coordinate for the original batted ball location
#' @param catchRadius The radius of the competitive area
#' @returns A list of points within the circle, down to a half-foot
#' @export
#' @examples {getCompetitiveArea(-129,188,58.8)}

getCompetitiveArea <- function(startingX, startingY, catchRadius) {

  ## Get extrema and initalize pointList
  x_max <- round(4*(startingX + catchRadius))/4
  x_min <- round(4*(startingX - catchRadius))/4
  y_max <- round(4*(startingY + catchRadius))/4
  y_min <- round(4*(startingY - catchRadius))/4
  pointList <- data.frame(x=NULL,y=NULL)
  
  ## Sweep across x and y, ensuring that points are within circle
  sqRegion <- expand.grid(x = seq(x_min,x_max,.25), y = seq(y_min,y_max,.25))
  pointList <- sqRegion[(startingX - sqRegion$x)^2 + (startingY - sqRegion$y)^2 <= catchRadius^2,]
  
  ## Return pointList
  pointList
}