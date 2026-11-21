#' MLB Approximated Park Dimensions Data
#'
#' Paths that construct fair territory for all MLB ballparks used as primary home parks between the years of 2020 and 2026.
#' Approximated using Andrew Clem's pixel art for each stadium, translated such that the back of home plate is (0,0), and scaled using the official left-field measurement.
#'
#' @format ## `who`
#' A data frame with 37 rows and 6 columns:
#' \describe{
#'   \item{id}{Unique identifier for park.}
#'   \item{name}{Name of the stadium.}
#'   \item{team}{Three-letter code of primary occupant of stadium.}
#'   \item{yearMin, yearMax}{Bounds for which the specific outfield configuration specified in 'path' was active.}
#'   \item{path}{A list of (x,y) coordinates that depict fair territory. The back of home plate is (0,0).}
#'   ...
#' }
#' @source <http://www.andrewclem.com/Baseball/index.html>