#' Functions to compute the distance between geographic locations
#'
#' @inheritParams arguments
#' @name distance2points
#' @aliases distance2points distance2points_vec
#'
#' @examples
#' # Compute the distance between Paris and Berlin
#' distance2points(lat1 = 48.856667, long1 = 2.352222, lat2 = 52.52, long2 = 13.405)
#'
#' # Compute the distance between Paris and Berlin, and between Berlin and Paris
#' distance2points_vec(lat1 = c(48.856667, 52.52),
#'                     long1 = c(2.352222, 13.405),
#'                     lat2 = c(52.52, 48.856667),
#'                     long2 = c(13.405, 2.352222))
NULL

#' @describeIn distance2points simple version to compute distances between two geographic locations
#' @return `distance2points()` returns an object of class `units` providing the distance between geographic locations
#' @export
distance2points <- function(lat1, long1, lat2, long2) {
  location1 <- sf::st_point(c(long1, lat1))
  location2 <- sf::st_point(c(long2, lat2))
  locations <- sf::st_sfc(location1, location2)
  sf::st_crs(locations) <- 4326 # = WGS 84
  sf::st_distance(locations, which = "Great Circle")[1, 2]
}

#' @describeIn distance2points vectorised version to compute distances between two sets of coordinates
#' @return `distance2points_vec()` returns a vector of objects of class `units` providing the distance between geographic locations
#' @export
distance2points_vec <- Vectorize(distance2points) ## vectorised version of the function (not efficient but works)


#' Compute the number of elements in a vector after discarding NAs
#'
#' @inheritParams arguments
#'
#' @return the number of elements
#' @export
#'
#' @examples
#' howmany(c(1, 3, NA))
#' howmany(c("1", "3", NA))
#'
howmany <- function(x) {
  length(unique(x[!is.na(x)]))
  }
