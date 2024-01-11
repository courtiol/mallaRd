#' Compute the distance between two geographic locations
#'
#' @inheritParams arguments
#'
#' @return an object of class `units` providing the distance between two geographic locations.
#' @export
#'
#' @examples
#' # Compute the distance between Paris and Berlin
#' distance2points(lat1 = 48.856667, long1 = 2.352222, lat2 = 52.52, long2 = 13.405)
#'
distance2points <- function(lat1, long1, lat2, long2) {
  location1 <- sf::st_point(c(long1, lat1))
  location2 <- sf::st_point(c(long2, lat2))
  locations <- sf::st_sfc(location1, location2)
  sf::st_crs(locations) <- 4326 # = WGS 84
  sf::st_distance(locations, which = "Great Circle")[1, 2]
}
