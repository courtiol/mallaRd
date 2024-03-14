#' Create table with summary statistics about the predictors
#'
#' @inheritParams arguments
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' # example code
#' table_predictors(data_model)
#'
table_predictors <- function(data) {

  if (any(colnames(data) %in% "PSW1000")) {
    ## if not data_model but data_all
    data |>
     dplyr::mutate(delta_year = .data$year - dplyr::lag(.data$year),  ## compute number of calendar year between two events
                   delta_season = factor(dplyr::case_when(.data$delta_year < 1 ~ "same breeding season",
                                                          .data$delta_year < 2 ~ "one breeding season apart",
                                                          TRUE ~ "more than one breeding season apart")),
                   location_lat_previous = dplyr::lag(.data$location_lat), ## retrieve latitude of previous event
                   location_long_previous = dplyr::lag(.data$location_long),
                   lat_relocation_previous = dplyr::lag(.data$release_site_lat),
                   long_relocation_previous = dplyr::lag(.data$release_site_long),
                   relocation_distance = distance2points_vec(lat1 = .data$location_lat, long1 = .data$location_long, ## delta distance
                                                             lat2 = .data$location_lat_previous, long2 = .data$location_long_previous),
                   relocation_distance_previous = distance2points_vec(lat1 = .data$location_lat_previous, long1 = .data$location_long_previous,
                                                                      lat2 = .data$lat_relocation_previous, long2 = .data$long_relocation_previous),
                   .by = "ring_number") -> data
  } else {
    data$habitat_type   <- NULL
    data$location_ID    <- NULL
    data$location_long  <- NULL
    data$location_lat   <- NULL
    colnames(data) <- gsub("_previous", "", colnames(data))
  }

  data |>
    dplyr::mutate(habitat_balcony = as.numeric(.data$habitat_type == "balcony"),
                  habitat_courtyard = as.numeric(.data$habitat_type == "courtyard"),
                  habitat_other = as.numeric(.data$habitat_type == "other"),
                  habitat_roof_terrace = as.numeric(.data$habitat_type == "roof_terrace"),
                  habitat_unknown = as.numeric(.data$habitat_type == "unknown"),
                  PSW1000 = as.numeric(.data$PSW1000),
                  delta_season_same = as.numeric(.data$delta_season == "same breeding season"),
                  delta_season_successive = as.numeric(.data$delta_season == "one breeding season apart"),
                  delta_season_distant = as.numeric(.data$delta_season == "more than one breeding season apart")
                  ) |>
    dplyr::mutate(dplyr::across(dplyr::contains("_z"), \(x) x*attr(x, "sd") + attr(x, "mean"))) |>
    dplyr::summarise(dplyr::across(c("habitat_balcony",
                                     "habitat_courtyard",
                                     "habitat_other",
                                     "habitat_roof_terrace",
                                     "habitat_unknown",
                                     "brood_size",
                                     "PSW1000",
                                     "trafficvolume2000",
                                     "populationdensity500",
                                     "delta_season_same",
                                     "delta_season_successive",
                                     "delta_season_distant",
                                     "relocation_distance_previous"),
                                  \(x) data.frame(min = min(x, na.rm = TRUE),
                                                  max = max(x, na.rm = TRUE),
                                                  median = stats::median(x, na.rm = TRUE),
                                                  mean = mean(x, na.rm = TRUE),
                                                  sd = stats::sd(x, na.rm = TRUE),
                                                  n = length(x[!is.na(x)])),
                                  .unpack = "{outer}.{inner}")) |>
    tidyr::pivot_longer(cols = dplyr::everything()) |>
    tidyr::separate(.data$name, sep = "\\.", into = c("predictor", "stats")) |>
    tidyr::pivot_wider(names_from = .data$stats, values_from = .data$value)
  }
