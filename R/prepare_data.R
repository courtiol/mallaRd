#' Filtering and reformatting the data for statistical modeling
#'
#' This functions perform a series of data filtering steps. It also express predictors as
#' differences between a capture event and the previous capture. Check the body of the function to
#' see all steps in details.
#'
#' @inheritParams arguments
#' @return a dataframe with the data ready for modeling
#' @export
#'
#' @examples
#' #see ?mallaRd
#'
prepare_data <- function(rawdata) {

  rawdata |>
    dplyr::mutate(individual_ID = .data$ring_number) |>
    dplyr::select("year", "individual_ID", "date",
                  "habitat_type",
                  "location_lat", "location_long",
                  "location_ID",
                  "release_site_lat", "release_site_long",
                  "brood_size",
                   "DNSW",
                  "PSW1000", "PSW2000",
                  "trafficvolume500", "trafficvolume1000", "trafficvolume2000",
                  "populationdensity500", "populationdensity1000", "populationdensity2000") |>
    dplyr::mutate(year = as.numeric(.data$year)) |>
    dplyr::arrange(.data$individual_ID, .data$date) |>  ## sort by duckID and date
    dplyr::filter(!is.na(.data$individual_ID)) -> data_for_model_1 ## remove unknown ducks

  data_for_model_1 |>
    dplyr::filter(dplyr::n() > 1, .by = "individual_ID") -> data_for_model_2 ## remove duck only seen once

  data_for_model_2 |>
    dplyr::mutate(
           habitat_type_previous = dplyr::lag(.data$habitat_type, n = 1),
           location_ID_previous = dplyr::lag(.data$location_ID, n = 1),
           date_previous = dplyr::lag(.data$date, n = 1),
           delta_time = as.numeric(.data$date - .data$date_previous), ## compute time between event and next
           delta_year = .data$year - dplyr::lag(.data$year),  ## compute number of calendar year between two events
           delta_season = factor(dplyr::case_when(.data$delta_year < 1 ~ "same breeding season",
                                                .data$delta_year < 2 ~ "one breeding season apart",
                                                TRUE ~ "more than one breeding season apart")), ## categorial, 1 = same year, 2 = 2 or more years
           location_lat_previous = dplyr::lag(.data$location_lat), ## retrieve latitude of previous event
           location_long_previous = dplyr::lag(.data$location_long),
           lat_relocation_previous = dplyr::lag(.data$release_site_lat),
           long_relocation_previous = dplyr::lag(.data$release_site_long),
           delta_distance = distance2points_vec(lat1 = .data$location_lat, long1 = .data$location_long, ## delta distance
                                                lat2 = .data$location_lat_previous, long2 = .data$location_long_previous),
           relocation_distance = distance2points_vec(lat1 = .data$location_lat_previous, long1 = .data$location_long_previous,
                                                     lat2 = .data$lat_relocation_previous, long2 = .data$long_relocation_previous),
           relocation_distance_factor = factor(dplyr::case_when(.data$relocation_distance < 1000 ~ "less than 1K",
                                                                .data$relocation_distance >= 1000 & .data$relocation_distance < 2000 ~ "1 - 2K",
                                                         TRUE ~ ">= 2K")), ## categorial
           brood_size_previous = dplyr::lag(.data$brood_size),
           DNSW_previous = dplyr::lag(.data$DNSW),
           PSW1000_previous = dplyr::lag(.data$PSW1000),
           PSW2000_previous = dplyr::lag(.data$PSW2000),
           trafficvolume500_previous = dplyr::lag(.data$trafficvolume500),
           trafficvolume1000_previous = dplyr::lag(.data$trafficvolume1000),
           trafficvolume2000_previous = dplyr::lag(.data$trafficvolume2000),
           populationdensity500_previous = dplyr::lag(.data$populationdensity500),
           populationdensity1000_previous = dplyr::lag(.data$populationdensity1000),
           populationdensity2000_previous = dplyr::lag(.data$populationdensity2000),
           .by = "individual_ID") -> data_for_model_3

  data_for_model_3 |> ## remove first occurrence per duck as no past info
    dplyr::slice(-1, .by = "individual_ID") -> data_for_model_4

  data_for_model_4 |>
    dplyr::filter(.data$delta_season != "more than one breeding season apart") -> data_for_model_5 ## remove recapture event too far apart in time

  data_for_model_5 |>
    tidyr::drop_na("individual_ID", "habitat_type_previous", "delta_season",
            "delta_distance", "relocation_distance",
            "location_lat_previous", "location_long_previous",
            "location_long", "location_lat", "location_ID",
            "brood_size", "brood_size_previous",
            "DNSW", "DNSW_previous",
            tidyselect::starts_with("presence"),  tidyselect::starts_with("traffic"), tidyselect::starts_with("populationdensity")) -> data_for_model_6 ## remove rows with NAs

  data_for_model_6 |>
    dplyr::mutate(return = .data$location_ID == .data$location_ID_previous,
                  PSW1000_previous_f = as.factor(.data$PSW1000_previous),
                  PSW2000_previous_f = as.factor(.data$PSW2000_previous)) |>   ## if distance is under 20m, we consider ducks to go back to same place
    dplyr::mutate(dplyr::across(c("brood_size_previous",
                                  "relocation_distance",
                                  "DNSW_previous",
                                  "trafficvolume500_previous", "trafficvolume1000_previous", "trafficvolume2000_previous",
                                  "populationdensity500_previous", "populationdensity1000_previous", "populationdensity2000_previous"),
                                .fns = \(x) {zz <- scale(x)[, 1]
                                             attr(zz, "mean") <- mean(x)
                                             attr(zz, "sd") <- stats::sd(x)
                                             zz},
                                .names = "{.col}_z")) |>
    dplyr::select("individual_ID",
                  "date", "return",
                  "habitat_type", "habitat_type_previous",
                  "location_lat", "location_long",
                  "location_lat_previous", "location_long_previous",
                  "location_ID", "location_ID_previous",
                  "delta_season",
                  "brood_size_previous", "brood_size_previous_z",
                  "relocation_distance", "relocation_distance_z",
                  "DNSW_previous", "DNSW_previous_z",
                  "PSW1000_previous", "PSW2000_previous",
                  "trafficvolume500_previous", "trafficvolume500_previous_z",
                  "trafficvolume1000_previous", "trafficvolume1000_previous_z",
                  "trafficvolume2000_previous", "trafficvolume2000_previous_z",
                  "populationdensity500_previous", "populationdensity500_previous_z",
                  "populationdensity1000_previous", "populationdensity1000_previous_z",
                  "populationdensity2000_previous", "populationdensity2000_previous_z") -> data_for_model

  # describe discarded data
  res <- tibble::tibble(description = c("raw data",
                             "removed because unknown ducks",
                             "removed because only observed once",
                             "removed because of reshaping the data",
                             "removed because first occurrence per duck not used",
                             "removed because recapture events more than 1 year apart",
                             "removed because of missing values for predictors",
                             "final number of rows"),
             breeding_events_removed = c(0,  ## initial number of rows
                         nrow(rawdata) - nrow(data_for_model_1), ## number of rows removed because they correspond to unknown ducks
                         nrow(data_for_model_1) - nrow(data_for_model_2), ## number of rows removed because they correspond to ducks only observed once
                         nrow(data_for_model_2) - nrow(data_for_model_3), ## number of rows removed because of reshaping the data
                         nrow(data_for_model_3) - nrow(data_for_model_4), ## number of rows removed because they correspond to first occurrence per duck that is recapture
                         nrow(data_for_model_4) - nrow(data_for_model_5), ## number of rows removed because they correspond to recapture events more than 1 year apart
                         nrow(data_for_model_5) - nrow(data_for_model_6), ## number of rows removed because of missing values for predictors
                         nrow(data_for_model) ## final number of rows
                      ),
             remaining_events = c(nrow(rawdata),
                                  nrow(data_for_model_1),
                                  nrow(data_for_model_2),
                                  nrow(data_for_model_3),
                                  nrow(data_for_model_4),
                                  nrow(data_for_model_5),
                                  nrow(data_for_model_6),
                                  nrow(data_for_model)),
             delta_events_pct = 100 * (.data$breeding_events_removed / nrow(rawdata)),
             ducks_removed = c(0,
                               length(unique(stats::na.omit(rawdata$ring_number))) - length(unique(stats::na.omit(data_for_model_1$individual_ID))),
                               length(unique(stats::na.omit(data_for_model_1$individual_ID))) - length(unique(stats::na.omit(data_for_model_2$individual_ID))),
                               length(unique(stats::na.omit(data_for_model_2$individual_ID))) - length(unique(stats::na.omit(data_for_model_3$individual_ID))),
                               length(unique(stats::na.omit(data_for_model_3$individual_ID))) - length(unique(stats::na.omit(data_for_model_4$individual_ID))),
                               length(unique(stats::na.omit(data_for_model_4$individual_ID))) - length(unique(stats::na.omit(data_for_model_5$individual_ID))),
                               length(unique(stats::na.omit(data_for_model_5$individual_ID))) - length(unique(stats::na.omit(data_for_model_6$individual_ID))),
                               length(unique(stats::na.omit(data_for_model_6$individual_ID))) - length(unique(stats::na.omit(data_for_model$individual_ID)))),
             remaining_ducks = c(length(unique(stats::na.omit(rawdata$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_1$individual_ID))),
                                 length(unique(stats::na.omit(data_for_model_2$individual_ID))),
                                 length(unique(stats::na.omit(data_for_model_3$individual_ID))),
                                 length(unique(stats::na.omit(data_for_model_4$individual_ID))),
                                 length(unique(stats::na.omit(data_for_model_5$individual_ID))),
                                 length(unique(stats::na.omit(data_for_model_6$individual_ID))),
                                 length(unique(stats::na.omit(data_for_model$individual_ID)))),
            delta_duck_pct = 100 * (.data$ducks_removed / length(unique(stats::na.omit(rawdata$ring_number))))
         )

  cat("Information on data filtering\n")
  print(res)
  cat("\n")

  data_for_model
}


globalVariables(".data")
