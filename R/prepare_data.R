#' Reformatting the data for statistical modelling
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
    dplyr::mutate(breeding_site_ID = as.factor(paste0(round(.data$breeding_site_lat, 3),
                                                "_",
                                                round(.data$breeding_site_long, 3)))) |>
    dplyr::select("year", "ring_number", "date",
                  "breeding_site_lat", "breeding_site_long",
                  "breeding_site_ID",
                  "release_site_lat", "release_site_long",
                  "brood_size",
                   "DNSW",
                  "PSW1000", "PSW2000",
                  "trafficvolume500", "trafficvolume1000", "trafficvolume2000",
                  "populationdensity500", "populationdensity1000", "populationdensity2000") |>
    dplyr::mutate(year = as.numeric(.data$year)) |>
    dplyr::arrange(.data$ring_number, .data$date) |>  ## sort by duckID and date
    dplyr::filter(!is.na(.data$ring_number)) -> data_for_model_1 ## remove unknown ducks

  data_for_model_1 |>
    dplyr::filter(dplyr::n() > 1, .by = "ring_number") -> data_for_model_2 ## remove duck only seen once

  data_for_model_2 |>
    dplyr::mutate(date_previous = dplyr::lag(.data$date, n = 1),
           delta_time = as.numeric(.data$date - .data$date_previous), ## compute time between event and next
           delta_year = .data$year - dplyr::lag(.data$year),  ## compute number of calendar year between two events
           delta_season = factor(dplyr::case_when(.data$delta_year < 1 ~ "same breeding season",
                                                .data$delta_year < 2 ~ "one breeding season apart",
                                                TRUE ~ "more than one breeding season apart")), ## categorial, 1 = same year, 2 = 2 or more years
           breeding_site_lat_previous = dplyr::lag(.data$breeding_site_lat), ## retrieve latitude of previous event
           breeding_site_long_previous = dplyr::lag(.data$breeding_site_long),
           lat_relocation_previous = dplyr::lag(.data$release_site_lat),
           long_relocation_previous = dplyr::lag(.data$release_site_long),
           delta_distance = distance2points_vec(lat1 = .data$breeding_site_lat, long1 = .data$breeding_site_long, ## delta distance
                                                lat2 = .data$breeding_site_lat_previous, long2 = .data$breeding_site_long_previous),
           relocation_distance = distance2points_vec(lat1 = .data$breeding_site_lat_previous, long1 = .data$breeding_site_long_previous,
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
           .by = "ring_number") -> data_for_model_3

  data_for_model_3 |> ## remove first occurrence per duck as no past info
    dplyr::slice(-1, .by = "ring_number") -> data_for_model_4

  data_for_model_4 |>
    dplyr::filter(.data$delta_season != "more than one breeding season apart") -> data_for_model_5 ## remove recapture event too far apart in time

  data_for_model_5 |>
    tidyr::drop_na("ring_number", "delta_time",
            "delta_distance", "relocation_distance",
            "breeding_site_lat_previous", "breeding_site_long_previous",
            "breeding_site_long", "breeding_site_lat", "breeding_site_ID",
            "brood_size", "brood_size_previous",
            "DNSW", "DNSW_previous",
            tidyselect::starts_with("presence"),  tidyselect::starts_with("traffic"), tidyselect::starts_with("populationdensity")) -> data_for_model_6 ## remove rows with NAs

  data_for_model_6 |>
    dplyr::mutate(stay = .data$delta_distance < 20,
                  PSW1000_previous_f = as.factor(.data$PSW1000_previous),
                  PSW2000_previous_f = as.factor(.data$PSW2000_previous)) |>   ## if distance is under 20m, we consider ducks to go back to same place
    # mutate(across(c(brood_size_previous, DNSW_previous,
    #                 trafficvolume500_previous, populationdensity500_previous,
    #                 trafficvolume1000_previous, populationdensity1000_previous,
    #                 trafficvolume2000_previous, populationdensity2000_previous
    #                 ), .fns = \(x) scale(x)[, 1], .names = "{.col}_z")) |>  ## TODO: remove scaling
    as.data.frame() -> data_for_model

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
                               length(unique(stats::na.omit(rawdata$ring_number))) - length(unique(stats::na.omit(data_for_model_1$ring_number))),
                               length(unique(stats::na.omit(data_for_model_1$ring_number))) - length(unique(stats::na.omit(data_for_model_2$ring_number))),
                               length(unique(stats::na.omit(data_for_model_2$ring_number))) - length(unique(stats::na.omit(data_for_model_3$ring_number))),
                               length(unique(stats::na.omit(data_for_model_3$ring_number))) - length(unique(stats::na.omit(data_for_model_4$ring_number))),
                               length(unique(stats::na.omit(data_for_model_4$ring_number))) - length(unique(stats::na.omit(data_for_model_5$ring_number))),
                               length(unique(stats::na.omit(data_for_model_5$ring_number))) - length(unique(stats::na.omit(data_for_model_6$ring_number))),
                               length(unique(stats::na.omit(data_for_model_6$ring_number))) - length(unique(stats::na.omit(data_for_model$ring_number)))),
             remaining_ducks = c(length(unique(stats::na.omit(rawdata$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_1$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_2$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_3$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_4$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_5$ring_number))),
                                 length(unique(stats::na.omit(data_for_model_6$ring_number))),
                                 length(unique(stats::na.omit(data_for_model$ring_number)))),
            delta_duck_pct = 100 * (.data$ducks_removed / length(unique(stats::na.omit(rawdata$ring_number))))
         )

  cat("Information on data filtering\n")
  print(res)
  cat("\n")

  tibble::as_tibble(droplevels(data_for_model))
}


globalVariables(".data")
