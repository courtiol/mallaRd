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
#' table_predictors(data_all_known)
#'
table_predictors <- function(data) {

  data$habitat_type  <- NULL
  data$location_ID    <- NULL
  data$location_long  <- NULL
  data$location_lat   <- NULL
  data$relocation_distance <- NULL
  data$relocation_distance_z <- NULL
  colnames(data) <- gsub("_previous", "", colnames(data))

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
                                     "relocation_distance"),
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
