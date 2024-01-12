#' Prepare the formulas for the models to be fitted
#'
#' @return a dataframe with the formulas of all models to be fitted
#' @export
#'
#' @examples
#' prepare_formula()
#'
prepare_formula <- function() {

  base_formula <- "return ~ "

  add_formula <- expand.grid(habitat_type = c("habitat_type_previous", ""),
                             habitat_structure = c("DNSW_previous_z",
                                                   "PSW1000_previous",
                                                   "PSW2000_previous",
                                                   ""),
                             traffic_volume = c("trafficvolume500_previous_z",
                                                "trafficvolume1000_previous_z",
                                                "trafficvolume2000_previous_z",
                                                ""),
                             population_density = c("populationdensity500_previous_z",
                                                    "populationdensity1000_previous_z",
                                                    "populationdensity2000_previous_z",
                                                    ""),
                             brood_size = c("brood_size_z", ""),
                             breeding_season = c("delta_season", ""),
                             relocation_distance = c("relocation_distance_z", ""),
                             random = c("(1 | individual_ID)",
                                        "(1 | location_ID)",
                                        "(1 | individual_ID) + (1 | location_ID)",
                                        ""),
                             intercept = "1") ## important for processed formulas not ending with "+"

  all_formulas <- apply(cbind(base_formula, add_formula), 1, paste, collapse = " + ")

  all_formulas |>
    gsub(pattern = "\\s", replacement = "", x = _) |>
    gsub(pattern = "\\+{2,}", replacement = "+", x = _) |>
    gsub(pattern = "\\~\\+", replacement = "~", x = _) -> all_formulas

  data.frame(formula = all_formulas) |>
    dplyr::rowwise() |>
    dplyr::mutate(habitat_type = grepl("type", x = .data$formula),
                  habitat_structure = grepl("W", x = .data$formula),
                  traffic_volume = grepl("traffic", x = .data$formula),
                  population_density = grepl("density", x = .data$formula),
                  brood_size = grepl("brood", x = .data$formula),
                  breeding_season = grepl("season", x = .data$formula),
                  relocation_distance = grepl("relocation", x = .data$formula)) |>
    dplyr::mutate(nb_predictors_fix = sum(dplyr::c_across(-"formula"))) |>
    dplyr::mutate(individual_ID = grepl("individual_ID", x = .data$formula),
                  location_ID = grepl("location_ID", x = .data$formula)) |>
    dplyr::mutate(nb_predictors_rand = sum(dplyr::c_across("individual_ID":"location_ID")),
                  nb_predictors = .data$nb_predictors_fix + .data$nb_predictors_rand) |>
    dplyr::ungroup() -> all_formulas_df

  #all_formulas_df <- dplyr::cross_join(all_formulas_df, data.frame(link = c("logit", "probit", "cauchit")))

  all_formulas_df
}
