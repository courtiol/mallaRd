#' mallaRd: Breeding output and breeding site fidelity of mallards in Berlin, Germany
#'
#' This help page reproduces, in the section **Examples** the analyses and results from the paper
#' "Housing search in the concrete jungle – breeding site selection of urban mallards and
#' implications for conservation management strategies" by Engler et al.
#'
#' All three datasets used in the workflow below are readily available after loading this package
#' (`data_raw`, `data_all` and `data_model`) but the workflow below shows how we created such
#' datasets starting from the raw data file stored in the subfolder extdata of this package.
#'
#' Note that the workflow relies on functions from other packages (e.g. \pkg{dplyr}), but we reexported
#' these functions so you don't have to load these packages as long as **mallaRd** is loaded. If
#' you want to expand upon the proposed workflow and require more functions, you will however have
#' to load such packages explicitly.
#'
#' @name mallaRd-package
#' @aliases mallaRd-package mallaRd
#' @keywords package
#'
#' @references reference will be added once the paper is published
#'
#' @examples
#'
#' # Methods ---------------------------------------------------------------------------------------
#'
#' ## Loading the raw data
#'
#' data_raw <- read.csv(system.file("extdata/raw_data.csv", package = "mallaRd"))
#' head(data_raw)
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_raw, overwrite = TRUE)
#'
#'
#' ## Formatting the data
#'
#' data_raw |>
#'   as_tibble() |>
#'   mutate(date = as.Date(.data$date, format = c("%d.%m.%Y")),
#'          hatch_date = as.Date(.data$hatch_date, format = c("%d.%m.%Y")),
#'          jan_1st = as.Date(paste0(.data$year, "-01-01")),
#'          hatch_doy = as.numeric(.data$hatch_date - as.Date(.data$jan_1st)),
#'          ring_number = ifelse(.data$ring_number == "", NA, .data$ring_number)) |>
#'   mutate(habitat_type_balcony = .data$habitat_type == "balcony",
#'          habitat_type_courtyard = .data$habitat_type == "courtyard",
#'          habitat_type_other = .data$habitat_type == "other",
#'          habitat_type_roof_terrace = .data$habitat_type == "roof_terrace",
#'          habitat_type_unknown = .data$habitat_type == "unknown") |>
#'   mutate(across(c("PSW1000", "PSW2000"), \(x) factor(ifelse(x == 0, "no", "yes")))) |>
#'   mutate(across(where(is.character), as.factor)) -> data_all
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_all, overwrite = TRUE)
#'
#' ## Summary statistics for the full dataset
#'
#' ### total number of breeding events
#' nrow(data_all)
#' # [1] 1732
#'
#' ### breeding events from identified (ringed)  ducks
#' sum(!is.na(data_all$ring_number))
#' # [1] 1211
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all$ring_number)
#' # [1] 810
#'
#' ### total number of years
#' howmany(data_all$year)
#' # [1] 16
#'
#' ### range of years
#' range(data_all$year)
#' # [1] 2005 2020
#'
#' ### breeding events for which floor level known
#' sum(!is.na(data_all$floor_level))
#' # [1] 100
#'
#' ### pct of breeding events for which floor level known
#' round(100*(sum(!is.na(data_all$floor_level))/nrow(data_all)))
#' # [1] 6
#'
#'
#' ## Reformatting the data for statistical modelling retaining only observations for which all
#' ## predictors used for modelling are available
#'
#' data_model <- prepare_data(data_all)
#' # Information on data filtering
#' # # A tibble: 8 × 7
#' #   description                                             breeding_events_removed remaining_events delta_events_pct ducks_removed remaining_ducks delta_duck_pct
#' #   <chr>                                                                     <dbl>            <int>            <dbl>         <dbl>           <int>          <dbl>
#' # 1 raw data                                                                      0             1732            0                 0             810          0
#' # 2 removed because unknown ducks                                               521             1211           30.1               0             810          0
#' # 3 removed because only observed once                                          582              629           33.6             582             228         71.9
#' # 4 removed because of reshaping the data                                         0              629            0                 0             228          0
#' # 5 removed because first occurrence per duck not used                          228              401           13.2               0             228          0
#' # 6 removed because recapture events more than 1 year apart                      91              310            5.25             42             186          5.19
#' # 7 removed because of missing values for predictors                             10              300            0.577             6             180          0.741
#' # 8 final number of rows                                                        300              300           17.3               0             180          0
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_model, overwrite = TRUE)
#'
#'
#' ### total number of breeding events (nrow + 1/duck)
#' nrow(data_model) + howmany(data_model$individual_ID)
#' # [1] 480
#'
#' ### total number of recapture events
#' nrow(data_model)
#' # [1] 300
#'
#' ### total number of ducks complete dataset
#' howmany(data_model$individual_ID)
#' # [1] 180
#'
#' ### correlation between quantitative predictors of different type
#' cor_matrix <- cor(data_model[,
#'   c("brood_size_previous_z",
#'     "relocation_distance_z",
#'     "trafficvolume500_previous_z", "trafficvolume1000_previous_z", "trafficvolume2000_previous_z",
#'     "populationdensity500_previous_z","populationdensity1000_previous_z", "populationdensity2000_previous_z")])
#' diag(cor_matrix) <- NA
#' cor_matrix[c("trafficvolume500_previous_z", "trafficvolume1000_previous_z", "trafficvolume2000_previous_z"),
#'            c("trafficvolume500_previous_z", "trafficvolume1000_previous_z", "trafficvolume2000_previous_z")] <- NA
#' cor_matrix[c("populationdensity500_previous_z","populationdensity1000_previous_z", "populationdensity2000_previous_z"),
#'            c("populationdensity500_previous_z","populationdensity1000_previous_z", "populationdensity2000_previous_z")] <- NA
#' range(cor_matrix, na.rm = TRUE)
#' # [1] -0.10731  0.65026
#'
#'
#' ## Prepare formulas for model fits
#'
#' formula_df <- prepare_formulas()
#'
#' ### number of different models to fit
#' nrow(formula_df)
#' # [1] 4096
#'
#'
#' # Results on breeding output --------------------------------------------------------------------
#'
#' ## descriptive statistics
#'
#' ### sample size for clutch size
#' sum(!is.na(data_all$clutch_size)) # breeding events
#' # [1] 1273
#' howmany(data_all$ring_number[!is.na(data_all$clutch_size)]) # ducks
#' # [1] 714
#'
#' ### mean clutch size
#' mean(data_all$clutch_size, na.rm = TRUE)
#' # [1] 8.666143
#'
#' ### range clutch size
#' range(data_all$clutch_size, na.rm = TRUE)
#' # [1] 1 17
#'
#' ### sample size for brood size
#' sum(!is.na(data_all$brood_size)) # breeding events
#' # [1] 1719
#' howmany(data_all$ring_number[!is.na(data_all$brood_size)]) # ducks
#' # [1] 805
#'
#' ### mean brood size
#' mean(data_all$brood_size, na.rm = TRUE)
#' # [1] 7.028505
#'
#' ### range brood size
#' range(data_all$brood_size, na.rm = TRUE)
#' # [1] 1 15
#'
#' ### sample size for body mass
#' sum(!is.na(data_all$body_mass_g)) # breeding events
#' # [1] 1171
#' howmany(data_all$ring_number[!is.na(data_all$body_mass_g)]) # ducks
#' # [1] 767
#'
#' ### mean body mass
#' mean(data_all$body_mass_g, na.rm = TRUE)
#' # [1] 817.0171
#'
#' ### range body mass
#' range(data_all$body_mass_g, na.rm = TRUE)
#' # [1]  520 1030
#'
#' ### sample size for wing length
#' sum(!is.na(data_all$wing_length_mm)) # breeding events
#' # [1] 1190
#' howmany(data_all$ring_number[!is.na(data_all$wing_length_mm)]) # ducks
#' # [1] 783
#'
#' ### mean wing length
#' mean(data_all$wing_length_mm, na.rm = TRUE)
#' # [1] 264.458
#'
#' ### range wing length
#' range(data_all$wing_length_mm, na.rm = TRUE)
#' # [1] 226 287
#'
#' ### sample size for hatching day of the year
#' sum(!is.na(data_all$hatch_doy)) # breeding events
#' # [1] 1457
#' howmany(data_all$ring_number[!is.na(data_all$hatch_doy)]) # ducks
#' # [1] 774
#'
#' ### mean hatching day of the year
#' mean(data_all$hatch_doy, na.rm = TRUE)
#' # [1] 146.5573
#'
#' ### range hatching day of the year
#' range(data_all$hatch_doy, na.rm = TRUE)
#' # [1] 61 207
#'
#'
#' # Results on site selection ---------------------------------------------------------------------
#'
#' ## descriptive statistics
#'
#' ### sample size for habitat type predictor
#' sum(!is.na(data_all$habitat_type)) # breeding events
#' # [1] 1732
#' howmany(data_all$ring_number[!is.na(data_all$habitat_type)]) # ducks
#' # [1] 810
#'
#' ### sample size for habitat type roof terraces
#' sum(data_all$habitat_type_roof_terrace) # breeding events
#' # [1] 485
#' howmany(data_all$ring_number[data_all$habitat_type_roof_terrace]) # ducks
#' # [1] 277
#'
#' ### pct habitat type roof terraces
#' 100*mean(data_all$habitat_type_roof_terrace, na.rm = TRUE)
#' # [1] 28.00231
#'
#' ### sample size for habitat type balconies
#' sum(data_all$habitat_type_balcony) # breeding events
#' # [1] 509
#' howmany(data_all$ring_number[data_all$habitat_type_balcony]) # ducks
#' # [1] 319
#'
#' ### pct habitat type roof balconies
#' 100*mean(data_all$habitat_type_balcony, na.rm = TRUE)
#' # [1] 29.38799
#'
#' ### sample size for habitat type courtyards
#' sum(data_all$habitat_type_courtyard) # breeding events
#' # [1] 437
#' howmany(data_all$ring_number[data_all$habitat_type_courtyard]) # ducks
#' # [1] 243
#'
#' ### pct habitat type roof courtyards
#' 100*mean(data_all$habitat_type_courtyard, na.rm = TRUE)
#' # [1] 25.23095
#'
#' ### sample size for habitat type other
#' sum(data_all$habitat_type_other) # breeding events
#' # [1] 77
#' howmany(data_all$ring_number[data_all$habitat_type_other]) # ducks
#' # [1] 41
#'
#' ### pct habitat type other
#' 100*mean(data_all$habitat_type_other, na.rm = TRUE)
#' # [1] 4.445727
#'
#' ### sample size for habitat type unknown
#' sum(data_all$habitat_type_unknown) # breeding events
#' # [1] 224
#' howmany(data_all$ring_number[data_all$habitat_type_unknown]) # ducks
#' # [1] 34
#'
#' ### pct habitat type unknown
#' 100*mean(data_all$habitat_type_unknown, na.rm = TRUE)
#' # [1] 12.93303
#'
#' # Results on breeding site fidelity -------------------------------------------------------------
#'
#'
#'
"_PACKAGE"
