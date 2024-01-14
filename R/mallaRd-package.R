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
#' # Preliminary steps -----------------------------------------------------------------------------
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
#'          habitat_type_unknown = .data$habitat_type == "unknown",
#'          location_ID = as.factor(location_ID)) |>
#'   mutate(across(c("PSW1000", "PSW2000"), \(x) factor(ifelse(x == 0, "no", "yes")))) |>
#'   mutate(across(where(is.character), as.factor)) -> data_all
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_all, overwrite = TRUE)
#'
#'
#' # Introduction-----------------------------------------------------------------------------------
#'
#' ## Summary statistics for the full dataset
#'
#' ### breeding events from identified (ringed) ducks
#' sum(!is.na(data_all$ring_number))
#' # [1] 1199
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all$ring_number)
#' # [1] 795
#'
#' ### total number of years
#' howmany(data_all$year)
#' # [1] 16
#'
#' ### range of years
#' range(data_all$year)
#' # [1] 2005 2020
#'
#'
#' # Methods: 2.1 Study background and data collection ---------------------------------------------
#'
#' ## Descriptive statistics for the full dataset
#'
#' ### total number of breeding events
#' nrow(data_all)
#' # [1] 1634
#'
#' ### breeding events from identified (ringed) ducks
#' sum(!is.na(data_all$ring_number))
#' # [1] 1199
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all$ring_number)
#' # [1] 795
#'
#' ### pct of breeding events for which floor level known
#' round(100*(sum(!is.na(data_all$floor_level))/nrow(data_all)))
#' # [1] 6
#'
#' ### breeding events for which floor level known
#' sum(!is.na(data_all$floor_level))
#' # [1] 100
#'
#'
#' # Methods: 2.2 Breeding site selection and site fidelity ----------------------------------------
#'
#' ## Reformatting the data for statistical modelling retaining only observations for which all
#' ## predictors used for modelling are available
#'
#' data_model <- prepare_data(data_all)
#' # Information on data filtering
#' # # A tibble: 8 × 7
#' #   description                                             breeding_events_removed remaining_events delta_events_pct ducks_removed remaining_ducks delta_duck_pct
#' #   <chr>                                                                     <dbl>            <int>            <dbl>         <dbl>           <int>          <dbl>
#' # 1 raw data                                                                      0             1634            0                 0             795          0
#' # 2 removed because unknown ducks                                               435             1199           26.6               0             795          0
#' # 3 removed because only observed once                                          567              632           34.7             567             228         71.3
#' # 4 removed because of reshaping the data                                         0              632            0                 0             228          0
#' # 5 removed because first occurrence per duck not used                          228              404           14.0               0             228          0
#' # 6 removed because recapture events more than 1 year apart                      91              313            5.57             42             186          5.28
#' # 7 removed because of missing values for predictors                             12              301            0.734             6             180          0.755
#' # 8 final number of rows                                                        301              301           18.4               0             180          0
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_model, overwrite = TRUE)
#'
#'
#' ## Descriptive statistics for the dataset used fo modelling
#'
#' ### many statistics are directly provided by the table above displayed while calling `prepare_data() `
#'
#' ### total number of breeding events (nrow + 1/duck)
#' nrow(data_model) + howmany(data_model$individual_ID)
#' # [1] 481
#'
#' ### total number of recapture events
#' nrow(data_model)
#' # [1] 301
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
#' # [1] -0.1119443  0.6505292
#'
#'
#' # Methods: 2.3 Statistical analysis -------------------------------------------------------------
#'
#' ## Prepare formulas for model fits
#'
#' formula_df <- prepare_formulas()
#'
#' ## number of different models to fit
#' nrow(formula_df)
#' # [1] 4096
#'
#' packageVersion("spaMM")
#' # [1] ‘4.4.16’
#'
#' packageVersion("DHARMa")
#' # [1] ‘0.4.6’
#'
#' packageVersion("base")
#' # [1] ‘4.3.1’
#'
#'
#' # Results: 3.1 Breeding output ------------------------------------------------------------------
#'
#' ## descriptive statistics
#'
#' ### total number of breeding events
#' nrow(data_all)
#' # [1] 1634
#'
#' ### mean/sd clutch size
#' mean(data_all$clutch_size, na.rm = TRUE)
#' # [1] 8.650242
#'
#' sd(data_all$clutch_size, na.rm = TRUE)
#' # [1] 2.293634
#'
#' ### range clutch size
#' range(data_all$clutch_size, na.rm = TRUE)
#' # [1] 1 17
#'
#' ### sample size for clutch size
#' sum(!is.na(data_all$clutch_size)) # breeding events
#' # [1] 1238
#' howmany(data_all$ring_number[!is.na(data_all$clutch_size)]) # ducks
#' # [1] 711
#'
#' ### mean/sd brood size
#' mean(data_all$brood_size, na.rm = TRUE)
#' # [1] 6.979642
#' sd(data_all$brood_size, na.rm = TRUE)
#' # [1] 3.133681
#'
#' ### range brood size
#' range(data_all$brood_size, na.rm = TRUE)
#' # [1] 1 15
#'
#' ### sample size for brood size
#' sum(!is.na(data_all$brood_size)) # breeding events
#' # [1] 1621
#' howmany(data_all$ring_number[!is.na(data_all$brood_size)]) # ducks
#' # [1] 790
#'
#' ### mean/sd body mass
#' mean(data_all$body_mass_g, na.rm = TRUE)
#' # [1] 817.4053
#' sd(data_all$body_mass_g, na.rm = TRUE)
#' # [1] 66.07133
#'
#' ### range body mass
#' range(data_all$body_mass_g, na.rm = TRUE)
#' # [1]  520 1030
#'
#' ### sample size for body mass
#' sum(!is.na(data_all$body_mass_g)) # breeding events
#' # [1] 1140
#' howmany(data_all$ring_number[!is.na(data_all$body_mass_g)]) # ducks
#' # [1] 754
#'
#' ### mean/sd wing length
#' mean(data_all$wing_length_mm, na.rm = TRUE)
#' # [1] 264.4053
#' sd(data_all$wing_length_mm, na.rm = TRUE)
#' # [1] 7.605253
#'
#' ### range wing length
#' range(data_all$wing_length_mm, na.rm = TRUE)
#' # [1] 226 287
#'
#' ### sample size for wing length
#' sum(!is.na(data_all$wing_length_mm)) # breeding events
#' # [1] 1162
#' howmany(data_all$ring_number[!is.na(data_all$wing_length_mm)]) # ducks
#' # [1] 773
#'
#' ### mean/sd hatching day of the year
#' mean(data_all$hatch_doy, na.rm = TRUE)
#' # [1] 146.5946
#' sd(data_all$hatch_doy, na.rm = TRUE)
#' # [1] 23.3779
#'
#' ### range hatching day of the year
#' range(data_all$hatch_doy, na.rm = TRUE)
#' # [1] 61 207
#'
#' ### sample size for hatching day of the year
#' sum(!is.na(data_all$hatch_doy)) # breeding events
#' # [1] 1411
#' howmany(data_all$ring_number[!is.na(data_all$hatch_doy)]) # ducks
#' # [1] 771
#'
#'
#' # Results: 3.2 Breeding site selection ----------------------------------------------------------
#'
#' ## descriptive statistics
#'
#' ### pct habitat type roof terraces
#' 100*mean(data_all$habitat_type_roof_terrace, na.rm = TRUE)
#' # [1] 29.62056
#'
#' ### sample size for habitat type roof terraces
#' sum(data_all$habitat_type_roof_terrace) # breeding events
#' # [1] 484
#' howmany(data_all$ring_number[data_all$habitat_type_roof_terrace]) # ducks
#' # [1] 267
#'
#' ### pct habitat type roof balconies
#' 100*mean(data_all$habitat_type_balcony, na.rm = TRUE)
#' # [1] 32.74174
#'
#' ### sample size for habitat type balconies
#' sum(data_all$habitat_type_balcony) # breeding events
#' # [1] 535
#' howmany(data_all$ring_number[data_all$habitat_type_balcony]) # ducks
#' # [1] 319
#'
#' ### pct habitat type roof courtyards
#' 100*mean(data_all$habitat_type_courtyard, na.rm = TRUE)
#' # [1] 23.86781
#'
#' ### sample size for habitat type courtyards
#' sum(data_all$habitat_type_courtyard) # breeding events
#' # [1] 390
#' howmany(data_all$ring_number[data_all$habitat_type_courtyard]) # ducks
#' # [1] 220
#'
#' ### pct habitat type other
#' 100*mean(data_all$habitat_type_other, na.rm = TRUE)
#' # [1] 4.406365
#'
#' ### sample size for habitat type other
#' sum(data_all$habitat_type_other) # breeding events
#' # [1] 72
#' howmany(data_all$ring_number[data_all$habitat_type_other]) # ducks
#' # [1] 37
#'
#' ### pct habitat type unknown
#' 100*mean(data_all$habitat_type_unknown, na.rm = TRUE)
#' # [1] 9.363525
#'
#' ### sample size for habitat type unknown
#' sum(data_all$habitat_type_unknown) # breeding events
#' # [1] 153
#' howmany(data_all$ring_number[data_all$habitat_type_unknown]) # ducks
#' # [1] 16
#'
#'
#' # Results: 3.3 Breeding site fidelity -------------------------------------------------------------
#'
#' ## descriptive statistics
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all$ring_number)
#' # [1] 795
#'
#' ### total number of ducks observed more than once
#' data_all |>
#'   filter(!is.na(.data$ring_number)) |>
#'   summarise(n = n(), .by = "ring_number") -> captures_per_duck
#'
#' captures_per_duck |>
#'   filter(.data$n > 1) |>
#'   nrow() -> reoccuring_ducks
#'
#' reoccuring_ducks
#' # [1] 228
#'
#' ## percentage of ducks captured more than once
#' 100*reoccuring_ducks/howmany(data_all$ring_number)
#' # [1] 28.67925
#'
#' ## range of number of captures
#' range(captures_per_duck$n)
#' # [1]  1 13
#'
#' ## mean/sd number of captures
#' mean(captures_per_duck$n)
#' # [1] 1.508176
#' sd(captures_per_duck$n)
#' # [1] 1.110232
#'
"_PACKAGE"
