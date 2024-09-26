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
#' Note that the workflow relies on functions from other packages (e.g. \pkg{dplyr}), but we
#' reexported these functions so you don't have to load these packages as long as **mallaRd** is
#' loaded. If you want to expand upon the proposed workflow and require more functions, you will
#' however have to load such packages explicitly.
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
#' data_all_known <- prepare_data(data_all, filtering = FALSE)
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_all_known, overwrite = TRUE)
#'
#' # Introduction-----------------------------------------------------------------------------------
#'
#' ## Summary statistics for the full dataset
#'
#' ### breeding events from identified (ringed) ducks
#' nrow(data_all_known)
#' # [1] 1199
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all_known$individual_ID)
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
#' nrow(data_all_known)
#' # [1] 1199
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all_known$individual_ID)
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
#' #   description                           breeding_events_removed remaining_events delta_events_pct ducks_removed remaining_ducks delta_duck_pct
#' #   <chr>                                                   <dbl>            <int>            <dbl>         <dbl>           <int>          <dbl>
#' # 1 raw data                                                    0             1634            0                 0             795          0
#' # 2 removed because unknown ducks                             435             1199           26.6               0             795          0
#' # 3 removed because only observed once                        567              632           34.7             567             228         71.3
#' # 4 removed because of reshaping the data                       0              632            0                 0             228          0
#' # 5 removed because first occurrence per duck not used        228              404           14.0               0             228          0
#' # 6 removed because recapture events more than 1 year apart    91              313            5.57             42             186          5.28
#' # 7 removed because of missing values for predictors           12              301            0.734             6             180          0.755
#' # 8 final number of rows                                      301              301           18.4               0             180          0
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(data_model, overwrite = TRUE)
#'
#'
#' ## Descriptive statistics for the dataset used fo modelling
#'
#' ### many statistics are directly provided by the table above displayed while calling `prepare_data()`
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
#'     "relocation_distance_previous_z",
#'     "trafficvolume500_previous_z", "trafficvolume1000_previous_z", "trafficvolume2000_previous_z",
#'     "populationdensity500_previous_z","populationdensity1000_previous_z", "populationdensity2000_previous_z")])
#' diag(cor_matrix) <- NA
#' cor_matrix[c("trafficvolume500_previous_z",
#'              "trafficvolume1000_previous_z",
#'              "trafficvolume2000_previous_z"),
#'            c("trafficvolume500_previous_z",
#'              "trafficvolume1000_previous_z",
#'              "trafficvolume2000_previous_z")] <- NA
#' cor_matrix[c("populationdensity500_previous_z",
#'              "populationdensity1000_previous_z",
#'              "populationdensity2000_previous_z"),
#'            c("populationdensity500_previous_z",
#'              "populationdensity1000_previous_z",
#'              "populationdensity2000_previous_z")] <- NA
#' range(cor_matrix, na.rm = TRUE)
#' # [1] -0.1119443  0.6505292
#'
#'
#' # Methods: 2.3 Statistical analysis -------------------------------------------------------------
#'
#' ## Prepare formulas for model fits
#'
#' formulas_df <- prepare_formulas()
#'
#' ## number of different models to fit
#' nrow(formulas_df)
#' # [1] 27
#'
#' packageVersion("spaMM")
#' # [1] ‘4.4.23.1’
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
#'
#' # Results: 3.2 Type of breeding site ------------------------------------------------------------
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
#' ## stats for fig 2
#' nrow(data_all) # breeding events
#' # [1] 1634
#' howmany(data_all$ring_number)
#' # [1] 795
#'
#'
#' # Results: 3.3 Breeding site fidelity -----------------------------------------------------------
#'
#' ## descriptive statistics
#'
#' ### total number of identified (ringed) ducks
#' howmany(data_all_known$individual_ID)
#' # [1] 795
#'
#' ### total number of ducks observed more than once
#' data_all_known |>
#'   summarise(n = n(), .by = "individual_ID") -> captures_per_duck
#'
#' captures_per_duck |>
#'   filter(.data$n > 1) |>
#'   nrow() -> reoccuring_ducks
#'
#' reoccuring_ducks
#' # [1] 228
#'
#' ## percentage of ducks captured more than once
#' 100*reoccuring_ducks/howmany(data_all_known$individual_ID)
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
#' ## percentage of ducks observed multiple times that used the same site more than once
#' ## note: also used in abstract
#' data_all_known |>
#'   mutate(n_obs = n(), .by = "individual_ID") |>
#'   filter(.data$n_obs > 1, !is.na(.data$location_ID)) |>
#'   count(.data$location_ID, .data$individual_ID) |>
#'   summarise(reuse = any(.data$n > 2), .by = "individual_ID") |>
#'   count(.data$reuse) |>
#'   mutate(pct = 100*.data$n/sum(.data$n))
#' # # A tibble: 2 × 3
#' #   reuse     n   pct
#' #   <lgl> <int> <dbl>
#' # 1 FALSE   176  77.2
#' # 2 TRUE     52  22.8
#'
#' ## number of breeding sites used by known ducks
#' howmany(data_all_known$location_ID)
#' # 863
#'
#' ## number of breeding sites used by more than one known ducks
#' data_all_known |>
#'   filter(!is.na(.data$location_ID)) |>
#'   mutate(n_usage_breedingsite = n(), .by = "location_ID") |>
#'   filter(.data$n_usage_breedingsite > 1) |>
#'   summarise(n_ducks = howmany(individual_ID), .by = "location_ID") -> reocuring_breeding_site
#'
#' ### number of breeding sites used more than once
#' howmany(reocuring_breeding_site$location_ID)
#' # [1] 153
#'
#' ### proportion of breeding sites used more than once
#' howmany(reocuring_breeding_site$location_ID)/howmany(data_all_known$location_ID)
#' # [1] 0.1772885
#'
#' ### number of breeding sites used by more than one duck
#' reocuring_breeding_site |>
#'   count(n_ducks > 1)
#'  # # A tibble: 2 × 2
#'  #   `n_ducks > 1`     n
#'  #   <lgl>         <int>
#'  # 1 FALSE            89
#'  # 2 TRUE             64
#'
#' ## stats for fig 3
#'
#' ### breeding events from identified (ringed) ducks
#' sum(!is.na(data_all_known$location_ID))
#' # [1] 1194
#'
#' ### total number of identified (ringed) ducks breeding at known locations
#' howmany(data_all_known$individual_ID[!is.na(data_all_known$location_ID)])
#' # [1] 794
#'
#' ### breeding events for recaptured individuals
#' nrow(data_model)
#' # [1] 301
#'
#' ### number of recaptured individuals
#' howmany(data_model$individual_ID)
#' # [1] 180
#'
#' ### breeding events at site mostly used by ducks
#'
#' ### fitting models
#' ## NOTE: this takes 40 sec using 27 CPUs
#' ## turn fitmodel to TRUE to run and do adjust ncpus to your system
#' fitmodel <- FALSE
#' if (fitmodel) {
#'   fitting_results <- fit(formulas = formulas_df, data = data_model, ncpus = 27)
#'
#'   ## for developer only: save the created data into the package
#'   # usethis::use_data(fitting_results, overwrite = TRUE)
#' }
#'
#' ### best model
#' fitting_results |>
#'   filter(.data$logLik == max(.data$logLik)) |>
#'   pull(.data$formula) -> best_formula
#' best_formula
#' # [1] "return~habitat_type_previous+PSW1000_previous+trafficvolume2000_previous_z+
#' # populationdensity500_previous_z+brood_size_previous_z+delta_season+relocation_distance_previous_z+
#' # (1|individual_ID)+(1|location_ID)+1"
#'
#' ### best fit
#' best_fit <- fitme(as.formula(best_formula),
#'                   data = data_model,
#'                   family = stats::binomial(link = "logit"),
#'                   method = "PQL/L")
#' best_fit
#' # formula: return ~ habitat_type_previous + PSW1000_previous + trafficvolume2000_previous_z +
#' #     populationdensity500_previous_z + brood_size_previous_z +
#' #     delta_season + relocation_distance_previous_z + (1 | individual_ID) +
#' #     (1 | location_ID) + 1
#' # Estimation of fixed effects by h-likelihood approximation.
#' # Estimation of lambda by 'outer' ML, maximizing logL.
#' # family: binomial( link = logit )
#' #  ------------ Fixed effects (beta) ------------
#' #                                   Estimate Cond. SE  t-value
#' # (Intercept)                        0.23537   0.4407  0.53409
#' # habitat_type_previouscourtyard    -0.24255   0.5277 -0.45964
#' # habitat_type_previousother        -0.56527   1.5568 -0.36309
#' # habitat_type_previousroof_terrace  0.01982   0.4589  0.04319
#' # PSW1000_previousyes               -0.02228   0.4075 -0.05469
#' # trafficvolume2000_previous_z      -0.12668   0.2196 -0.57680
#' # populationdensity500_previous_z   -0.42710   0.2209 -1.93375
#' # brood_size_previous_z              0.11891   0.1767  0.67291
#' # delta_seasonsame breeding season   0.78563   0.4459  1.76201
#' # relocation_distance_previous_z    -0.31289   0.1871 -1.67239
#' #  --------------- Random effects ---------------
#' # Family: gaussian( link = identity )
#' #            --- Variance parameters ('lambda'):
#' # lambda = var(u) for u ~ Gaussian;
#' #    individua.  :  0.003021
#' #    location_.  :  3.492
#' # # of obs: 301; # of groups: individua., 180; location_., 197
#' #  ------------- Likelihood values  -------------
#' #                          logLik
#' #        h-likelihood:  -65.56095
#' # logL       (p_v(h)): -177.47029
#'
#' ## for developer only: save the created data into the package
#' # usethis::use_data(best_fit, overwrite = TRUE)
#'
#'
#' ### comparison to alternative parameterisations
#' ## NOTE: this takes several minutes using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'  fit_comparison <- compare_fits(fitting_results, data = data_model, ncpus = 100)
#'
#'  ## for developer only: save the created data into the package
#'  # usethis::use_data(fit_comparison, overwrite = TRUE)
#' }
#'
#'
#' ### alternative proxies retained among best models
#' fit_comparison |>
#'   filter(.data$p >= 0.05) -> top_models
#'  nrow(top_models)
#'  # [1] 18
#'
#'  top_models |>
#'   select("formula") |>
#'   mutate(DNSW =                  grepl("DNSW", .data$formula),
#'          PSW1000 =               grepl("PSW1000", .data$formula),
#'          PSW2000 =               grepl("PSW2000", .data$formula),
#'          trafficvolume500 =      grepl("trafficvolume500", .data$formula),
#'          trafficvolume1000 =     grepl("trafficvolume1000", .data$formula),
#'          trafficvolume2000 =     grepl("trafficvolume2000", .data$formula),
#'          populationdensity500 =  grepl("populationdensity500", .data$formula),
#'          populationdensity1000 = grepl("populationdensity1000", .data$formula),
#'          populationdensity2000 = grepl("populationdensity2000", .data$formula)) |>
#'    select(-"formula") |>
#'    summarise(across(everything(), any)) |> t()
#' #                        [,1]
#' # DNSW                   TRUE
#' # PSW1000                TRUE
#' # PSW2000                TRUE
#' # trafficvolume500       TRUE
#' # trafficvolume1000      TRUE
#' # trafficvolume2000      TRUE
#' # populationdensity500   TRUE
#' # populationdensity1000  TRUE
#' # populationdensity2000  TRUE
#'
#'
#' ## refit PQL for more accurate random effects
#' best_fit_PQL <- fitme(as.formula(best_formula),
#'                       data = data_model,
#'                       family = stats::binomial(link = "logit"),
#'                       method = "PQL")
#'
#' ### test of overall model
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_null <- update(best_fit, . ~ 1)
#'   test_null <- compute_LRT(best_fit, fit_null, ncpus = 100)
#'   test_null
#' #      chi2_LR df      p_value
#' # p_v 42.41568 11 1.372163e-05
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.000999
#' # Bartlett-corrected LR test:
#' #      chi2_LR df      p_value
#' # p_v 49.28926 11 8.397054e-07
#' }
#'
#' ### test all fixed effects at once
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_fix <- update(best_fit, . ~ 1 + (1|individual_ID) + (1|location_ID))
#'   test_fix <- compute_LRT(best_fit, fit_no_fix, ncpus = 100)
#'   test_fix
#' #      chi2_LR df    p_value
#' # p_v 20.10444  9 0.01727868
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.036
#' # Bartlett-corrected LR test:
#' #      chi2_LR df    p_value
#' # p_v 17.66125  9 0.03931335
#' }
#'
#' ### test all random effects at once
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_random <- update(best_fit, . ~ . - (1|individual_ID) - (1|location_ID), method = "PQL")
#'   test_random <- compute_LRT(best_fit_PQL, fit_no_random, ncpus = 100)
#'   test_random
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.000999
#' 2*(logLik(best_fit_PQL) - logLik(fit_no_random)) ## LRT
#' # 27.07088
#' }
#'
#' ### test of habitat type
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_habitat_type <- update(best_fit, . ~ . - habitat_type_previous)
#'   test_habitat_type <- compute_LRT(best_fit, fit_no_habitat_type, ncpus = 100)
#'   test_habitat_type
#' #       chi2_LR df   p_value
#' # p_v 0.6766501  3 0.8786819
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.9
#' # Bartlett-corrected LR test:
#' #       chi2_LR df   p_value
#' # p_v 0.5571846  3 0.9061601
#' }
#'
#' ### test of presence of suitable water
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_PSW1000 <- update(best_fit, . ~ . - PSW1000_previous)
#'   test_PSW1000 <- compute_LRT(best_fit, fit_no_PSW1000, ncpus = 100)
#'   test_PSW1000
#' #         chi2_LR df   p_value
#' # p_v 0.001290191  1 0.9713468
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.974
#' # Bartlett-corrected LR test:
#' #         chi2_LR df   p_value
#' # p_v 0.001049238  1 0.9741595
#' }
#'
#' ### test of traffic volume
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_traffic <- update(best_fit, . ~ . - trafficvolume2000_previous_z)
#'   test_traffic <- compute_LRT(best_fit, fit_no_traffic, ncpus = 100)
#'   test_traffic
#' #       chi2_LR df   p_value
#' # p_v 0.4229285  1 0.5154794
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.539
#' # Bartlett-corrected LR test:
#' #       chi2_LR df   p_value
#' # p_v 0.3809129  1 0.5371151
#' }
#'
#' ### test of human pop density
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_density <- update(best_fit, . ~ . - populationdensity500_previous_z)
#'   test_density <- compute_LRT(best_fit, fit_no_density, ncpus = 100)
#'   test_density
#' #      chi2_LR df    p_value
#' # p_v 5.434193  1 0.01974621
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.039
#' # Bartlett-corrected LR test:
#' #      chi2_LR df    p_value
#' # p_v 4.608996  1 0.03180465
#' }
#'
#' ### describing effect of population density
#' range(data_all$populationdensity500, na.rm = TRUE)
#' # 0.0000 295.3047
#' mean(data_all$populationdensity500, na.rm = TRUE)
#' # 106.7098
#' sd(data_all$populationdensity500, na.rm = TRUE)
#' # 62.53258
#'
#' range(data_model$populationdensity500_previous, na.rm = TRUE)
#' # 9.053046 276.798559
#' mean(data_model$populationdensity500_previous, na.rm = TRUE)
#' # 109.055
#' sd(data_model$populationdensity500_previous, na.rm = TRUE)
#' # 58.08383
#'
#' ### Odds ratio when adding 100 humans / hectare (mind the z transformation)
#' beta_popdens_z <- fixef(best_fit)["populationdensity500_previous_z"]
#' effect_popdens <- beta_popdens_z*100/attr(data_model$populationdensity500_previous_z, "sd")
#' OR_popdens_z <- exp(effect_popdens) ## odds ratio +100
#' OR_popdens_z
#' # 0.479356
#' SE_popdens_z <- sqrt(vcov(best_fit)["populationdensity500_previous_z", "populationdensity500_previous_z"])
#' exp(qnorm(0.025, mean = effect_popdens, sd = SE_popdens_z)) ## lower CI boundary
#' # 0.3109263
#' exp(qnorm(0.975, mean = effect_popdens, sd = SE_popdens_z)) ## upper CI boundary
#' # 0.7390246
#'
#' ### test of previous breeding success
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_BS <- update(best_fit, . ~ . - brood_size_previous_z)
#'   test_BS <- compute_LRT(best_fit, fit_no_BS, ncpus = 100)
#'   test_BS
#' #      chi2_LR df  p_value
#' # p_v 0.689293  1 0.406405
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.425
#' # Bartlett-corrected LR test:
#' #       chi2_LR df   p_value
#' # p_v 0.6385657  1 0.4242307
#' }
#'
#' ### test of delta breeding season
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_season <- update(best_fit, . ~ . - delta_season)
#'   test_season <- compute_LRT(best_fit, fit_no_season, ncpus = 100, boot.repl = 100000)
#'   test_season ## NB: significant for other seeds at 1000 bootstraps, hence 100000
#' #      chi2_LR df    p_value
#' # p_v 3.973929  1 0.04620983
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.0559
#' # Bartlett-corrected LR test:
#' #      chi2_LR df    p_value
#' # p_v 3.846168  1 0.04985977
#' }
#'
#'
#' ### test of relocation distance
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_relocdist <- update(best_fit, . ~ . - relocation_distance_previous_z)
#'   test_relocdist <- compute_LRT(best_fit, fit_no_relocdist, ncpus = 100)
#'   test_relocdist
#' #      chi2_LR df    p_value
#' # p_v 4.282736  1 0.03850132
#' #  ======== Bootstrap: ========
#' # Raw simulated p-value: 0.043
#' # Bartlett-corrected LR test:
#' #      chi2_LR df    p_value
#' # p_v 4.198098  1 0.04046935
#' }
#'
#' ### describing effect of relocation distance (for full data, not using previous)
#' sum(!is.na(data_all_known$relocation_distance))
#' # [1] 1192
#' mean(data_all_known$relocation_distance, na.rm = TRUE)
#' # [1] 2420.263
#' sd(data_all_known$relocation_distance, na.rm = TRUE)
#' # [1] 3738.127
#' range(data_all_known$relocation_distance, na.rm = TRUE)
#' # [1] 30.98786 37386.42255
#' sum(data_all_known$relocation_distance[!is.na(data_all_known$relocation_distance)] < 5000)
#' # [1] 1072
#' mean(data_all_known$relocation_distance[!is.na(data_all_known$relocation_distance)] < 5000)
#' # [1] 0.8993289
#' sum(data_all_known$relocation_distance[!is.na(data_all_known$relocation_distance)] > 10000)
#' # [1] 77
#' mean(data_all_known$relocation_distance[!is.na(data_all_known$relocation_distance)] > 10000)
#' # [1] 0.06459732
#'
#' sum(!is.na(data_model$relocation_distance_previous))
#' # [1] 301
#' mean(data_model$relocation_distance_previous)
#' # [1] 2172.022
#' sd(data_model$relocation_distance_previous)
#' # [1] 3299.925
#' range(data_model$relocation_distance_previous)
#' # [1] 82.12002 30905.78841
#' sum(data_model$relocation_distance_previous < 5000)
#' # [1] 280
#' mean(data_model$relocation_distance_previous < 5000)
#' # [1] 0.9302326
#' sum(data_model$relocation_distance_previous > 10000)
#' # [1] 14
#' mean(data_model$relocation_distance_previous > 10000)
#' # [1] 0.04651163
#'
#' ### Odds ratio when adding 5 km (mind the z transformation)
#' beta_reloc_z <- fixef(best_fit)["relocation_distance_previous_z"]
#' effect_reloc <- beta_reloc_z*5000/attr(data_model$relocation_distance_previous_z, "sd")
#' OR_reloc_z <- exp(effect_reloc) ## odds ratio for 5 km
#' OR_reloc_z
#' #  0.62245
#' SE_reloc_z <- sqrt(vcov(best_fit)["relocation_distance_previous_z", "relocation_distance_previous_z"])
#' exp(qnorm(0.025, mean = effect_reloc, sd = SE_reloc_z)) ## lower CI
#' # [1] 0.4313705
#' exp(qnorm(0.975, mean = effect_reloc, sd = SE_reloc_z)) ## upper CI
#' # [1] 0.8981699
#'
#' ### test random effect of individual
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_ID <- update(best_fit_PQL, . ~ . - (1|individual_ID))
#'   test_ID <- compute_LRT(best_fit_PQL, fit_no_ID, ncpus = 100)
#'   test_ID
#' # ======== Bootstrap: ========
#' # Raw simulated p-value: 0.000999
#' 2*(logLik(best_fit_PQL) - logLik(fit_no_ID)) ## LRT
#' # 10.46622
#' }
#'
#' ### test random effect of location
#' ## NOTE: this takes ca. 60 sec using 100 CPUs
#' ## turn doboot to TRUE to run and do adjust ncpus to your system
#' doboot <- FALSE
#' if (doboot) {
#'   fit_no_loc <- update(best_fit_PQL, . ~ . - (1|location_ID))
#'   test_loc <- compute_LRT(best_fit_PQL, fit_no_loc, ncpus = 100)
#'   test_loc
#' # ======== Bootstrap: ========
#' # Raw simulated p-value: 0.000999
#' 2*(logLik(best_fit_PQL) - logLik(fit_no_loc)) ## LRT
#' # 14.59554
#' }
#'
#' ## Comparing predictive power
#'
#' summarize_fit(best_fit)
#' # A tibble: 14 × 6
#' #  model                                 mAIC  cAIC  TjursD delta_mAIC delta_cAIC
#' #  <fct>                                 <chr> <chr> <chr>  <chr>      <chr>
#' # 1 fixed effects + location ID (PQL)     379.  363.  0.333  0.00       7.09
#' # 2 full model (PQL/L)                    379.  359.  0.496  0.277      2.59
#' # 3 fixed effects + location ID (PQL/L)   379.  365.  0.302  0.783      8.56
#' # 4 random effects (PQL/L)                381.  366.  0.484  2.38       9.84
#' # 5 random effects (PQL)                  381.  366.  0.506  2.42       9.79
#' # 6 fixed effects + individual ID (PQL)   382.  369.  0.291  3.69       13.0
#' # 7 location ID (PQL)                     382.  369.  0.272  3.78       13.1
#' # 8 location ID (PQL/L)                   383.  370.  0.269  3.90       13.2
#' # 9 fixed effects + individual ID (PQL/L) 383.  371.  0.261  4.22       14.3
#' # 0 full model (PQL)                      384.  356.  0.719  4.84       0.00
#' # 1 individual ID (PQL)                   386.  374.  0.237  7.39       17.9
#' # 2 individual ID (PQL/L)                 386.  374.  0.233  7.50       18.1
#' # 3 fixed effects (PQL/L)                 389.  389.  0.0927 10.6       32.9
#' # 4 null model (PQL/L)                    399.  399.  0.00   20.7       43.0
#'
#' # Discussion ------------------------------------------------------------------------------------
#'
#' ## Duck that returned the most
#' ID_max_return <- names(which.max(table(data_all_known$individual_ID)))
#' ID_max_return
#' # "JC69668"
#' 
#' ### Number of returns
#' sum(data_all_known$individual_ID == ID_max_return)
#' # [1] 13
#'
#' ### Number of years
#' range(data_all_known$date[data_all_known$individual_ID == ID_max_return])
#' # [1] "2012-06-18" "2020-07-06" # 9 years
#'
#' ### Most used site for the duck
#' location_max_return <- names(which.max(table(data_all_known$location_ID[data_all_known$individual_ID == ID_max_return])))
#'
#' #### Which type of habitat was it?
#' unique(data_all_known$habitat_type[data_all_known$individual_ID == ID_max_return & data_all_known$location_ID == location_max_return])
#' # [1] courtyard
#'
#' #### How often this site was used by this duck?
#' length(data_all_known$habitat_type[data_all_known$individual_ID == ID_max_return & data_all_known$location_ID == location_max_return])
#' # [1] 12
#'
#'
#' # Figures ---------------------------------------------------------------------------------------
#'
#' ## choose to save figures on the hard drive or not
#' redraw <- FALSE ## turn to TRUE for saving
#'
#' ## create folder where figures will be saved
#' if (redraw) {
#'   dir.create("figures")
#' }
#'
#'
#' ## figure S1
#' figureS1(data_all)
#' if (redraw) {
#'   showtext::showtext_opts(dpi = 300)
#'   ggsave("figures/figS1.pdf", width = 19, height = 12, dpi = 300, units = "cm")
#'   ggsave("figures/figS1.png", width = 19, height = 12, dpi = 300, units = "cm")
#' }
#'
#' ## figure 1
#' figure1(data_all)
#' if (redraw) {
#'   showtext::showtext_opts(dpi = 300)
#'   ggsave("figures/fig1.pdf", width = 19, height = 15, dpi = 300, units = "cm")
#'   ggsave("figures/fig1.png", width = 19, height = 15, dpi = 300, units = "cm")
#' }
#'
#' ## figure 2
#' figure2(data_all, data_model)
#' if (redraw) {
#'   showtext::showtext_opts(dpi = 300)
#'   ggsave("figures/fig2.pdf", width = 19, height = 9, dpi = 300, units = "cm")
#'   ggsave("figures/fig2.png", width = 19, height = 9, dpi = 300, units = "cm")
#' }
#'
#' ## figure 3
#' figure3(data_model, best_fit, level = qnorm(0.75), levels = NULL) # levels = NULL is there to prevent partial matching for level
#'                                                                   # qnorm(0.75) for the so-called probable error
#' if (redraw) {
#'   showtext::showtext_opts(dpi = 300)
#'   ggsave("figures/fig3.pdf", width = 19, height = 9, dpi = 300, units = "cm")
#'   ggsave("figures/fig3.png", width = 19, height = 9, dpi = 300, units = "cm")
#' }
#'
#' # Tables ----------------------------------------------------------------------------------------
#'
#' ## Table 2
#' 
#' ### info on biometric parameters
#' table_biometrics(data_all)
#' #            parameter   mean_sd    range n_breedingevents n_adultfemales
#' # 1        Clutch size 8.65±2.29     1-17             1238            711
#' # 2         Brood size 6.98±3.13     1-15             1621            790
#' # 3      Body mass (g) 817.±66.1 520-1030             1140            754
#' # 4   Wing length (mm) 264.±7.61  226-287             1162            773
#' # 5 Hatching day (doy) 147.±23.4   61-207             1411            771
#' 
#' 
#' ## Table 3
#'
#' ### estimates fixed effects
#' pretty(cbind(estimate = fixef(best_fit), Cond.SE = sqrt(diag(vcov(best_fit)))))
#' #                                   estimate Cond.SE
#' # (Intercept)                          0.235   0.441
#' # habitat_type_previouscourtyard      -0.243   0.528
#' # habitat_type_previousother          -0.565    1.56
#' # habitat_type_previousroof_terrace   0.0198   0.459
#' # PSW1000_previousyes                -0.0223   0.407
#' # trafficvolume2000_previous_z        -0.127   0.220
#' # populationdensity500_previous_z     -0.427   0.221
#' # brood_size_previous_z                0.119   0.177
#' # delta_seasonsame breeding season     0.786   0.446
#' # relocation_distance_previous_z      -0.313   0.187
#'
#' ### estimates random effects
#' pretty(cbind(lambda = c(best_fit_PQL$lambda)))
#' #            lambda
#' # individua.   1.12
#' # location_.   13.3
#'
#' ### breeding events for recaptured individuals
#' nrow(data_model)
#' # [1] 301
#'
#' ### number of recaptured individuals
#' howmany(data_model$individual_ID)
#' # [1] 180
#'
#'
"_PACKAGE"

