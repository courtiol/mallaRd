#' Fit the Generalised Linear Mixed Effects Models
#'
#' This functions fit the GLMMs predicting the return probability of the adult females.
#'
#' @inheritParams arguments
#'
#' @return a dataframe with the formulas and all the statistics of goodness of fit
#' @export
#'
#' @examples
#' \dontrun{
#'   twoformulas <- prepare_formulas()[1:2,]
#'   fit(formulas = twoformulas, data = data_model)
#' }
#'
fit <- function(formulas, data, ncpus = 2) {
  if (ncpus > parallel::detectCores()) stop("Number of CPUs requested too large")
  oldplan <- future::plan(future::multisession, workers = ncpus)
  on.exit(future::plan(oldplan), add = TRUE)
  timing <- system.time({
    job <- furrr::future_map_dfr(seq_len(nrow(formulas)), .f = \(row) {
                    form <- formulas[row, "formula", drop = TRUE]
                    fit <- spaMM::fitme(stats::as.formula(form), data = data,
                                        family = stats::binomial(link = "logit"),
                                        method = "PQL/L",
                                        control.dist = list(dist.method = "Earth")) # for Matern only
                    mAIC <- stats::AIC(fit, verbose = FALSE, short.names = TRUE)["mAIC"]
                    cAIC <- stats::AIC(fit, verbose = FALSE, short.names = TRUE)["cAIC"]
                    logLik <- stats::logLik(fit)[[1]]
                    D <- TjurD(fit)
                    fit_time <- spaMM::how(fit, verbose = FALSE)[['fit_time']]
                    tibble::tibble(formula = form, logLik = logLik,
                                   mAIC = mAIC, cAIC = cAIC,
                                   TjursD =  D$TjurD, fit_time_s = fit_time)
                    }, .options = furrr::furrr_options(seed = TRUE))
  })
  print(timing)

  job$rank_mAIC <- rank(job$mAIC)
  job$rank_cAIC <- rank(job$cAIC)
  job$delta_mAIC <- job$mAIC - min(job$mAIC)
  job$delta_cAIC <- job$cAIC - min(job$cAIC)

  dplyr::left_join(formulas, job)
}


#' Compare the best fit to other fits using parametric bootstrap
#'
#' This function compute all the information required for model comparison.
#'
#' @inheritParams arguments
#'
#' @return a dataframe with the formulas and all the statistics of goodness of fit
#' @export
#'
#' @examples
#' \dontrun{
#'   threeformulas <- prepare_formulas()[1:3,]
#'   fits <- fit(formulas = threeformulas, data = data_model)
#'   comparison <- compare_fits(fits, data = data_model, ncpus = 20, boot.repl = 100)
#'   comparison
#' }
#'
compare_fits <- function(fits, data, ncpus = 2, boot.repl = 1000, seed = 123) {

  fits |>
    dplyr::filter(.data$logLik == max(.data$logLik)) |>
    dplyr::pull(.data$formula) -> best_formula

  fits |>
    dplyr::filter(.data$logLik != max(.data$logLik)) |>
    dplyr::pull(.data$formula) -> other_formulas

  best_fit <- spaMM::fitme(stats::as.formula(best_formula),
                           data = data,
                           family = stats::binomial(link = "logit"),
                           method = "PQL/L")

  timing <- system.time({
    job <- lapply(other_formulas, \(form) {
                  print(paste("Comparison best fit vs", form))
                  fit <- spaMM::fitme(stats::as.formula(form), data = data,
                                      family = stats::binomial(link = "logit"),
                                      method = "PQL/L")
                  res_boot <- compute_LRT2(best_fit, fit, ncpus = ncpus, boot.repl = boot.repl, seed = seed)
                  tibble::tibble(formula = form, chi2 = res_boot["chi2_LR"], p = res_boot["p_value"])
                  })
  })
  print(timing)
  do.call(rbind, job)
}

#' Compute the Tjur's D statistics
#'
#' Tjur's D measures the accuracy of the fitted model at predicting the fitted data. D is defined by
#' the average difference between the probabilities of staying for breeding events that did actually
#' occur at the same location and for those that did not occur at the same locations. The predictive
#' power increases with increasing D.
#'
#' @references Tjur, T. (2009). Coefficients of determination in logistic regression models - A new
#'   proposal: The coefficient of discrimination. The American Statistician, 63(4), 366-372.
#'
#' @inheritParams arguments
#'
#' @return a dataframe with the Tjur's D and associated statistics
#' @export
#'
TjurD <- function(fit) {
  tibble::as_tibble(cbind(return = stats::model.frame(fit)$return, prob = stats::predict(fit)[, 1])) |>
    dplyr::summarize(prob_return_when_return  = mean(.data$prob[.data$return == 1]),
                     prob_return_when_swap = mean(.data$prob[.data$return == 0]),
                     TjurD = .data$prob_return_when_return - .data$prob_return_when_swap)
}


#' Compute stuff on the fit
#'
#' @inheritParams arguments
#'
#' @return a tibble with information on the fit
#' @export
#'
#' @examples
#' summarize_fit(best_fit)
#'
summarize_fit <- function(best_fit, pretty = TRUE) {

  extract_formula <- function(fit) {
    paste(as.character(stats::formula(fit))[c(2, 3)], collapse = " ~ ")
  }

  summary_selected_fits <- tibble::tibble(model = c("best_fit_PQLL",
                                                    "best_fit_PQL",
                                                    "best_fit_fix_only_PQLL",
                                                    "best_fit_random_only_PQLL",
                                                    "best_fit_random_only_PQL",
                                                    "best_fit_fix_ID_PQLL",
                                                    "best_fit_fix_ID_PQL",
                                                    "best_fit_fix_location_PQLL",
                                                    "best_fit_fix_location_PQL",
                                                    "null_ID_PQLL",
                                                    "null_ID_PQL",
                                                    "null_location_PQLL",
                                                    "null_location_PQL",
                                                    "null_PQLL"))

  best_fit_PQLL              <- stats::update(best_fit, method = "PQL/L")
  best_fit_PQL               <- stats::update(best_fit, method = "PQL")

  best_fit_fix_only_PQLL     <- stats::update(best_fit, . ~ . - (1|individual_ID) - (1|location_ID), method =  "PQL/L")

  best_fit_random_only_PQLL  <- stats::update(best_fit, . ~ 1 + (1|individual_ID) + (1|location_ID), method = "PQL/L")
  best_fit_random_only_PQL   <- stats::update(best_fit, . ~ 1 + (1|individual_ID) + (1|location_ID), method = "PQL")

  best_fit_fix_ID_PQLL       <- stats::update(best_fit, . ~ . - (1|location_ID), method = "PQL/L")
  best_fit_fix_ID_PQL        <- stats::update(best_fit, . ~ . - (1|location_ID), method = "PQL")

  best_fit_fix_location_PQLL <- stats::update(best_fit, . ~ . - (1|individual_ID), method = "PQL/L")
  best_fit_fix_location_PQL  <- stats::update(best_fit, . ~ . - (1|individual_ID), method = "PQL")

  null_ID_PQLL               <- stats::update(best_fit, . ~ 1 + ( 1|individual_ID), method = "PQL/L")
  null_ID_PQL                <- stats::update(best_fit, . ~ 1 + ( 1|individual_ID), method = "PQL")

  null_location_PQLL         <- stats::update(best_fit, . ~ 1 + (1|location_ID), method = "PQL/L")
  null_location_PQL          <- stats::update(best_fit, . ~ 1 + (1|location_ID), method = "PQL")

  null_PQLL                 <- stats::update(best_fit, . ~ 1, method = "PQL/L")

  summary_selected_fits$formula    <- sapply(summary_selected_fits$model, \(fit) extract_formula(get(fit)))
  summary_selected_fits$mAIC       <- sapply(summary_selected_fits$model, \(fit) stats::AIC(get(fit), verbose = FALSE, short.names = TRUE)["mAIC"])
  summary_selected_fits$cAIC       <- sapply(summary_selected_fits$model, \(fit) stats::AIC(get(fit), verbose = FALSE, short.names = TRUE)["cAIC"])
  summary_selected_fits$TjursD     <- sapply(summary_selected_fits$model, \(fit)  TjurD(get(fit))$TjurD)
  summary_selected_fits$delta_mAIC <- summary_selected_fits$mAIC - min(summary_selected_fits$mAIC)
  summary_selected_fits$delta_cAIC <- summary_selected_fits$cAIC - min(summary_selected_fits$cAIC)

  summary_selected_fits$model <- dplyr::case_match(summary_selected_fits$model,
                                                   "best_fit_PQLL" ~ "full model (PQL/L)",
                                                   "best_fit_PQL" ~ "full model (PQL)",
                                                   "best_fit_fix_only_PQLL" ~ "fixed effects (PQL/L)",
                                                   "best_fit_random_only_PQLL" ~ "random effects (PQL/L)",
                                                   "best_fit_random_only_PQL" ~ "random effects (PQL)",
                                                   "best_fit_fix_ID_PQLL" ~ "fixed effects + individual ID (PQL/L)",
                                                   "best_fit_fix_ID_PQL" ~ "fixed effects + individual ID (PQL)",
                                                   "best_fit_fix_location_PQLL" ~ "fixed effects + location ID (PQL/L)",
                                                   "best_fit_fix_location_PQL" ~ "fixed effects + location ID (PQL)",
                                                   "null_ID_PQLL" ~ "individual ID (PQL/L)",
                                                   "null_ID_PQL" ~ "individual ID (PQL)",
                                                   "null_location_PQLL" ~ "location ID (PQL/L)",
                                                   "null_location_PQL" ~ "location ID (PQL)",
                                                   "null_PQLL" ~ "null model (PQL/L)")

  summary_selected_fits$model <- factor(summary_selected_fits$model, levels = c("full model (PQL/L)",
                                                                                "full model (PQL)",
                                                                                "fixed effects (PQL/L)",
                                                                                "random effects (PQL/L)",
                                                                                "random effects (PQL)",
                                                                                "fixed effects + individual ID (PQL/L)",
                                                                                "fixed effects + individual ID (PQL)",
                                                                                "fixed effects + location ID (PQL/L)",
                                                                                "fixed effects + location ID (PQL)",
                                                                                "individual ID (PQL/L)",
                                                                                "individual ID (PQL)",
                                                                                "location ID (PQL/L)",
                                                                                "location ID (PQL)",
                                                                                "null model (PQL/L)"))

  summary_selected_fits$group  <- factor(c("fixed+random", "fixed+random",
                                           "fixed",
                                           "random", "random",
                                           "fixed+random", "fixed+random",
                                           "fixed+random", "fixed+random",
                                           "random", "random",
                                           "random", "random",
                                           "null model"))

  if (pretty) {
    summary_selected_fits |>
      dplyr::select(-"group", -"formula") |>
      dplyr::arrange(.data$delta_mAIC) |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), \(x) pretty(x))) -> summary_selected_fits
  }

  summary_selected_fits
}

#' Compute Likelihood Ratio Test using parametric bootstrap
#'
#' @inheritParams arguments
#'
#' @return the output of [spaMM::LRT]
#' @export
#'
compute_LRT <- function(fit, fit_null, ncpus = 2, boot.repl = 1000, seed = 123) {
  nbcores <- parallel::detectCores()
  nbcores <- min(c(nbcores, ncpus))
  spaMM::spaMM.options(nb_cores = nbcores)
  requireNamespace("doSNOW")
  spaMM::LRT(fit, fit_null, boot.repl = boot.repl, seed = seed)
}


#' Compute Likelihood Ratio Test using parametric bootstrap for non nested models
#'
#' @inheritParams arguments
#'
#' @return the output of [spaMM::LRT]
#' @export
#'
compute_LRT2 <- function(best_fit, fit_alt, ncpus = 2, boot.repl = 1000, seed = 123) {
  nbcores <- parallel::detectCores()
  nbcores <- min(c(nbcores, ncpus))
  spaMM::spaMM.options(nb_cores = nbcores)
  requireNamespace("doSNOW")
  LRT_obs <- 2*(stats::logLik(best_fit) - stats::logLik(fit_alt))

  LRT_H0 <- function(y, what = NULL, best_fit, fit_alt, ...) {
     data <- best_fit$data
     data[, deparse(stats::formula(best_fit)[[2]])] <- y ## replaces original response
     best_refit <- stats::update(best_fit, data = data)  ## fits the full model on the simulated response
     alt_refit  <- stats::update(fit_alt, data = data)   ## fits the alternative model on the simulated response
     2*(stats::logLik(best_refit) - stats::logLik(alt_refit)) ## compute LRT
    }

  res_boot <- spaMM::spaMM_boot(fit_alt, simuland = LRT_H0, nsim = boot.repl,
                                best_fit = best_fit, fit_alt = fit_alt, seed = seed, nb_cores = ncpus,
                                type = "marginal")[["bootreps"]]

  p <- (sum(as.vector(res_boot) >= LRT_obs) + 1) / (length(as.vector(res_boot)) + 1)
  c(chi2_LR = LRT_obs[[1]], p_value = p)
}

