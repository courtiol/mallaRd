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
summarize_fit <- function(best_fit, method = "PQL/L", pretty = TRUE) {

  extract_formula <- function(fit) {
    paste(as.character(stats::formula(fit))[c(2, 3)], collapse = " ~ ")
  }

  summary_selected_fits <- tibble::tibble(model = c("best_fit",
                                                    "best_fit_fix_only", "best_fit_random_only",
                                                    "best_fit_fix_ID", "best_fit_fix_location",
                                                    "null_ID", "null_location",
                                                    "null"))

  best_fit              <- stats::update(best_fit, method = method)
  best_fit_fix_only     <- stats::update(best_fit, . ~ . - (1|individual_ID) - (1|location_ID), method = method)
  best_fit_random_only  <- stats::update(best_fit, . ~ 1 + (1|individual_ID) + (1|location_ID), method = method)
  best_fit_fix_ID       <- stats::update(best_fit, . ~ . - (1|location_ID), method = method)
  best_fit_fix_location <- stats::update(best_fit, . ~ . - (1|individual_ID), method = method)
  null_ID               <- stats::update(best_fit, . ~ 1 + ( 1|individual_ID), method = method)
  null_location         <- stats::update(best_fit, . ~ 1 + (1|location_ID), method = method)
  null                  <- stats::update(best_fit, . ~ 1, method = method)

  summary_selected_fits$formula    <- sapply(summary_selected_fits$model, \(fit) extract_formula(get(fit)))
  summary_selected_fits$mAIC       <- sapply(summary_selected_fits$model, \(fit) stats::AIC(get(fit), verbose = FALSE, short.names = TRUE)["mAIC"])
  summary_selected_fits$cAIC       <- sapply(summary_selected_fits$model, \(fit) stats::AIC(get(fit), verbose = FALSE, short.names = TRUE)["cAIC"])
  summary_selected_fits$TjursD     <- sapply(summary_selected_fits$model, \(fit)  TjurD(get(fit))$TjurD)
  summary_selected_fits$delta_mAIC <- summary_selected_fits$mAIC - min(summary_selected_fits$mAIC)
  summary_selected_fits$delta_cAIC <- summary_selected_fits$cAIC - min(summary_selected_fits$cAIC)

  summary_selected_fits$model <- dplyr::case_match(summary_selected_fits$model,
                                                   "best_fit" ~ "full model",
                                                   "best_fit_fix_only" ~ "fixed effects",
                                                   "best_fit_random_only" ~ "random effects",
                                                   "best_fit_fix_ID" ~ "fixed effects + individual ID",
                                                   "best_fit_fix_location" ~ "fixed effects + location ID",
                                                   "null_ID" ~ "individual ID",
                                                   "null_location" ~ "location ID",
                                                   "null" ~ "null model")

  summary_selected_fits$model <- factor(summary_selected_fits$model, levels = c("full model", "fixed effects",
                                                                                "random effects", "fixed effects + individual ID",
                                                                                "fixed effects + location ID", "individual ID",
                                                                                "location ID", "null model"))

  summary_selected_fits$group  <- factor(c("fixed+random", "fixed",
                                          "random", "fixed+random", "fixed+random",
                                          "random", "random", "null model"))

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

