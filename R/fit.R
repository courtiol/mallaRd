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
