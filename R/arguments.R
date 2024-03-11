#' Definition of the function arguments
#'
#' Here is the list of most function arguments used in the package.
#' The arguments not contained in this list are those for which the exact meaning depends on the context.
#'
#' @name arguments
#'
######################################################################################################################
# Reminder for developers: sort by alphabetical order, single line, use points as delimiter, no cap, no final point. #
######################################################################################################################
#'
#' @param best_fit the fit of the model with the best predictive power
#' @param boot.repl the number of bootstrap replicates
#' @param data the dataframe containing the data used by the function
#' @param data_model the dataframe containing the data used for model fitting
#' @param fit a fitted model
#' @param fit_null a fitted model more simple than the other fit it is compared to
#' @param formulas the dataframe of formulas created with `prepare_formulas()`
#' @param lat the latitude of a point
#' @param lat1 the latitude of point 1
#' @param lat2 the latitude of point 2
#' @param long the longitude of a point
#' @param long1 the longitude of point 1
#' @param long2 the longitude of point 2
#' @param ncpus the number of CPU cores to use for parallel processing (default = 2)
#' @param predictor the name of the focal predictor
#' @param rawdata the data frame containing the raw data
#' @param seed the seed to be used for bootstrapping
#' @param threshold_distance_m a number specifying the maximum distance between two spatial points belonging to the same group
#' @param x a vector
#' @param xlab the name for the x-axis
#' @param ymax a vector defining the maximal values along the y axis (plot per plot)
NULL
