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
#' @param data the dataframe containing the data used for model fitting
#' @param fit a fitted model
#' @param formulas the dataframe of formulas created with `prepare_formulas()`
#' @param lat the latitude of a point
#' @param lat1 the latitude of point 1
#' @param lat2 the latitude of point 2
#' @param long the longitude of a point
#' @param long1 the longitude of point 1
#' @param long2 the longitude of point 2
#' @param ncpus the number of CPU cores to use for parallel processing (default = 2)
#' @param rawdata the data frame containing the raw data
#' @param threshold_distance_m a number specifying the maximum distance between two spatial points belonging to the same group
#' @param x a vector
NULL
