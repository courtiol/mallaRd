#' The raw dataset of all breeding events
#'
#' This dataset contains the raw data, unprocessed.
#'
#' @name data_raw
#' @docType data
#' @format a `data.frame`
#' @references this study
#' @keywords datasets
#' @seealso [data_all], [data_model]
#' @examples
#' data_raw
#'
NULL

#' The cleaned dataset of all breeding events
#'
#' This dataset contains the raw data after reformatting the variables but without any filtering. It
#' derives from `data_raw` as shown in `?mallaRd`.
#'
#' @name data_all
#' @docType data
#' @format a `tibble`
#' @references this study
#' @keywords datasets
#' @seealso [data_raw], [data_model]
#' @examples
#' data_all
#'
NULL


#' The dataset used for fitting the models
#'
#' This dataset contains the data used to fit all the models. It derives from `data_all` as shown in
#' `?mallaRd`.
#'
#' @name data_model
#' @docType data
#' @format a `tibble`
#' @references this study
#' @keywords datasets
#' @seealso [data_raw], [data_all]
#' @examples
#' data_model
#'
NULL

