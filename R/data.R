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

#' The cleaned dataset of all breeding events for identified birds
#'
#' This dataset contains the raw data after reformatting the variables and after discarding the unknown birds.
#' It derives from `data_all` as shown in `?mallaRd`.
#'
#' @name data_all_known
#' @docType data
#' @format a `tibble`
#' @references this study
#' @keywords datasets
#' @seealso [data_raw], [data_model]
#' @examples
#' data_all_known
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

#' The table of fitted the models
#'
#' This tibble contains the models fitted with the function [fit()] as shown in `?mallaRd`.
#'
#' @name fitting_results
#' @docType data
#' @format a `tibble`
#' @references this study
#' @keywords datasets
#' @seealso [fit()]
#' @examples
#' fitting_results
#'
NULL

#' The fit of the full (best) model
#'
#' This object contains the model fitted with the function [fit()] as shown in `?mallaRd`.
#'
#' @name best_fit
#' @docType data
#' @format a `HLfit` object produced by spaMM
#' @references this study
#' @keywords model
#' @seealso [fit()]
#' @examples
#' best_fit
#'
NULL

#' The comparison of alternative fits
#'
#' This object contains the output with the function [compare_fits()] as shown in `?mallaRd`.
#'
#' @name fit_comparison
#' @docType data
#' @format a `tibble` object produced by [compare_fits()]
#' @references this study
#' @keywords data
#' @seealso [compare_fits()]
#' @examples
#' fit_comparison
#'
NULL
