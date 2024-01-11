#' mallaRd: Breeding output and breeding site fidelity of mallards in Berlin, Germany
#'
#' This help page reproduces, in the section **Examples** the analyses and results from the paper
#' "Housing search in the concrete jungle – breeding site selection of urban mallards and
#' implications for conservation management strategies" by Engler et al.
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
#' ## Loading the raw data
#' rawdata <- read.csv(system.file("extdata/raw_data.csv", package = "mallaRd"))
#' head(rawdata)
#'
#' ## Formatting the data
#' rawdata |>
#'   mutate(date = as.Date(.data$date, format = c("%d.%m.%Y"))) |>
#'   mutate(across(where(is.character), as.factor)) -> duck
#'
#' str(duck)
#'
"_PACKAGE"