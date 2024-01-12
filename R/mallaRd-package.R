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
#'
#' rawdata <- read.csv(system.file("extdata/raw_data.csv", package = "mallaRd"))
#' head(rawdata)
#'
#'
#' ## Formatting the data
#'
#' rawdata |>
#'   as_tibble() |>
#'   mutate(date = as.Date(.data$date, format = c("%d.%m.%Y")),
#'          ring_number = ifelse(.data$ring_number == "", NA, .data$ring_number)) |>
#'   mutate(across(c("PSW1000", "PSW2000"), \(x) factor(ifelse(x == 0, "no", "yes")))) |>
#'   mutate(across(where(is.character), as.factor)) -> duck_all
#'
#'
#' ## Summary statistics for the full dataset
#'
#' ### total number of breeding events
#' nrow(duck_all)
#' # [1] 1732
#'
#' ### breeding events from identified (ringed)  ducks
#' sum(!is.na(duck_all$ring_number))
#' # [1] 1211
#'
#' ### total number of identified (ringed) ducks
#' length(levels(duck_all$ring_number))
#' # [1] 810
#'
#' ### total number of years
#' length(unique(duck_all$year))
#' # [1] 16
#'
#' ### range of years
#' range(duck_all$year)
#' # [1] 2005 2020
#'
#' ### breeding events for which floor level known
#' sum(!is.na(duck_all$floor_level))
#' # [1] 100
#'
#' ### pct of breeding events for which floor level known
#' round(100*(sum(!is.na(duck_all$floor_level))/nrow(duck_all)))
#' # [1] 6
#'
#'
#' ## Reformatting the data for statistical modelling retaining only observations for which all
#' ## predictors used for modelling are available
#'
#' data_model <- prepare_data(duck_all)
#' # Information on data filtering
#' # # A tibble: 8 × 7
#' #   description                                             breeding_events_removed remaining_events delta_events_pct ducks_removed remaining_ducks delta_duck_pct
#' #   <chr>                                                                     <dbl>            <int>            <dbl>         <dbl>           <int>          <dbl>
#' # 1 raw data                                                                      0             1732            0                 0             810          0
#' # 2 removed because unknown ducks                                               521             1211           30.1               0             810          0
#' # 3 removed because only observed once                                          582              629           33.6             582             228         71.9
#' # 4 removed because of reshaping the data                                         0              629            0                 0             228          0
#' # 5 removed because first occurrence per duck not used                          228              401           13.2               0             228          0
#' # 6 removed because recapture events more than 1 year apart                      91              310            5.25             42             186          5.19
#' # 7 removed because of missing values for predictors                             10              300            0.577             6             180          0.741
#' # 8 final number of rows                                                        300              300           17.3               0             180          0
#'
#' ### total number of breeding events (nrow + 1/duck)
#' nrow(data_model) + length(unique(data_model$ring_number))
#' # [1] 480
#'
#' ### total number of recapture events
#' nrow(data_model)
#' # [1] 300
#'
#' ### total number of ducks complete dataset
#' length(unique(data_model$ring_number))
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
#' # [1] -0.10731  0.65026
#'
#'
#' ## Prepare formulas for model fits
#'
#' formula_df <- prepare_formula()
#'
#' ### number of different models to fit
#' nrow(formula_df)
#' #[1] 4096
#'
"_PACKAGE"
