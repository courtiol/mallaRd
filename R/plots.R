## General theme for all plots
theme1 <- ggplot2::theme(legend.position = "none",
                         axis.title.x = ggplot2::element_text(size = 12),
                         axis.title.y = ggplot2::element_text(size = 12),
                         axis.text.x = ggplot2::element_text(size = 11),
                         axis.text.y.left = ggplot2::element_text(),
                         axis.text.y = ggplot2::element_text(),
                         axis.line = ggplot2::element_line(colour = "black", size = 1),
                         panel.background = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_line(size = .01, colour = "gray"),
                         panel.grid.minor.y = ggplot2::element_blank(),
                         panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         plot.margin = ggplot2::margin(t = 1, r = 0.3, b = 0.3, l = 0.3, unit = "cm"))


theme2 <- ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_text(size = 12),
                         axis.text.y.left = ggplot2::element_text(size = 11),
                         axis.line = ggplot2::element_line(colour = "black", size = 1),
                         panel.background = ggplot2::element_blank(),
                         panel.grid.major.y = ggplot2::element_line(size = .01, colour = "gray"),
                         panel.grid.major.x = ggplot2::element_blank(),
                         panel.grid.minor.x = ggplot2::element_blank(),
                         legend.title = ggplot2::element_blank(),
                         legend.position = "top",
                         legend.background = ggplot2::element_blank(),
                         legend.key = ggplot2::element_blank())


globalVariables("density")


#' Build figure 1
#'
#' @inheritParams arguments
#'
#' @export
#'
#' @examples
#' figure1(data_all)
#'
figure1 <- function(data, ymax = c(0.15, 0.20, 0.01, 0.06, 0.025)) {

  data |>
    dplyr::filter(!is.na(.data$brood_size)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$brood_size)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black",
                            binwidth = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.data$brood_size)),
                        col = "red",
                        linewidth = 1.2) +
    ggplot2::scale_y_continuous(name = "Density", limits = c(0, ymax[1])) +
    ggplot2::scale_x_continuous(name = "Brood size", breaks = seq(1, 15, 2)) +
    theme1 -> p1

  data |>
    dplyr::filter(!is.na(.data$clutch_size)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$clutch_size)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black",
                            binwidth = 1) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.data$clutch_size)),
                        col = "red",
                        linewidth = 1.2) +
    ggplot2::scale_y_continuous(name = "Density", limits = c(0, ymax[2])) +
    ggplot2::scale_x_continuous(name = "Clutch size", breaks = seq(1, 17, 2)) +
    theme1 +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) -> p2

  data |>
    dplyr::filter(!is.na(.data$body_mass_g)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$body_mass_g)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black",
                            binwidth = 20) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.data$body_mass_g)),
                        col = "red",
                        linewidth = 1.2) +
    ggplot2::scale_y_continuous(name = "Density", limits = c(0, ymax[3])) +
    ggplot2::scale_x_continuous(name = "Body mass [g]", breaks = seq(400, 1200, 100)) +
    theme1 +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) -> p3

  data |>
    dplyr::filter(!is.na(.data$wing_length_mm)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$wing_length_mm)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black",
                            binwidth = 3) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.data$wing_length_mm)),
                        col = "red",
                        linewidth = 1.2) +
    ggplot2::scale_y_continuous(name = "Density",limits = c(0, ymax[4])) +
    ggplot2::scale_x_continuous(name = "Wing length [mm]", breaks = seq(220, 300, 10)) +
    theme1 -> p4

  data |>
    dplyr::filter(!is.na(.data$hatch_doy)) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$hatch_doy)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                            colour = "black",
                            binwidth = 5) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = mean(.data$hatch_doy)),
                        col = "red",
                        linewidth = 1.2) +
    ggplot2::scale_y_continuous(name = "Density", limits = c(0, ymax[5])) +
    ggplot2::scale_x_continuous(name = "Day of the year at hatching", breaks = seq(60, 210, 30)) +
    theme1 +
    ggplot2::theme(axis.title.y = ggplot2::element_blank()) -> p5

  p <- cowplot::plot_grid(p1,
                     p2,
                     p3,
                     p4,
                     p5,
                     labels = "AUTO",
                     nrow = 2,
                     hjust = -1)
  print(p)

}


#' Build figure 2
#'
#' @inheritParams arguments
#'
#' @export
#'
#' @examples
#' figure2(data_all)
#'
figure2 <- function(data) {

  data |>
    dplyr::select("year", "habitat_type") |>
    dplyr::summarise(n = dplyr::n(), .by = c("year", "habitat_type")) |>
    dplyr::mutate(prop = (.data$n / sum(.data$n)) * 100, .by = "year") |>
    dplyr::mutate(n_year = sum(.data$n), .by = "year") |>
    ggplot2::ggplot(ggplot2::aes(.data$year, .data$prop / 100, colour = .data$habitat_type, label = .data$habitat_type)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_point(mapping = ggplot2::aes(size = n)) +
    ggplot2::labs(x = "year", y = "Proportion") +
    ggplot2::scale_colour_viridis_d(
      direction = -1,
      breaks = c("roof_terrace", "balcony", "courtyard",  "other", "unknown"),
      labels = c("roof terrace", "balcony", "courtyard",  "other", "unknown")
    ) +
    ggplot2::scale_x_continuous(breaks = seq(2005, 2020, 1)) +
    ggplot2::scale_y_continuous(breaks = seq(0, 0.55, 0.05), limits = c(0, 0.55)) +
    ggplot2::guides(size = ggplot2::guide_legend(override.aes = list(colour = "gray31"))) +
    theme2 +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 11)) -> p1

  #figure for comparing Hallau & Otto 2005 with our data
  N <- nrow(data)

  data |>
    dplyr::summarise(n = dplyr::n(), .by = "habitat_type") |>
    dplyr::mutate(class = paste0("this study (n = ", N, ")"),
                  prop = (.data$n / sum(.data$n))) |>
    dplyr::select(-"n") |>
    rbind(data.frame(
      habitat_type = c("roof_terrace", "balcony", "courtyard",  "other", "unknown"),
      class = rep("Hallau & Otto 2005 (n = 890)", times = 5),
      prop = c(.29, .35, .18, .15, .03)
    )) |>
    dplyr::mutate(
      class = factor(
        class,
        levels = c(paste0("this study (n = ", N,")"), "Hallau & Otto 2005 (n = 890)")
      ),
      habitat_type = factor(
        .data$habitat_type,
        levels = c("roof_terrace", "balcony", "courtyard",  "other", "unknown")
      )
    ) |>
    ggplot2::ggplot(ggplot2::aes(.data$habitat_type, .data$prop, fill = .data$class)) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 0.7,
      position = ggplot2::position_dodge2(width = 0.1),
      colour = "black"
    ) +
    ggplot2::scale_x_discrete(
      breaks = c("roof_terrace", "balcony", "courtyard",  "other", "unknown"),
      labels = c("roof terrace", "balcony", "courtyard",  "other", "unknown")
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 0.35), breaks = seq(0, 0.35, 0.05)) +
    ggplot2::labs(x = "Type of breeding site", y = "Proportion") +
    theme2 +
    ggplot2::scale_fill_manual(values = c("darkred", "darkgrey")) -> p2

  print(cowplot::plot_grid(p1, p2, labels = "AUTO", nrow = 2))
}


#' Build figure 3
#'
#' @inheritParams arguments
#'
#' @export
#'
#' @examples
#' figure3(data_all, data_model)
#'
figure3 <- function(data, data_model) {

  data |>
    dplyr::filter(!is.na(.data$ring_number)) |>
    dplyr::summarise(n = dplyr::n(), .by = "ring_number") |>
    ggplot2::ggplot(ggplot2::aes(.data$n)) +
    ggplot2::geom_bar(colour = "black") +
    ggplot2::labs(x = "Number of breeding events per individual", y = "Frequency") +
    theme1 +
    ggplot2::scale_x_continuous(limits = c(0,14), breaks = seq(1, 13, 1)) +
    ggplot2::scale_y_continuous(limits = c(0,600), breaks = seq(0, 600, 100)) -> p1

  data_model |>
    dplyr::summarise(Nreturns = dplyr::n(), .by = c("individual_ID", "return")) -> data_returns

  data_returns |>
    dplyr::filter(!.data$return) |>
    dplyr::summarise(Nducks = sum(.data$Nreturns)) |>
    dplyr::mutate(Nreturns = 0, .before = 1) -> no_returns

  data_returns |>
    dplyr::filter(.data$return) |>
    dplyr::count(.data$Nreturns, name = "Nducks") -> yes_returns

  returns <- rbind(no_returns, yes_returns)

  returns |>
    ggplot2::ggplot(ggplot2::aes(x = .data$Nreturns, y = .data$Nducks)) +
    ggplot2::geom_col(fill = "darkred", colour = "black") +
    ggplot2::labs(x = "Number of returns per individual", y = "Frequency") +
    theme1 +
    ggplot2::scale_x_continuous(limits = c(-0.5, 12), breaks = seq(0, 12, 1)) +
    ggplot2::scale_y_continuous(limits = c(0, 120), breaks = seq(0, 120, 20)) -> p2

  print(cowplot::plot_grid(p1, p2, labels = "AUTO", nrow = 1))
}


#' Build components of figure 4
#'
#' @inheritParams arguments
#'
#' @export
#'
#' @examples
#' subfigure4(data_model, fit = best_fit,
#'            predictor = "relocation_distance_previous_z",
#'            xlab = "Relocation distance (m)")
#'
#' subfigure4(data_model, fit = best_fit,
#'            predictor = "relocation_distance_previous_z",
#'            xlab = "Relocation distance (km)",
#'            fn = \(x) x/1000)
#'
subfigure4 <- function(data_model, fit, predictor,  fn = identity, xlab = "", ...) {
  pred <- spaMM::pdep_effects(fit, predictor, length.out = 100, ...)
  pred$focal_var <- fn(pred$focal_var * attr(data_model[[predictor]], "sd") + attr(data_model[[predictor]], "mean"))

  predictor_mean <- fn(attr(data_model[[predictor]], "mean"))
  pred_at_mean <- spaMM::pdep_effects(fit, predictor, focal_values = 0, ...)[["pointp"]]

  ggplot2::ggplot(pred) +
    ggplot2::aes(x = .data$focal_var, y = .data$pointp) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$low, ymax = .data$up), fill = "lightgrey") +
    ggplot2::geom_line(colour = "darkred", size = 1.3) +
    ggplot2::geom_segment(x = -1e6, y = pred_at_mean, xend = predictor_mean, yend = pred_at_mean, linetype = "dashed") +
    ggplot2::geom_segment(x = predictor_mean, y = pred_at_mean, xend = predictor_mean, yend = 0, linetype = "dashed") +
    theme1 +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0), "cm")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name = "\n P(return)", expand = c(0, 0)) +
    ggplot2::scale_x_continuous(name = xlab)


}


#' Build figure 4
#'
#' @inheritParams arguments
#'
#' @export
#'
#' @examples
#' figure4(data_model, best_fit)
#'
figure4 <- function(data_model, fit, ...) {
  p1 <- subfigure4(data_model, fit = best_fit,
                   predictor = "populationdensity500_previous_z",
                   xlab = "Human pop density (in/ha)", ...)
  p2 <- subfigure4(data_model, fit = best_fit,
                   predictor = "relocation_distance_previous_z",
                   xlab = "Relocation distance (km)",
                   fn = \(x) x/1000, ...)

  pred <- spaMM::pdep_effects(fit, "delta_season", ...)

  p3 <- ggplot2::ggplot(pred) +
    ggplot2::aes(x = .data$focal_var, y = .data$pointp) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$low, ymax = .data$up), colour = "darkgrey", width = 0.1) +
    ggplot2::geom_point(colour = "darkred", fill = NA, size = 3) +
    theme1 +
    ggplot2::theme(plot.margin = ggplot2::unit(c(0.5, 0.5, 0.5, 0), "cm")) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), name = "\n P(return)", expand = c(0, 0)) +
    ggplot2::scale_x_discrete(name = "Breeding season", labels = c("successive", "same"))

  p <- cowplot::plot_grid(p1, p2, p3, labels = "AUTO", nrow = 1)
  print(p)
}

#' Build figure 5
#'
#' @inheritParams arguments
#'
#' @export
#'
#' @examples
#' figure5(best_fit)
#'
figure5 <- function(best_fit) {

  summary_selected_fits <- summarize_fit(best_fit, pretty = FALSE)

  summary_selected_fits |>
    ggplot2::ggplot(ggplot2::aes(.data$model, .data$delta_cAIC, fill = .data$group)) +
    ggplot2::geom_col(colour = "black") +
    ggplot2::scale_y_continuous(minor_breaks = NULL, breaks = seq(0, 300, 5)) +
    theme2 +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 1),
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = c("darkred", "black", "grey", "white"),
                               limits = c('fixed+random', 'fixed', 'random', 'null model'), name = "model structure") +
    ggplot2::labs(x = NULL, y = "ΔcAIC") -> plot_delta_cAIC

  summary_selected_fits |>
    ggplot2::ggplot(ggplot2::aes(.data$model, .data$delta_mAIC, fill = .data$group)) +
    ggplot2::geom_col(colour = "black") +
    ggplot2::scale_y_continuous(minor_breaks = NULL, breaks = seq(0, 20, 5)) +
    theme2 +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 1),
                   legend.position = "none",
                   axis.text.x = ggplot2::element_blank()) +
    ggplot2::scale_fill_manual(values = c("darkred", "black", "grey", "white"),
                               limits = c('fixed+random', 'fixed', 'random', 'null model'),
                               name = "model structure") +
    ggplot2::labs(x = NULL, y = "ΔmAIC") -> plot_delta_mAIC

  summary_selected_fits |>
    ggplot2::ggplot(ggplot2::aes(.data$model, .data$TjursD, fill = .data$group)) +
    ggplot2::geom_col(colour = "black") +
    ggplot2::scale_y_continuous(minor_breaks = NULL, breaks = seq(0, 0.4, 0.05)) +
    theme2 +
    ggplot2::theme(panel.border = ggplot2::element_rect(fill = NA, size = 1),
                   legend.position = "none") +
    ggplot2::scale_fill_manual(values = c("darkred", "black", "grey", "white"),
                               limits = c('fixed+random', 'fixed', 'random'), name = "model structure") +
    ggplot2::labs(x = NULL, y = "Tjur's D") -> plot_TjursD


  grid::grid.draw(cbind(rbind(ggplot2::ggplotGrob(plot_delta_cAIC),
                              ggplot2::ggplotGrob(plot_delta_mAIC),
                              ggplot2::ggplotGrob(plot_TjursD),
                              size = "last")))

  #cowplot::plot_grid(plot_delta_cAIC, plot_delta_mAIC, plot_TjursD,
  #                   labels = "AUTO", ncol = 1, axis = "l")
}



