# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('quantile', 'xmin', 'xmax', 'ymin', 'ymax'))

#' Plot PPMC parameter distributions
#'
#' @param data tibble
#' @param parameter column name that holds numeric values to plot density ridges for
#' @param group column name to optionally specify that multiple density ridges should be plotted regarding the grouping value
#' @param rope boolean; should the rope be plotted as a gray area?
#' @param hdi_width double
#' @param color color; sets color of ridges
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' or_data <- get_or(fit, n_samples = 500)
#' or_data %>% select(-or_rep_samples) %>% filter(item1 == 1, item2 == 2) %>%
#' birtsms::unnest_keep_attr(or_dif_samples) %>%
#' birtsms::plot_ppmc_distribution(or_dif)
#'
#' or_data %>% select(-or_rep_samples) %>% filter(item1 == 1, item2 %in% 2:4) %>%
#' birtsms::unnest_keep_attr(or_dif_samples) %>%
#' birtsms::plot_ppmc_distribution(or_dif, group = item2, rope = TRUE)
#' }
plot_ppmc_ridges <- function (data, parameter, group = 0, rope = FALSE, hdi_width = .89, color = 'lightblue') {
  hdi_custWidth_internal <- function(...) {
    dots <- list(...)
    hdi_width <- dots[[2]]
    HDInterval::hdi(dots[[1]], credMass = hdi_width)
  }

  parameter <- rlang::ensym(parameter)

  if (rlang::enexpr(group) != 0) {
    group <- rlang::ensym(group)
    data <- data %>% dplyr::mutate({{group}} := as.factor({{group}})) # if group is specified multiple ridges get plotted
  }

  g <- data %>% ggplot2::ggplot(ggplot2::aes(x = {{parameter}}, y = {{group}}, fill = ggplot2::stat(quantile))) +
    ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi_custWidth_internal, quantiles = hdi_width, vline_linetype = 2) +
    ggplot2::scale_fill_manual(values = c("transparent", color, "transparent"), guide = "none")

  if (rope) {
    rect <- data.frame(xmin = 0 - attr(data, 'rope_width'), xmax= 0 + attr(data, 'rope_width'), ymin = -Inf, ymax = Inf)

    g <- g + ggplot2::geom_rect(data=rect, inherit.aes = FALSE, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      color = "transparent", fill = "grey50", alpha = 0.3)
  }

  return(g)
}
