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
#' @param color color; sets color of ridges
#' @param range double; c(min, max) used for setting xlim in coord_cartesian()
#' @param ... arguments passed to ggridges::stat_density_ridges()
#' @param hdi boolean; should HDI or equitailed region be plotted
#' @param ci_width double
#' @param custom_ci double vector to pass the ci limits manually (e.g. when )
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
plot_ppmc_ridges <- function (data, parameter, group = 0, rope = FALSE, hdi = TRUE, ci_width = .89, color = 'lightblue', range = NULL, custom_ci = NULL, ...) {
  hdi_custWidth_internal <- function(...) {
    dots <- list(...)
    hdi_width <- dots[[2]]
    # hdi <- HDInterval::hdi(dots[[1]], credMass = hdi_width, allowSplit = TRUE) # does not split hdi!?
    hdi2 <- tidybayes::mode_hdi(dots[[1]], .width = hdi_width) %>% dplyr::select(ymin, ymax) %>% dplyr::rename(lower = ymin, upper = ymax) %>%
      unlist() %>% sort()

    return(hdi2)
  }

  custom_lims <- function(...) {
    dots <- list(...)
    ci <- brms::logit_scaled(head(dots[[2]], -1)) / tail(dots[[2]], 1)
    ci <- sort(ci)

    return(ci)
  }

  calc_custom_lims <- function(x) {
    if(max(abs(x)) > 1) {
      times <- 1/10^floor(log10(max(abs(x))))
    } else times <- 1

    custom_ci <- (x*times) %>% sort() %>% brms::inv_logit_scaled()

    return(c(custom_ci, times))
  }

  if(!is.null(custom_ci)) {
    ci_width <- calc_custom_lims(custom_ci)
    qf <- function(...) custom_lims(...)
  } else if(hdi) {
    qf <- function(...) hdi_custWidth_internal(...)
  } else {
    qf <- function(...) quantile(...)
    ci_width <- c(.5-ci_width/2, .5+ci_width/2)
  }

  parameter <- rlang::ensym(parameter)

  if (rlang::enexpr(group) != 0) {
    group <- rlang::ensym(group)
    data <- data %>% dplyr::mutate({{group}} := as.factor({{group}})) # if group is specified multiple ridges get plotted
  }

  g <- data %>% ggplot2::ggplot(ggplot2::aes(x = {{parameter}}, y = {{group}}, fill = ggplot2::after_stat(quantile))) + # ggplot2::after_stat instead of sta
    ggridges::stat_density_ridges(geom = 'density_ridges_gradient', ...,
                                  quantile_lines = TRUE, quantile_fun = qf, quantiles = ci_width, vline_linetype = 2) +
    ggplot2::scale_fill_manual(values = c(rep(c("transparent", color),3), "transparent"), guide = "none")

  if (rope) {
    rect <- data.frame(xmin = 0 - attr(data, 'rope_width'), xmax= 0 + attr(data, 'rope_width'), ymin = -Inf, ymax = Inf)

    g <- g + ggplot2::geom_rect(data=rect, inherit.aes = FALSE, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                      color = "transparent", fill = "grey50", alpha = 0.3)
  }

  if(!is.null(range)) {
    g <- g + ggplot2::coord_cartesian(xlim = range)
  }

  return(g)
}
