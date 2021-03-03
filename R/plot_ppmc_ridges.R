# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('quantile', 'xmin', 'xmax', 'ymin', 'ymax', 'ymax_spline', 'ymin_spline', 'points',
                                                        'density', 'ci_interval', 'ci_x', 'ci_y', 'x', 'y'))

#' Plot multiple PPMC parameter distributions
#' Deprecated!
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
  .Deprecated("plot_ppmc_distribution")

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
    ci <- brms::logit_scaled(utils::head(dots[[2]], -1)) / utils::tail(dots[[2]], 1)
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

#' Plot PPMC parameter distributions
#'
#' @param data numeric or single column from dataframe
#' @param method character; 'Mueller94' or 'SJexpanded'
#' @param ci_width double
#' @param density_ci boolean; should CI for density should be bootstrapped?
#' @param smooth_density_ci boolean; should CI be smoothed?
#' @param color color
#' @param n integer, number of points of desity estimation; can be smaller for Mueller94 (start with 128)
#' @param clean_data boolean; should infinite values and NaNs get removed?
#'
#' @return ggplot2 object
#' @export
#'
#' @examples
#' \dontrun{
#' or_data <- get_or(fit, n_samples = 500)
#' or_data %>% select(-or_rep_samples) %>% filter(item1 == 1, item2 == 2) %>%
#' birtsms::unnest_keep_attr(or_dif_samples) %>% select(or_dif)
#' birtsms::plot_ppmc_distribution()
#' }
plot_ppmc_distribution <- function(data, method = 'SJexpanded', ci_width = .89,
                      density_ci = FALSE, smooth_density_ci = FALSE,
                      color = 'lightblue', n = 512, clean_data = FALSE) {
  if (!(method %in% c('Mueller94', 'SJexpanded'))) stop('Method not defined.')

  density_boot <- function(d, w) {
    data <- d[w]
    dens <- data %>% get_density(boot = TRUE)

    return(list(density = dens$y, points = dens$x))
  }

  set_interval <- function(x, ci) {
    result <- rep(0, length(x))
    for (i in ci) {
      result <- ifelse(x > i, result + 1, result)
    }

    return(result)
  }

  get_grid <- function(dat, with_ci = TRUE) {
    grid <- seq(min(dat), max(dat), stepsize)

    if ( with_ci) {
      scale <- ceiling(log10(max(abs(dat))))
      scale <- ifelse(scale > 1, 10^scale, 10) # otherwise we will get a tie at "group_by(point) %>% summarise()" later

      grid <- sort(c(grid, ci-scale*.Machine$double.eps, ci+scale*.Machine$double.eps))
    }
    return(grid)
  }

  dens_mueller <- function(dat, boot = FALSE) {
    grid <- get_grid(dat, FALSE)
    dens <- dat %>% bde::bde(estimator = "boundarykernel", dataPointsCache = grid, lower.limit = min(data_org), upper.limit = max(data_org))

    if(boot) grid <- get_grid(data_org, !boot)
    else grid <- get_grid(dat, !boot)

    dens <- dens %>% density_converter(grid = grid)
    return(dens)
  }

  dens_sj <- function(dat, boot = FALSE) {
    dens <- dens_sj_bounded(dat, n, cut = FALSE)

    if(boot) grid <- get_grid(data_org, !boot)
    else grid <- get_grid(dat, !boot)

    a <- stats::approx(dens$x, dens$y, grid)
    dens$x <- a$x
    dens$y <- a$y
    return(dens)
  }

  median_call <- function(x) {
    warning('Some density points got zero variance while bootstrapping. Try reducing the number of density points n.')
    median(x)
  }

  if (method == 'Mueller94') {
    get_density <- dens_mueller
  } else if (method == 'SJexpanded') {
    get_density <- dens_sj
  }

  data <- data %>% as.numeric()
  if (clean_data) {
    data <- data[is.finite(data)]
    data <- data[!is.na(data)]
  }
  data_org <- data

  stepsize = (max(data)-min(data))/n
  ci <- NULL
  ci <- data %>% get_density() %>% HDInterval::hdi(allowSplit = TRUE) %>% as.numeric()

  dens <- get_density(data)

  ci_data <- tibble::tibble(ci_x = ci, ci_y = stats::approx(dens$x, dens$y, xout = ci)$y)
  dens_data <- tibble::tibble(x = dens$x, y = dens$y, ci_interval = as.factor(set_interval(x, ci)))

  g <-  dens_data %>% ggplot2::ggplot() + ggplot2::geom_area(aes(x = x, y = y, fill = ci_interval), color = 'black') +
    ggplot2::scale_fill_manual(values = c(rep(c("transparent", color),length(ci/2)), "transparent"), guide = "none")
  g <- g + ggplot2::geom_segment(data = ci_data, aes(x = ci_x, xend = ci_x, y = 0, yend = ci_y), linetype = 'dashed')

  if (density_ci) {
    density_draws <- rep(length(data), 1000) %>% purrr::map(.f = ~sample(.x, size = length(data), replace = TRUE)) %>%
      purrr::map_df(.f = ~density_boot(data, .x))
    birtms::aggregate_warnings({
      density_ci_data <- density_draws %>% dplyr::group_by(points) %>%
        dplyr::summarise(min = ifelse(!is.na(stats::sd(density)), ggdist::hdci(density)[1], median_call(density)),
                         max = ifelse(!is.na(stats::sd(density)), ggdist::hdci(density)[2], median_call(density))) # using hdci with SJ density throws error: sample is too sparse to find TD
    })

    if (smooth_density_ci) {
      g_temp <- ggplot2::ggplot() + ggformula::geom_spline(data = density_ci_data, aes(x = points, y= min), linetype = 'dashed') +
        ggformula::geom_spline(data = density_ci_data, aes(x = points, y= max), linetype = 'dashed')

      gg_data <- ggplot2::ggplot_build(g_temp)

      density_ci_splines <- gg_data$data[[1]][,1:2] %>% cbind(gg_data$data[[2]][,2]) %>% stats::setNames(c('x', 'ymin_spline', 'ymax_spline'))

      g <- g + ggplot2::geom_ribbon(data = density_ci_splines, aes(x = x, ymin = ymin_spline, max = ymax_spline), alpha = .25)
    } else {
      g <- g + ggplot2::geom_ribbon(data = density_ci_data, aes(x = points, ymin = min, max = max), alpha = .25)
    }
  }

  return(g)
}
