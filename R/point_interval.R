
#' Get mode
#'
#' @param v vector
#' @param ... arguments passed to modeest::hsm()
#'
#' @return vector
#' @export
#'
#' @examples
#' v <- c(2, 2, 1)
#' get_mode(v)
get_mode <- function(v, ...) {
  uniqv <- unique(v)
  tab <- tabulate(match(v, uniqv))
  modus <- uniqv[which( tab == max(tab))]
  n_modes <- length(modus)

  if(is.numeric(v)) modus <- modeest::hsm(v, ...)

  if(n_modes != 1) {
    m <- paste0('Tie occured! There are ', n_modes, ' modes.')
    if(is.numeric(v)) m <- paste(m, 'Returning a mode within highest density region.\nSee modeest::hsm(). Use ... arguments to adjust behavior.')
    else m <- paste(m, 'Returning all modes.')
    warning(m)
  }

  return(modus)
}

#' Mode via hsm and HDI also for bounded densities
#' define a point_interval function using the hsm (half sample mode) estimator from modeest
#'
#' @param ... columns to get point_interval for
#' @param .data dataframe
#' @param .width double, ci width
#' @param inf.rm boolean, should infinite and NaN values be dropped
#' @param na.rm boolean, should NA values be dropped
#' @param tie.limit double
#'
#' @return dataframe
#' @export
#'
#' @examples
#' hsm_hdi_sjb(c(1,2,3,3,2.5))
hsm_hdi_sjb <- function(.data, ..., .width = .89, inf.rm = FALSE, na.rm = FALSE, tie.limit = .05) {
  hdi_sjb <- function(...) hdi_sj_bounded(..., allowSplit = TRUE)
  hsm <- function(...) modeest::hsm(..., tie.limit = tie.limit)

  dots <- rlang::enquos(...)

  if (inf.rm) .data <- .data[is.finite(.data)]

  ggdist::point_interval(.data, ..., .width = .width, .point = hsm, .interval = hdi_sjb, na.rm = na.rm)
}

#' Median and HDI also for bounded densities
#'
#' @param .data dataframe
#' @param ... columns to get point_interval for
#' @param .width double, ci width
#' @param inf.rm boolean, should infinite and NaN values be dropped
#' @param na.rm boolean, should NA values be dropped
#'
#' @return dataframe
#' @export
#'
#' @examples
#' #' median_hdi_sjb(c(1,2,3,3,2.5))
median_hdi_sjb <- function(.data, ..., .width = .89, inf.rm = FALSE, na.rm = FALSE) {
  hdi_sjb <- function(...) hdi_sj_bounded(..., allowSplit = TRUE)
  median <- function(...) stats::median(...)

  dots <- rlang::enquos(...)

  if (inf.rm) .data <- .data[is.finite(.data)]

  ggdist::point_interval(.data, ..., .width = .width, .point = median, .interval = hdi_sjb, na.rm = na.rm)
}

#' HDI suitable also for bounded densities based on SJ banswidth method
#'
#' @param x numeric vector
#' @param .width double, ci width
#' @param na.rm boolean, should NA values be dropped
#' @param allowSplit boolean, should the HDI be returned (possibly contains multiple regions)
#'
#' @return matrix
#' @export
#'
#' @examples
hdi_sj_bounded <- function(x, .width = 0.95, na.rm = FALSE, allowSplit = TRUE) {
  if (!na.rm && any(is.na(x))) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  intervals = HDInterval::hdi(dens_sj_bounded(x,na.rm = na.rm, cut = TRUE),
                              credMass = .width, allowSplit = allowSplit)

  if (nrow(intervals) == 1) {
    intervals = HDInterval::hdi(x, credMass = .width)
  }

  return(matrix(intervals, ncol = 2))
}

#' HDI suitable also for bounded densities based on Mueller 94
#'
#' @param x numeric vector
#' @param .width double, ci width
#' @param na.rm boolean, should NA values be dropped
#' @param allowSplit boolean, should the HDI be returned (possibly contains multiple regions)
#'
#' @return matrix
#' @export
#'
#' @examples
hdi_mueller_bounded <- function(x, .width = 0.95, na.rm = FALSE, allowSplit = TRUE) {
  if (!na.rm && any(is.na(x))) {
    return(matrix(c(NA_real_, NA_real_), ncol = 2))
  }

  intervals = HDInterval::hdi(dens_mueller_bounded(x, na.rm = na.rm),
                              credMass = .width, allowSplit = allowSplit)

  if (nrow(intervals) == 1) {
    intervals = HDInterval::hdi(x, credMass = .width)
  }

  return(matrix(intervals, ncol = 2))
}

#' Density with SJ-bandwidth method also suitable for bounded densities
#'
#' @param dat numeric vector
#' @param n integer, number of points where density gets evaluated
#' @param cut boolean, should only be FALSE for internal use
#' @param na.rm boolean
#'
#' @return S3 density
dens_sj_bounded <- function(dat, n = 512, cut = TRUE, na.rm = FALSE) {
  stepsize = (max(dat)-min(dat))/n
  grid <- seq(min(dat), max(dat), stepsize)
  n <- n*3

  rev_dat <- {-dat+max(dat)+min(dat)}
  rev_dat <- c(rev_dat+(max(rev_dat)-min(rev_dat)), rev_dat-(max(rev_dat)-min(rev_dat))) # max(rev_dat) = max(dat), analogous for min

  dens <- c(dat, rev_dat) %>% stats::density(n = n, bw="SJ", na.rm = na.rm, cut = 0) # not symmetrically around lower bound
  dens$y <- dens$y*3

  if (cut) {
    a <- stats::approx(dens$x, dens$y, grid)
    dens$x <- a$x
    dens$y <- a$y
    dens$n <- dens$n/3
  }

  return(dens)
}

dens_mueller_bounded <- function(dat, n = 128, na.rm = FALSE) {
  stepsize = (max(dat)-min(dat))/n
  grid <- seq(min(dat), max(dat), stepsize)

  if (na.rm) dat <- dat[is.finite(dat)]

  dens <- dat %>% bde::bde(estimator = "boundarykernel", dataPointsCache = grid, lower.limit = min(dat), upper.limit = max(dat))
  dens <- dens %>% density_converter(grid)
  return(dens)
}

density_converter <- function(density_S4, grid) {
  dens <- list(x = grid,
               y = bde::density(density_S4, grid),
               bw = bde::getb(density_S4),
               call = NULL,
               data.name = 'x',
               has.na = FALSE
  )
  class(dens) <- 'density'

  return(dens)
}
