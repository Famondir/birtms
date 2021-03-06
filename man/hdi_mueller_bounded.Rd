% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/point_interval.R
\name{hdi_mueller_bounded}
\alias{hdi_mueller_bounded}
\title{HDI suitable also for bounded densities based on Mueller 94}
\usage{
hdi_mueller_bounded(x, .width = 0.95, na.rm = FALSE, allowSplit = TRUE)
}
\arguments{
\item{x}{numeric vector}

\item{.width}{double, ci width}

\item{na.rm}{boolean, should NA values be dropped}

\item{allowSplit}{boolean, should the HDI be returned (possibly contains multiple regions)}
}
\value{
matrix
}
\description{
HDI suitable also for bounded densities based on Mueller 94
}
