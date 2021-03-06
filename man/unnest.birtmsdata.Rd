% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/birtsmsdata.R
\name{unnest.birtmsdata}
\alias{unnest.birtmsdata}
\title{Unnest for birtmsdata
Preserves additional attributes of birtmsdata objects.}
\usage{
\method{unnest}{birtmsdata}(data, ...)
}
\arguments{
\item{data}{data frame}

\item{...}{see tidyr::unnest()}
}
\value{
birtmsdata
}
\description{
Unnest for birtmsdata
Preserves additional attributes of birtmsdata objects.
}
\examples{
\dontrun{
#' or_data <- get_or(fit, n_samples = 500)
or_data \%>\% unnest(or_dif_samples)
}
}
