% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/personresponsefunction.R
\name{calc_personresponsedata}
\alias{calc_personresponsedata}
\title{Calculate person response data}
\usage{
calc_personresponsedata(model, post_responses, id = NULL)
}
\arguments{
\item{model}{birtmsfit object}

\item{post_responses}{data.frame from birtms::}

\item{id}{integer vector; persons to generate data for}
}
\value{
data.frame to use in birtms::plot_personresponsefunction
}
\description{
Calculate person response data
}
