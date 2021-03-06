% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{list2array}
\alias{list2array}
\title{Function to convert a list of dataframes to a 3D array
All objects in the list will be dataframes with identical column headings.
From Hannah Buckley (https://www.researchgate.net/publication/277670582_R_function_to_convert_a_list_of_dataframes_into_a_3-D_array)}
\usage{
list2array(input.list)
}
\arguments{
\item{input.list}{List of dataframes}
}
\value{
three dimensional array (rows) x (cols) x (dataframenumber in list)
}
\description{
Function to convert a list of dataframes to a 3D array
All objects in the list will be dataframes with identical column headings.
From Hannah Buckley (https://www.researchgate.net/publication/277670582_R_function_to_convert_a_list_of_dataframes_into_a_3-D_array)
}
\examples{
dat.ls <- vector("list",10) # empty list
for(i in 1:10) {
dat.ls[[i]] <- data.frame(xx=rnorm(25,3,2),yy=rnorm(25,2,1),zz=rnorm(25,2,1))
}  # fill list
dat.ary <- list2array(dat.ls)  # convert to ary
}
