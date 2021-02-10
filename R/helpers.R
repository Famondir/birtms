#' Function to convert a list of dataframes to a 3D array
#' All objects in the list will be dataframes with identical column headings.
#' From Hannah Buckley (https://www.researchgate.net/publication/277670582_R_function_to_convert_a_list_of_dataframes_into_a_3-D_array)
#'
#' @param input.list List of dataframes
#'
#' @return three dimensional array (rows) x (cols) x (dataframenumber in list)
#' @export
#'
#' @examples
#' dat.ls <- vector("list",10) # empty list
#' for(i in 1:10) {
#' dat.ls[[i]] <- data.frame(xx=rnorm(25,3,2),yy=rnorm(25,2,1),zz=rnorm(25,2,1))
#' }  # fill list
#' dat.ary <- list2array(dat.ls)  # convert to ary
list2array = function(input.list){  #input a list of dataframes
  rows.cols <- dim(input.list[[1]])
  sheets <- length(input.list)

  output.array <- array(unlist(input.list), dim = c(rows.cols, sheets))

  colnames(output.array) <- colnames(input.list[[1]])
  row.names(output.array) <- row.names(input.list[[1]])

  return(output.array)    # output as a 3-D array
}

#' Samples from the Posterior Predictive Distribution in long format
#'
#' @param model brmsfit
#' @param n_samples integer
#'
#' @return tibble with columns from brmsfit$data (at least original responses, person and item identifier).
#' Additionally yrep (predicted answers) and .draw (which MCMC draw comes the answer from).
#' @export
#'
#' @examples
posterior_predict_long <- function(model, n_samples = NULL) {
  yrep <- posterior_predictive_values_long(model, n_samples, brms::posterior_predict) %>%
    dplyr::rename(yrep = ppv)

  return(yrep)
}

#' Expected Values of the Posterior Predictive Distribution in long format
#'
#' @param model brmsfit
#' @param n_samples integer
#'
#' @return tibble with columns from brmsfit$data (at least original responses, person and item identifier).
#' Additionally ppe (posterior predictive estimate) and .draw (which MCMC draw comes the answer from).
#' @export
#'
#' @examples
posterior_epred_long <- function(model, n_samples = NULL) {
ppe <- posterior_predictive_values_long(model, n_samples, brms::posterior_epred) %>%
  dplyr::rename(ppe = ppv)
  return(ppe)
}

#' Returns posterior predictive values in long format
#'
#' @param model brmsfit
#' @param n_samples integer
#' @param f function
#'
#' @return tibble with columns from brmsfit$data (at least original responses, person and item identifier).
#' Additionally y_rep (predicted answers) or ppe (posterior predictive estimate) and .draw (which MCMC draw comes the answer from).
#'
#' @examples
posterior_predictive_values_long <- function(model, n_samples = NULL, f) {
  draws <- NULL
  if (!is.null(n_samples)) draws <- sample(1:brms::nsamples(model), size = n_samples, replace = FALSE) %>% sort()

  message('Extracting posterior predictiv values')
  ppv <- f(model, subset = draws)

  message('Converting responses to long format')
  ppv <- ppv %>% t() %>% as.data.frame()
  if (!is.null(draws)) ppv <- ppv %>% stats::setNames(paste0('V', draws))
  ppv <- ppv  %>% cbind(model$data) %>%
    tidyr::pivot_longer(names_to = ".draw", names_prefix = 'V', names_transform = as.numeric(), values_to = 'ppv', cols = dplyr::starts_with("V"))

  return(ppv)
}

#' Repeat dataframe
#' Repeats rows in dataframe n times (where n is specified in a vector with equal entries).
#' Used to lengthen out odds ratio from original responses to match with or dataframe from post for faster substraction.
#' from Peter Solymos (mefa-package: https://www.rdocumentation.org/packages/mefa/versions/3.2-7/topics/rep.data.frame)
#'
#' @param x a matrix or data frame, but can be a vector.
#' @param ... arguments passed to the function rep, i.e. times, length.out and each (see explanation there).
#'
#' @return
#' @export
#'
#' @examples
rep_dataframe <-  function(x, ...) {
  as.data.frame(lapply(x, rep, ...))
}

#' Make response data wider
#' Transforms dataset from a brmsfit from long into wide format
#'
#' @param model brmsfit object
#'
#' @return tibble
#' @export
#'
#' @examples
make_data_wider <- function(model) {
  item <- model$var_specs$item
  response <- model$var_specs$response

  data_wide <- model$data %>% tidyr::pivot_wider(names_from = {{item}}, values_from = {{response}})

  return(data_wide)
}
