# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c("Intercept"))

#' R.squared
#'
#' Computes R.squared for non hierarchical models with item and person covariates. (Hierarchical models not tested right now.)
#'
#' @param birtms_fit Object of type birtmsfit.
#' @param fast Boolean. If true observations get summarised by persons or items. Should work also for datasets with missings and different observation numbers per person. Attention: It's not tested if the results are the same for datasets when situationcovars are added!
#'
#' @return List with R.squared and sd value for each dimension for persons and items.
#' @importFrom rlang :=
#' @export
R2_latent <- function(birtms_fit, fast = TRUE) {
  if (is.birtmsfit(birtms_fit)) fit <- birtms_fit
  else stop('Object is not of type birtms_fit. Therefore var_specs element might be missing. Execution terminated.')
  if (fast & !is.null(fit$var_specs$situation_covariables)) warning('Model used situation covariables but fast argument is TRUE. Please set fast to FALSE to get more reliable estimates.')

  var_cor <- brms::VarCorr(fit, summary = FALSE)
  beta_all <- brms::fixef(fit, summary = FALSE)

  person <- fit$var_specs$person
  item <- fit$var_specs$item

  person_covars <- fit$var_specs[stringr::str_detect(names(fit$var_specs), 'person_covariables') | stringr::str_detect(names(fit$var_specs), 'situation_covariables')] %>% unlist(use.names = FALSE)
  item_covars <- fit$var_specs[stringr::str_detect(names(fit$var_specs), 'item_covariables') | stringr::str_detect(names(fit$var_specs), 'situation_covariables')] %>% unlist(use.names = FALSE)

  beta_person <- beta_all %>% tibble::as_tibble() %>% dplyr::select(Intercept, purrr::map(person_covars, tidyselect::starts_with, vars = colnames(.data)) %>% unlist()) %>% as.matrix()
  Y_person <- tibble::tibble({{person}} := fit$data[[person]]) %>% dplyr::mutate(as.data.frame(brms::make_standata(data = fit$data, formula = fit$formula)[['X']]))
  if (fast) Y_person <- Y_person %>% dplyr::group_by_(person) %>% dplyr::summarise_all(~ median(as.numeric(.x))) %>% dplyr::ungroup()
  Y_person <- Y_person %>% dplyr::select(-{{person}}) %>% dplyr::select(colnames(beta_person))

  beta_item <- beta_all %>% tibble::as_tibble() %>% dplyr::select(Intercept, purrr::map(item_covars, tidyselect::starts_with, vars = colnames(.data)) %>% unlist()) %>% as.matrix()
  Y_item <- tibble::tibble({{item}} := fit$data[[item]]) %>% dplyr::mutate(as.data.frame(brms::make_standata(data = fit$data, formula = fit$formula)[['X']]))
  if (fast) Y_item <- Y_item %>% dplyr::group_by_(item) %>% dplyr::summarise_all(~ median(as.numeric(.x))) %>% dplyr::ungroup()
  Y_item <- Y_item %>% dplyr::select(-!!item) %>% dplyr::select(colnames(beta_item))

  variance_person <- (var_cor[[person]]$sd %>% as.data.frame())^2
  variance_item <- (var_cor[[item]]$sd %>% as.data.frame())^2

  R2 <- list()
  for (i in 1:ncol(variance_person)) {
    name <- glue::glue('{person}.{colnames(variance_person)[i]}')
    R2[[name]] <- calc_latent_regression_coefs_distribution(variance_person[i], beta_person, Y_person)
  }

  for (i in 1:ncol(variance_item)) {
    name <- glue::glue('{item}.{colnames(variance_item)[i]}')
    R2[[name]] <- calc_latent_regression_coefs_distribution(variance_item[i], beta_item, Y_item)
  }

  return(R2)
}

#' Calculates distribution of latent regression coefficients
#'
#' @param variance dataframe of variances. Column for each dimension. Row per posteriorsample draw.
#' @param beta dataframe of regressioncoefficients. Column for each predictor. Row per posteriorsample draw.
#' @param Y dataframe of regression indicators. Column for each predictor. Row per observation.
#'
#' @return List with R.squared and sd median_hdi summary.
calc_latent_regression_coefs_distribution <- function(variance, beta, Y) {
  R2_vec <- rep(NA, nrow(beta))
  sd_vec <- rep(NA, nrow(beta))

  for (i in seq_along(beta[,1])) {
    temp <- calc_latent_regression_coefs_point_estimator(variance[i,] %>% as.numeric(), beta[i,] %>% as.numeric(), Y %>% as.matrix())
    R2_vec[i] <- temp$R2_theta
    sd_vec[i] <- temp$sd_theta
  }

  if(nrow(beta) > 1) return(list(R2 = tidybayes::median_hdi(R2_vec), sd_theta = tidybayes::median_hdi(sd_vec)))
  return(list(R2_theta = R2_vec, sd_theta = sd_vec))
}

#' Calculates point estimate of latent regression coefficients
#'
#' @param variance numeric value
#' @param beta numeric vector
#' @param Y numeric matrix
#'
#' @return List of R.quared and sd value.
calc_latent_regression_coefs_point_estimator <- function(variance, beta, Y) # extracted from tam_latent_regression_standardized_solution
{
  Y_exp <- Y %*% beta
  var_y_exp <- stats::var( Y_exp )

  sd_theta <- sqrt( var_y_exp + variance )
  R2_theta <- var_y_exp / sd_theta^2

  #--- output
  res <- list(R2_theta=R2_theta, sd_theta=sd_theta)

  return(res)
}
