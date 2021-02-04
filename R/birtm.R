#' Bayesian item response theory modeler
#'
#' Fits a bayesian IRT model.
#'
#' @param data Data in long format (i.e. composed with birtms::compose_data)
#' @param formula A brms::formula (i.e. builded with birtms::build_formula)
#' @param prior Bayesian priors in format of brms::prior
#' @param cores Number of CPU cores used to fit the model
#' @param file Path and name the fit object is saved for later reusage (will be loaded automatically if already exists)
#' @param check_prior_influence Boolean
#' @param preparing_bf Boolean
#' @param add_waic Boolean
#' @param add_loo Boolean
#' @param refit Boolean
#' @param variable_specifications A named list
#' @param model_specifications A named list
#' @param ... Arguments passed to brms::brm
#'
#' @return
#' @export
birtm <- function(data, formula, prior = NULL, cores = 4, file = NULL,
                  check_prior_influence = FALSE, preparing_bf = FALSE,
                  add_waic = TRUE, add_loo = TRUE, refit = FALSE,
                  variable_specifications = NULL, model_specifications = NULL, ...) {

  variable_specifications <- check_and_set_specifications(variable_specifications)
  model_specifications <- check_and_set_specifications(model_specifications)

  if (refit) {
    if (file.exists(glue::glue('{file}.rds'))) {
      file.remove(glue::glue('{file}.rds'))
    }
  }

  save_all_pars <- NULL
  if (preparing_bf) save_all_pars <- brms::save_pars(all = TRUE)

  start <- Sys.time()
  fit <- brms::brm(formula = formula,
            data = data,
            prior = prior,
            sample_prior = check_prior_influence,
            save_pars = save_all_pars,
            cores = cores,
            file = file,
            ...
  )
  end <- Sys.time()
  fit$model_time <- end-start

  message('Finished sampling')

  if(add_loo || add_waic) {
    start <- Sys.time()
    if (add_loo) {
      message('Adding loo')
      fit <- brms::add_criterion(fit, 'loo')
    }
    if (add_waic) {
      message('Adding waic')
      fit <- brms::add_criterion(fit, 'waic')
    }
    end <- Sys.time()
    fit$fitcriteria_time <- end-start
  }

  # fit$var_specs <- variable_specifications
  # fit$model_specs <-  model_specifications
  # class(fit) <- c('birtmsfit', 'brmsfit')
  # if (!is.null(file)) saveRDS(fit, file = glue::glue('{file}.rds'))

  fit <- new_birtmsfit(fit = fit, variable_specifications = variable_specifications, model_specifications = model_specifications, file = file)

  return(fit)
}
