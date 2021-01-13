birtm <- function(data, formula, prior = NULL, cores = 4, store = NULL, check_prior_influence = FALSE,
                  add_waic = TRUE, add_loo = TRUE,
                  variable_specifications = NULL, model_specifications = NULL, ...) {

  variable_specifications <- check_and_set_specifications(variable_specifications)
  model_specifications <- check_and_set_specifications(model_specifications)

  start <- Sys.time()
  fit <- brms::brm(formula = formula,
            data = data,
            prior = prior,
            sample_prior = check_prior_influence,
            save_pars = brms::save_pars(all = TRUE),
            cores = cores,
            file = store,
            ...
  )
  end <- Sys.time()
  fit$model_time <- end-start

  if (add_loo) fit <- brms::add_criterion(fit, 'loo')
  if (add_waic) fit <- brms::add_criterion(fit, 'waic')

  fit$var_specs <- variable_specifications
  fit$model_specs <-  model_specifications
  class(fit) <- c('birtmsfit', 'brmsfit')
  if (!is.null(store)) saveRDS(fit, file = glue::glue('{store}.rds'))

  return(fit)
}
