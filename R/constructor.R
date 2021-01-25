new_birtms <- function(fit, variable_specifications = NULL, model_specifications = NULL, file = NULL) {
  stopifnot(brms::is.brmsfit(fit))

  variable_specifications <- check_and_set_specifications(variable_specifications)
  model_specifications <- check_and_set_specifications(model_specifications)

  fit$var_specs <- variable_specifications
  fit$model_specs <-  model_specifications
  class(fit) <- c('birtmsfit', 'brmsfit')

  if (!is.null(file)) saveRDS(fit, file = glue::glue('{file}.rds'))

  invisible(fit)
}

is.birtmsfit <- function(fit) inherits(fit, "birtmsfit")
