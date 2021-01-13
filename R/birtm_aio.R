birtm_aio <- function(response_data, response_columns, person_data = NULL, item_data = NULL, situation_data = NULL,
                      variable_specifications = NULL, model_specifications = NULL,
                      prior = NULL, cores = 4, store = NULL, check_prior_influence = FALSE,
                      add_waic = TRUE, add_loo = TRUE, ...) {

  response_columns <- tryCatch(
    error = function(cnd) {
      response_columns <- rlang::enquo(response_columns)
    },
    {
      is.character(response_columns)
      response_columns <- response_columns
    }
  )
  test <- NULL

  data <- compose_dataset(response_data = response_data, response_columns = !!response_columns,
                          variable_specifications = variable_specifications,
                          person_data = person_data, item_data = item_data, situation_data = situation_data)
  formula <- build_formula(variable_specifications = variable_specifications, model_specifications = model_specifications)

  fit <- birtm(data = data, formula = formula, prior = prior, cores = cores, store = store,
               check_prior_influence = check_prior_influence, add_waic = add_waic, add_loo = TRUE,
               variable_specifications = variable_specifications, model_specifications = model_specifications, ...)

  return(fit)
}
