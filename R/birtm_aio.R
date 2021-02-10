#' All-in-one bayesian item response theory modeler
#'
#' A wrapper function to fit a bayesian IRT model with one command.
#'
#' @param response_data A tibble or dataframe with person identifiers and responsdata (and possibly person covariables).
#' @param response_columns A character or symbol vector with all columns that represent item responses or a range of columns specified corresponding to dplyr::select() rules (e. g.: item01:item25).
#' @param person_data A tibble or dataframe with person identifiers linked to person covariables.
#' @param item_data A tibble or dataframe with item identifiers linked to item covariables.
#' @param situation_data A tibble or dataframe with person and item identifiers linked to situation covariables.
#' @param variable_specifications Named list of characters or strings.
#' @param model_specifications Named list of strings and numerics.
#' @param prior Bayesian priors in format of brms::prior
#' @param cores Number of CPU cores used to fit the model
#' @param file Path and name the fit object is saved for later reusage (will be loaded automatically if already exists)
#' @param check_prior_influence Boolean
#' @param preparing_bf Booleans
#' @param add_waic Boolean
#' @param add_loo Boolean
#' @param refit Boolean
#' @param ... Arguments passed to brms::brm
#'
#' @return birtsmsfit object
#' @export
#'
#' @examples
#' \dontrun{
#' testdata <- data_spm[1:100,] # take only 1/5th of the data to speed up the examplary model fitting
#' fit_1d_1pl_spm2 <- birtms::birtm_aio(response_data = testdata, response_columns = i1:i12,
#' file = '../inst/extdata/fit_1d_1pl_spm2')
#' fit_1d_1pl_spm2
#' }
birtm_aio <- function(response_data, response_columns, person_data = NULL, item_data = NULL, situation_data = NULL,
                      variable_specifications = NULL, model_specifications = NULL,
                      prior = NULL, cores = 4, file = NULL,
                      check_prior_influence = FALSE, preparing_bf = FALSE,
                      add_waic = TRUE, add_loo = TRUE, refit = FALSE, ...) {

  response_columns <- tryCatch(
    error = function(cnd) {
      response_columns <- rlang::enquo(response_columns)
    },
    {
      is.character(response_columns)
      response_columns <- response_columns
    }
  )

  data <- compose_dataset(response_data = response_data, response_columns = !!response_columns,
                          variable_specifications = variable_specifications,
                          person_data = person_data, item_data = item_data, situation_data = situation_data)
  formula <- build_formula(variable_specifications = variable_specifications, model_specifications = model_specifications)

  fit <- birtm(data = data, formula = formula, prior = prior, cores = cores, file = file, refit = refit,
               check_prior_influence = check_prior_influence, preparing_bf = preparing_bf, add_waic = add_waic, add_loo = TRUE,
               variable_specifications = variable_specifications, model_specifications = model_specifications, ...)

  return(fit)
}
