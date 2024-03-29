#' Created a birtmsfit from brms fit by adding specs
#'
#' @param fit brmsfit object
#' @param variable_specifications list of stringt
#' @param model_specifications list of stringt
#' @param file character; filepath
#'
#' @return birtmsfit object
#' @export
#'
#' @examples
new_birtmsfit <- function(fit, variable_specifications = NULL, model_specifications = NULL, file = NULL) {
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
