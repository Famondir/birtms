library(rlang)
library(dplyr)
library(brms)

build_formula <- function(variable_specifications, model_specifications) {

  if(length(variable_specifications) < 1 || !is.list(variable_specifications)) {
    stop('The variable specifications are not of type list or have length 0.')
  }

  if(length(model_specifications) < 1 || !is.list(model_specifications)) {
    stop('The model specifications are not of type list or have length 0.')
  }

  if (model_specifications$response_type == 'dichotom') {
    form <- build_formula_linear()

  } else {
    stop('At the moment only models for dichotomous response is implemented.')
  }

  return(form)
}

build_formula_linear <- function() {
  x <- expr(1 + theta - beta)
  nl_formulae <- list(expr(theta ~ 1 + (1 | person)), expr(beta ~ 1 + (1 | item)))

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(response ~ !!x)

  form <- bf(formula = main_formula, nl = TRUE, flist = nl_formulae)
}

variable_specs <- list(item ='item', person = 'person')
model_specs <- list(response_type = 'dichotom')
f <- build_formula(variable_specs, model_specs)

make_stancode(f, irt_data)
