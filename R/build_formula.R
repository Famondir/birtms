library(rlang)
library(dplyr)
library(brms)
library(glue)

build_formula <- function(variable_specifications = NULL, model_specifications = NULL) {
  # checks validity of teh passed specification vectors and transforms the strings in variable_specifications to symbols
  var_specs <- check_and_set_specifications(variable_specifications) %>% ensym_list() # if %>% used all the way the passed name to check_and_set_specifications() will be "."
  mod_specs <- check_and_set_specifications(model_specifications)

  # selects the most suitable and efficient formula generator
  if (mod_specs$response_type == 'dichotom') {
    if (mod_specs$item_parameter_number == 1) {
      if (substr(mod_specs$dimensionality_type, 1, 16) != 'multidimensional') {
        form <- build_formula_linear(var_specs)
        return(form)
      } else if (mod_specs$dimensionality_type == 'multidimensional_noncompensatory') {
        stop('At the moment noncompensatory models are not implemented.')
      }
    }

    form <- build_formula_nonlinear(var_specs)
  } else {
    stop('At the moment only models for dichotomous response is implemented.')
  }

  return(form)
}

check_and_set_specifications <- function(specifications) {
  # extracts the first part (using prefix notation `[[`(x,i)) of the passed variable name (cuts at "_").
  name <- enexpr(specifications) %>% as_string() %>% strsplit(x = ., split = "_") %>% `[[`(., 1) %>% `[[`(., 1)

  specifications <- override_standard_specifications(specifications, name)

  # checks if the specification vector is a list of character(vectors)
  if(length(specifications) < 1 || !is.list(specifications)) {
    stop(glue('The {name} specifications are not of type list or have length 0.'))
  }

  # defines valid specification names
  valid_names_variable <- c('response', 'item', 'person', 'unregular_dimensions', # 'regular_dimensions' were removed because they seem to underestimate the SD
                            'person_covariables', 'item_covariables', 'situation_covariables', 'dif', 'person_grouping')
  valid_names_model <- c('response_type', 'item_parameter_number', 'dimensionality_type')

  # checks if all names in the specification vector are valid
  reference_names <- eval(sym(glue("valid_names_{name}")))
  if (!all(names(specifications) %in% reference_names)) {
    reference_names <- glue_collapse(reference_names, sep = ", ")
    stop(glue('The {name} specifications contain an invalid name. Check for typos!\n
              The allowed names are:\n
              {reference_names}'))
  }

  invisible(specifications)
}

override_standard_specifications <- function(specifications, name) {
  # defines the reference specifications for model and variable specifications
  variable_specs <- list(response = 'response', item ='item', person = 'person')
  model_specs <- list(response_type = 'dichotom', item_parameter_number = 1, dimensionality_type = 'unidimensional')

  # sets the specification list to compare with depending on input for name ("variable" or "model")
  reference_specs <- eval(eval(expr(sym(glue("{name}_specs")))))

  # overrides reference settings
  for (i in names(specifications)) {
    reference_specs[i] <- specifications[i]
  }

  return(reference_specs)
}

build_formula_nonlinear <- function(var_specs) {

  nl_formulae <- list(expr(theta ~ 0 + (1 | !!var_specs$person)), expr(beta ~ 1 + (1 | !!var_specs$item)))
  x <- expr(theta - beta)

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(!!var_specs$response ~ !!x)

  form <- bf(formula = main_formula, nl = TRUE, flist = nl_formulae)
}

build_formula_linear <- function(var_specs) {
  # a linear formula model specification is only possible if there are no unregular dimensions
  # a linear formula is modeled faster by Stan

  # common intercept helps to reduce SD for all variables
  # in a basic model (without regression coefficients etc.) the intercept can be interpreted as the item's mean difficulty
  # e. g. if set describes content knowledge and contains (chemistry, physics, biology) than there mustn't be 'chemistry' in a second set as well
  x <- expr(1)

  # set person grouping
  # multiple nestings are possible (e. g. students in classes in schools in countries)
  # the first specified group has the lowest hierarchy level the last the highest: c(class, school, country)
  p <- expr(!!var_specs$person)

  if (length(var_specs$person_grouping) == 1) {
    p <- expr(!!var_specs$person_grouping / !!p)
  } else if (length(var_specs$person_grouping) > 1) {
    # for multiple nesting the formula will print () around the groupings that aren't necessary in the specification and not part of the ast
    # e. g.: "response ~ 1 + (1 | school/(class/person)) - (1 | item)" equals "response ~ 1 + (1 | school/class/person) - (1 | item)"
    for (i in seq_along(var_specs$person_grouping)) {
      p <- expr(!!var_specs$person_grouping[[i]] / !!p)
    }
  }

  # set skill estimator for each group of regular ordered dimensions
  # a set of regular ordered dimensions assigns one dimension to each item
  # multiple regular ordered dimensions are possible as long as the mapping of the dimensions doesn't overlap
  if (length(var_specs$regular_dimensions) == 1) {
    x <- expr(!!x + (!!var_specs$regular_dimensions | !!p))
  } else if (length(var_specs$regular_dimensions) > 1) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + (!!var_specs$regular_dimensions[[i]] | !!p))
    }
  } else {
    x <- expr(!!x + (1 | !!p))
  }

  # set item grouping
  # please consider if pooling on item groups is the desired effect or if you want to model something different (e. g. a testlet effect)
  if (is.null(var_specs$item_grouping)) {
    i <- expr(!!var_specs$item)
  } else {
    i <- expr(!!var_specs$item_grouping / !!var_specs$item)
  }

  # sets item terms and terms for DIF if requested
  # DIF value is the difference between the two item estimators (so non of these is the "reference" category)
  # for analysis show the (absolute?) difference
  if (is.null(var_specs$dif)) {
    x <- expr(!!x - (1 | !!i))
  } else {
    x <- expr(!!x - (0 + !!var_specs$dif | !!i) + !!var_specs$dif)
  }

  # sets terms for person covariables
  if (length(var_specs$person_covariables) == 1) {
    x <- expr(!!x + !!var_specs$person_covariables)
  } else if (length(var_specs$person_covariables) > 1) {
    for (i in seq_along(var_specs$person_covariables)) {
      x <- expr(!!x + !!var_specs$person_covariables[[i]])
    }
  }

  # sets terms for item covariables / characteristics
  # e. g. a word count (numeric) or something catergorial (factors/strings) which gets dummy coded ("Did the item included a picture or video?")
  # technically these are modeled not different from person covariables but it might be beneficial to think about the type of predictors included
  # anyway, this distinction might be necessary for the model inspection in the upcoming Shiny app
  if (length(var_specs$item_covariables) == 1) {
    x <- expr(!!x + !!var_specs$item_covariables)
  } else if (length(var_specs$item_covariables) > 1) {
    for (i in seq_along(var_specs$item_covariables)) {
      x <- expr(!!x + !!var_specs$item_covariables[[i]])
    }
  }

  # sets terms for situation covariables / characteristics / description
  # e. g. day time of test taking, time since last break, teacher in class
  # technically these are modeled not different from person covariables (cf. item covariable comment)
  if (length(var_specs$situation_covariables) == 1) {
    x <- expr(!!x + !!var_specs$situation_covariables)
  } else if (length(var_specs$situation_covariables) > 1) {
    for (i in seq_along(var_specs$situation_covariables)) {
      x <- expr(!!x + !!var_specs$situation_covariables[[i]])
    }
  }

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(!!var_specs$response ~ !!x)

  form <- bf(formula = main_formula, nl = FALSE, family = brmsfamily("bernoulli", link = "logit"))
}

ensym_list <- function(string_list) {
  sym_list <- string_list

  # recursively converts character list into list of symbols to use in expression composition
  for (i in seq_along(sym_list)) {
    if (length(sym_list[[i]]) == 1) {
      sym_list[[i]] <- sym(sym_list[[i]])
    } else {
      sym_list[[i]] <- ensym_list(as.list(sym_list[[i]])) # as.list allows to pass vectors or lists of parameters (e. g. for covariables)
    }
  }

  return(sym_list)
}
