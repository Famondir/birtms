library(rlang)
library(dplyr)
library(brms)
library(glue)
library(zeallot)

build_formula <- function(variable_specifications = NULL, model_specifications = NULL) {
  # checks validity of teh passed specification vectors and transforms the strings in variable_specifications to symbols
  var_specs <- check_and_set_specifications(variable_specifications) %>% ensym_list() # if %>% used all the way the passed name to check_and_set_specifications() will be "."
  mod_specs <- check_and_set_specifications(model_specifications)

  # selects the most suitable and efficient formula generator
  if (mod_specs$response_type == 'dichotom') {
    if (mod_specs$item_parameter_number == 1) {
      if (! mod_specs$dimensionality_type %in% c('multidimensional_unregular', 'multidimensional_noncompensatory')) {
        form <- build_formula_linear(var_specs, mod_specs$add_common_dimension)
        return(form)
      } else if (mod_specs$dimensionality_type == 'multidimensional_noncompensatory') {
        stop('At the moment noncompensatory models are not implemented.')
      } else {
        form <- build_formula_nonlinear_1PL(var_specs, mod_specs$add_common_dimension)
        return(form)
      }
    }

    form <- build_formula_nonlinear_2PL(var_specs, mod_specs$add_common_dimension)
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
  valid_names_variable <- c('response', 'item', 'person', 'regular_dimensions', 'unregular_dimensions',
                            'person_covariables', 'item_covariables', 'situation_covariables', 'dif',
                            'person_grouping', 'item_grouping', 'skillintercept')
  valid_names_model <- c('response_type', 'item_parameter_number', 'dimensionality_type', 'add_common_dimension')

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
  model_specs <- list(response_type = 'dichotom', item_parameter_number = 1, dimensionality_type = 'unidimensional',
                      add_common_dimension = FALSE)

  # sets the specification list to compare with depending on input for name ("variable" or "model")
  reference_specs <- eval(eval(expr(sym(glue("{name}_specs")))))

  # overrides reference settings
  for (i in names(specifications)) {
    reference_specs[i] <- specifications[i]
  }

  return(reference_specs)
}

set_person_grouping <- function(var_specs) {
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

  return(p)
}

set_item_grouping <- function(var_specs) {
  # set item grouping
  # please consider if pooling on item groups is the desired effect or if you want to model something different (e. g. a testlet effect)
  if (is.null(var_specs$item_grouping)) {
    i <- expr(!!var_specs$item)
  } else {
    i <- expr(!!var_specs$item_grouping / !!var_specs$item)
  }

  return(i)
}

add_covars_linear <- function(x, specifications) {
  if (length(specifications) == 1) {
    x <- expr(!!x + !!specifications)
  } else if (length(specifications) > 1) {
    for (i in seq_along(specifications)) {
      x <- expr(!!x + !!specifications[[i]])
    }
  }

  return(x)
}

add_covars_nonlinear <- function(x, nl_formulae, specifications) {
  name <- enexpr(specifications) %>% as_string() %>% strsplit(x = ., split = "_") %>% `[[`(., 1) %>% `[[`(., 1)

  if (!is.null(specifications)) {
    x <- expr(!!x + !!sym(glue("{name}covars")))
  }

  if (length(specifications) == 1) {
    nl_formulae <- c(nl_formulae, expr(!!sym(glue("{name}covars")) ~ 0 + !!specifications))
  } else if (length(specifications) > 1) {
    pcovs <- expr(0 + !!specifications[[1]])

    for (i in seq_along(specifications)[-1]) {
      pcovs <- expr(!!pcovs + !!specifications[[i]])
    }

    nl_formulae <- c(nl_formulae, expr(!!sym(glue("{name}covars")) ~ !!pcovs))
  }

  return(list(x, nl_formulae))
}

build_formula_nonlinear_1PL <- function(var_specs, add_common_dimension = FALSE) {

  # common intercept helps to reduce SD for all variables
  # Attention!: different intercepts for different dimensions would model a difference in the mean skill value but seems to lead to big uncertainty
  x <- expr(skillintercept)
  if (is.null(var_specs$skillintercept)) {
    nl_formulae <- list(expr(skillintercept ~ 1))
  } else {
    nl_formulae <- list(expr(skillintercept ~ !!var_specs$skillintercept))
  }

  # sets person grouping term
  person_group <- set_person_grouping(var_specs)

  # adds a common dimension estimator (needed for e. g. testlet models)
  if(add_common_dimension && !(is.null(var_specs$regular_dimensions) && is.null(var_specs$unregular_dimensions))) {
    x <- expr(!!x + commontheta)
    nl_formulae <- c(nl_formulae, expr(commontheta ~ (1 | !!person_group)))
  }

  # set skill estimator for each group of regular ordered dimensions (c. f. build_formula_linear)
  counter_dimension <- 1
  if (length(var_specs$regular_dimensions) == 0 && length(var_specs$unregular_dimensions) == 0) {
    stop('This model should get a linear formula.')
  }

  if (length(var_specs$regular_dimensions) == 1) {
    x <- expr(!!x + !!sym(glue("theta{counter_dimension}")))
    nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (0 + !!var_specs$regular_dimensions | !!person_group)))

    counter_dimension <- counter_dimension + 1
  } else if (length(var_specs$regular_dimensions) > 1) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + !!sym(glue("theta{counter_dimension}")))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (0 + !!var_specs$regular_dimensions[[i]] | !!person_group)))

      counter_dimension <- counter_dimension + 1
    }
  }

  # set skill estimator for each unregular dimension
  if (length(var_specs$unregular_dimensions) == 1) {
    x <- expr(!!x + !!var_specs$unregular_dimensions * !!sym(glue("theta{counter_dimension}")))
    nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (1 | !!person_group)))

    counter_dimension <- counter_dimension + 1
  } else if (length(var_specs$unregular_dimensions) > 1) {
    for (i in seq_along(var_specs$unregular_dimensions)) {
      x <- expr(!!x + !!var_specs$unregular_dimensions[[i]] * !!sym(glue("theta{counter_dimension}")))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (1 | !!person_group)))

      counter_dimension <- counter_dimension + 1
    }
  }

  # sets item grouping term
  item_group <- set_item_grouping(var_specs)

  # sets item terms and terms for DIF if requested (c. f. build_formula_linear)
  x <- expr(!!x - beta)

  if (is.null(var_specs$dif)) {
    nl_formulae <- c(nl_formulae, expr(beta ~ 0 + (1 | !!item_group)))
  } else {
    nl_formulae <- c(nl_formulae, expr(beta ~ 0 + (0 + !!var_specs$dif | !!item_group) + !!var_specs$dif))
  }

  # sets terms for person covariables
  person_covariables <- var_specs$person_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, person_covariables)

  # sets terms for item covariables
  item_covariables <- var_specs$item_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, item_covariables)

  # sets terms for situation covariables
  situation_covariables <- var_specs$situation_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, situation_covariables)

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(!!var_specs$response ~ !!x)

  form <- bf(formula = main_formula, nl = TRUE, flist = nl_formulae, family = brmsfamily("bernoulli", link = "logit"))
  return(form)
}

build_formula_nonlinear_2PL <- function(var_specs, add_common_dimension = FALSE) {

  # common intercept helps to reduce SD for all variables
  # Attention!: different intercepts for different dimensions would model a difference in the mean skill value but seems to lead to big uncertainty
  x <- expr(skillintercept)
  if (is.null(var_specs$skillintercept)) {
    nl_formulae <- list(expr(skillintercept ~ 1))
  } else {
    nl_formulae <- list(expr(skillintercept ~ !!var_specs$skillintercept))
  }

  # sets person grouping term
  person_group <- set_person_grouping(var_specs)

  # sets item grouping term
  item_group <- set_item_grouping(var_specs)

  alpha_formulae <- list()
  # adds a common dimension estimator (needed for e. g. testlet models)
  if(add_common_dimension && !(is.null(var_specs$regular_dimensions) && is.null(var_specs$unregular_dimensions))) {
    x <- expr(!!x + commontheta * exp(logcommonalpha))
    nl_formulae <- c(nl_formulae, expr(commontheta ~ (1 | !!person_group)))
    alpha_formulae <- c(alpha_formulae, expr(logcommonalpha ~ (1 | !!item_group)))
  }

  # set skill estimator for each group of regular ordered dimensions (c. f. build_formula_linear)
  counter_dimension <- 1

  if (length(var_specs$regular_dimensions) == 1) {
    x <- expr(!!x + !!sym(glue("theta{counter_dimension}")) * exp(!!sym(glue("logalpha{counter_dimension}"))))
    nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (0 + !!var_specs$regular_dimensions | !!person_group)))
    alpha_formulae <- c(alpha_formulae, expr(!!sym(glue("logalpha{counter_dimension}")) ~ (1 | !!item_group)))

    counter_dimension <- counter_dimension + 1
  } else if (length(var_specs$regular_dimensions) > 1) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + !!sym(glue("theta{counter_dimension}")) * exp(!!sym(glue("logalpha{counter_dimension}"))))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (0 + !!var_specs$regular_dimensions[[i]] | !!person_group)))
      alpha_formulae <- c(alpha_formulae, expr(!!sym(glue("logalpha{counter_dimension}")) ~ (1 | !!item_group)))

      counter_dimension <- counter_dimension + 1
    }
  }

  # set skill estimator for each unregular dimension
  # builds stan code to set logalphas on a constant value if thi item does not belong to that dimension (mostly cosmetic)
  stan_code <- ''

  if (length(var_specs$unregular_dimensions) == 1) {
    x <- expr(!!x + !!var_specs$unregular_dimensions * !!sym(glue("theta{counter_dimension}")) * exp(!!sym(glue("logalpha{counter_dimension}"))))
    nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (1 | !!person_group)))
    alpha_formulae <- c(alpha_formulae, expr(!!sym(glue("logalpha{counter_dimension}")) ~ (1 | !!item_group)))
    stan_code <- glue('{stan_code}r_{counter_dimension+1}_logalpha{counter_dimension}_1 = r_{counter_dimension+1}_logalpha{counter_dimension}_1 .* {as_string(var_specs$unregular_dimensions)};')

    counter_dimension <- counter_dimension + 1
  } else if (length(var_specs$unregular_dimensions) > 1) {
    for (i in seq_along(var_specs$unregular_dimensions)) {
      x <- expr(!!x + !!var_specs$unregular_dimensions[[i]] * !!sym(glue("theta{counter_dimension}")) * exp(!!sym(glue("logalpha{counter_dimension}"))))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (1 | !!person_group)))
      alpha_formulae <- c(alpha_formulae, expr(!!sym(glue("logalpha{counter_dimension}")) ~ (1 | !!item_group)))
      stan_code <- glue('{stan_code}r_{(length(var_specs$item_grouping)+1)*(counter_dimension+1)}_logalpha{counter_dimension}_1 = r_{(length(var_specs$item_grouping)+1)*(counter_dimension+1)}_logalpha{counter_dimension}_1 .* {as_string(var_specs$unregular_dimensions[[i]])};')

      counter_dimension <- counter_dimension + 1
    }
  }

  nl_formulae <- c(alpha_formulae, nl_formulae)

  # sets item terms and terms for DIF if requested (c. f. build_formula_linear)
  x <- expr(!!x - beta)

  if (is.null(var_specs$dif)) {
    nl_formulae <- c(nl_formulae, expr(beta ~ 0 + (1 | !!item_group)))
  } else {
    nl_formulae <- c(nl_formulae, expr(beta ~ 0 + (0 + !!var_specs$dif | !!item_group) + !!var_specs$dif))
  }

  # sets terms for person covariables
  person_covariables <- var_specs$person_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, person_covariables)

  # sets terms for item covariables
  item_covariables <- var_specs$item_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, item_covariables)

  # sets terms for situation covariables
  situation_covariables <- var_specs$situation_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, situation_covariables)

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(!!var_specs$response ~ !!x)

  form <- bf(formula = main_formula, nl = TRUE, flist = nl_formulae, family = brmsfamily("bernoulli", link = "logit"))
  return(form)
}

build_formula_linear <- function(var_specs, add_common_dimension = FALSE) {
  # a linear formula model specification is only possible for one-parametric models
  # a linear formula model specification is only possible if there are no unregular dimensions
  # a linear formula is modeled faster by Stan

  # common intercept helps to reduce SD for all variables
  # in a basic model (without regression coefficients etc.) the intercept can be interpreted as the item's mean difficulty
  # e. g. if set describes content knowledge and contains (chemistry, physics, biology) than there mustn't be 'chemistry' in a second set as well
  # Attention!: different intercepts for different dimensions would model a difference in the mean skill value but seems to lead to big uncertainty
  if (is.null(var_specs$skillintercept)) {
    x <- expr(1)
  } else {
    x <- expr(!!var_specs$skillintercept)
  }


  # sets person grouping term
  person_group <- set_person_grouping(var_specs)

  # adds a common dimension estimator (needed for e. g. testlet models)
  if(add_common_dimension && !is.null(var_specs$regular_dimensions)) {
    x <- expr(!!x + (1 | !!person_group))
  }

  # set skill estimator for each group of regular ordered dimensions
  # a set of regular ordered dimensions assigns one dimension to each item
  # multiple regular ordered dimensions are possible as long as the mapping of the dimensions doesn't overlap
  # @2PL: estimations with regular dimension sets seem to underestimate the underlying alpha SD (uncorrelated dimensions)
  if (length(var_specs$regular_dimensions) == 1) {
    x <- expr(!!x + (0 + !!var_specs$regular_dimensions | !!person_group))
  } else if (length(var_specs$regular_dimensions) > 1) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + (0 + !!var_specs$regular_dimensions[[i]] | !!person_group))
    }
  } else {
    x <- expr(!!x + (1 | !!p))
  }

  # sets item grouping term
  item_group <- set_item_grouping(var_specs)

  # sets item terms and terms for DIF if requested
  # DIF value is the difference between the two item estimators (so non of these is the "reference" category)
  # for analysis show the (absolute?) difference
  if (is.null(var_specs$dif)) {
    x <- expr(!!x - (1 | !!item_group))
  } else {
    x <- expr(!!x - (0 + !!var_specs$dif | !!item_group) + !!var_specs$dif)
  }

  # sets terms for person covariables
  x <- add_covars_linear(x, var_specs$person_covariables)

  # sets terms for item covariables / characteristics
  # e. g. a word count (numeric) or something catergorial (factors/strings) which gets dummy coded ("Did the item included a picture or video?")
  # technically these are modeled not different from person covariables but it might be beneficial to think about the type of predictors included
  # anyway, this distinction might be necessary for the model inspection in the upcoming Shiny app
  x <- add_covars_linear(x, var_specs$item_covariables)

  # sets terms for situation covariables / characteristics / description
  # e. g. day time of test taking, time since last break, teacher in class
  # technically these are modeled not different from person covariables (cf. item covariable comment)
  x <- add_covars_linear(x, var_specs$situation_covariables)

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(!!var_specs$response ~ !!x)

  form <- bf(formula = main_formula, nl = FALSE, family = brmsfamily("bernoulli", link = "logit"))
  return(form)
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
