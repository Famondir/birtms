# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c("commontheta", "theta", "logalpha", "commonlogalpha", "skillintercept"))

#' Builds a brmsformula
#'
#' Builds a brmsformula for an IRT model based on user input. This is the main
#' function for formula generantion. It calls further functions to build an
#' appropriate formula. It can be called without it's arguments which loads the
#' standard settings for a linear 1PL model.
#'
#' @param variable_specifications Named list of characters or strings.
#' @param model_specifications Named list of strings and numerics.
#'
#' @return Returns an object of type brmsformula to use it with brms::brm()
#' @export
#'
#' @seealso \code{\link[brms]{brmsformula}}
#'
#' @examples
#' build_formula()
build_formula <- function(variable_specifications = NULL, model_specifications = NULL) {
  # checks validity of teh passed specification vectors and transforms the
  # strings in variable_specifications to symbols
  var_specs <- check_and_set_specifications(variable_specifications) %>%
    ensym_list() %>% enlist_syms() # if %>% used all the way the passed name to check_and_set_specifications() will be "."
  mod_specs <- check_and_set_specifications(model_specifications)

  if (mod_specs$dimensionality_type == 'multidimensional_noncompensatory') {
    stop('At the moment noncompensatory models are not implemented.')
  }

  # selects the most suitable and efficient formula generator
  if (mod_specs$response_type == 'dichotom') {
    if (mod_specs$item_parameter_number == 1) {
      if (length(var_specs$unregular_dimensions) == 0) {
        # specifies a linear formula if possible
        form <- build_formula_linear(var_specs, mod_specs$add_common_dimension)
        return(form)
      }
    }

    # specifies a nonlinear formula otherwise
    form <- build_formula_nonlinear(var_specs, mod_specs$add_common_dimension, mod_specs$item_parameter_number)
  } else {
    stop('At the moment only models for dichotomous response is implemented.')
  }

  return(form)
}

#' Checks model specifications and possibly adds standard settings
#'
#' Calls override_standard_specifications() to override the standard
#' specifications with user specifications. Checks if all specifications (the
#' names of the list) come from a list of valid names.
#'
#' @param specifications a named list of characters
#'
#' @return a list
#' @importFrom glue glue
#' @importFrom zeallot %<-%
check_and_set_specifications <- function(specifications) {
  # extracts the first part (using prefix notation `[[`(x,i)) of the passed variable name (cuts at "_").
  type <- rlang::enexpr(specifications) %>% rlang::as_string() %>% strsplit(split = "_") %>% `[[`(1) %>% `[[`(1)

  specifications <- override_standard_specifications(specifications, type)

  check_dimension_specification(specifications)

  # creates lists about which covariables should be dropped in specific
  # dimensions compared to the overall definition
  # (person_covariables_all_dimensions)
  if (type == 'variable') {
    for (i in c(specifications$regular_dimensions, specifications$unregular_dimensions, 'common')) {
      keep <- drop <- NULL
      c(keep, drop) %<-% create_droplist(eval(expr(`$`(specifications, !!glue::glue('person_covariables_{i}')))))
      eval(expr(`<-`(`$`(specifications, !!glue::glue('drop_person_covariables_{i}')),!!drop)))
      eval(expr(`<-`(`$`(specifications, !!glue::glue('person_covariables_{i}')),!!keep)))
    }
  }

  # checks if all names in the specification vector are valid
  reference_names <- get_reference_names(specifications, type)
  if (!all(names(specifications) %in% reference_names)) {
    reference_names <- glue::glue_collapse(reference_names, sep = ", ")
    stop(glue('The {type} specifications contain an invalid name. Check for typos!\n
              The allowed names are:\n
              {reference_names}'))
  }

  invisible(specifications)
}

get_reference_names <- function(specifications, type) {
  # defines valid specification names
  valid_names_variable <- c('response', 'item', 'person', 'regular_dimensions', 'unregular_dimensions',
                            'person_covariables_main_effect', 'person_covariables_all_dimensions', 'person_covariables_common',
                            'item_covariables_intercept', 'situation_covariables', 'uniform_dif',
                            'person_grouping', 'item_grouping', 'skillintercept',
                            'pseudo_guess_dimension', 'careless_error_dimension')
  valid_names_model <- c('response_type', 'item_parameter_number', 'dimensionality_type', 'add_common_dimension')

  # adds valid specification names based on defined dimensions
  for (i in c(specifications$regular_dimensions, specifications$unregular_dimensions, 'common')) {
    valid_names_variable <- c(valid_names_variable, glue('person_covariables_{i}'))
    valid_names_variable <- c(valid_names_variable, glue('drop_person_covariables_{i}'))
  }

  reference_names <- eval(sym(glue("valid_names_{type}")))
  return(reference_names)
}

check_dimension_specification <- function(specifications) {
  # checks if dimension names are unique and not equal to 'common'
  if ((length(unique(specifications$regular_dimensions)) != length(specifications$regular_dimensions)) ||
      (length(unique(specifications$unregular_dimensions)) != length(specifications$unregular_dimensions))) {
    stop('Dimension names have to be unique! Dimension name was used twice in either regular_dimensions or unregular_dimensions.')
  }
  if (length(intersect(specifications$regular_dimensions, specifications$unregular_dimensions)) > 0) {
    stop('Dimension names have to be unique! Compare regular and unregular dimensions.')
  }
  if ('common' %in% specifications$regular_dimensions || 'common' %in% specifications$unregular_dimensions) {
    stop('Dimension name "common" is reserved!')
  }
}

create_droplist <- function(person_covariable_list) {
  droplist <- c()
  if(!is.null(person_covariable_list)) {
    for (i in seq_along(person_covariable_list)) {
      if (stringr::str_sub(person_covariable_list[[i]], 1, 1) == '-') {
        droplist <- c(droplist, stringr::str_sub(person_covariable_list[[i]], 2, -1))
        person_covariable_list[i] <- stringr::str_sub(person_covariable_list[[i]], 2, -1)
      }
    }
  }

  return(list(person_covariable_list, droplist))
}

#' Augmants the standard specifications with user specifications
#'
#' @param specifications a named list
#' @param type the type of the
#'
#' @return a named list
#' @importFrom glue glue
#' @importFrom rlang expr
#' @importFrom rlang sym
override_standard_specifications <- function(specifications, type) {
  # defines the reference specifications for model and variable specifications
  variable_specs <- list(response = 'response', item ='item', person = 'person')
  model_specs <- list(response_type = 'dichotom', item_parameter_number = 1, dimensionality_type = 'unidimensional',
                      add_common_dimension = FALSE)

  # sets the specification list to compare with depending on input for type
  # ("variable" or "model")
  reference_specs <- eval(eval(expr(sym(glue("{type}_specs")))))

  # overrides reference settings
  for (i in names(specifications)) {
    reference_specs[i] <- specifications[i]
  }

  return(reference_specs)
}

#' Combines hierarchy and DIF specifications for person variables
#'
#' @param var_specs a named list of symbols
#'
#' @return an expression p
#' @importFrom rlang expr
set_person_grouping <- function(var_specs) {
  # set person grouping
  # multiple nestings are possible (e. g. students in classes in schools in
  # countries) the first specified group has the lowest hierarchy level the last
  # the highest: c(class, school, country)
  p <- expr(!!var_specs$person[[1]])

  if (!is.null(var_specs$person_grouping)) {
    # for multiple nesting the formula will print () around the groupings that
    # aren't necessary in the specification and not part of the ast e. g.:
    # "response ~ 1 + (1 | school/(class/person)) + (1 | item)" equals "response
    # ~ 1 + (1 | school/class/person) + (1 | item)"
    for (i in seq_along(var_specs$person_grouping)) {
      p <- expr(!!var_specs$person_grouping[[i]] / !!p)
    }
  }

  return(p)
}

#' Title
#'
#' @param var_specs a named list of symbols
#'
#' @return an expression p
#' @importFrom rlang expr
set_item_grouping <- function(var_specs) {
  # set item grouping
  i <- expr(!!var_specs$item[[1]])

  if (!is.null(var_specs$item_grouping)) {
    message('Please consider if pooling on item groups is the desired effect or if you want to model something different (e. g. a testlet effect).')

    for (j in seq_along(var_specs$item_grouping)) {
      i <- expr(!!var_specs$item_grouping[[j]] / !!i)
    }
  }

  return(i)
}

#' Title
#'
#' @param x an expression
#' @param specifications a list of symbols
#'
#' @return an expression x
#' #' @importFrom rlang expr
add_covars_linear <- function(x, specifications) {
  for (i in seq_along(specifications)) {
    x <- expr(!!x + !!specifications[[i]])
  }

  return(x)
}

add_covars_interaction <- function(x, specifications, dimension) {
  for (i in seq_along(specifications)) {
    x <- expr(!!x + !!dimension:!!specifications[[i]])
  }

  return(x)
}

#' Title
#'
#' @param x an expression
#' @param nl_formulae a list of expressions
#' @param specifications a list of symbols
#'
#' @return list of expressions
#' @importFrom glue glue
#' @importFrom rlang expr
#' @importFrom rlang sym
add_covars_nonlinear <- function(x, nl_formulae, specifications) {
  name <- rlang::enexpr(specifications) %>% rlang::as_string() %>% strsplit(split = "_") %>% `[[`(1) %>% `[[`(1)

  if (!is.null(specifications)) {
    x <- expr(!!x + !!sym(glue("{name}covars")))

    pcovs <- expr(0 + !!specifications[[1]])

    for (i in seq_along(specifications)[-1]) {
      pcovs <- expr(!!pcovs + !!specifications[[i]])
    }

    nl_formulae <- c(nl_formulae, expr(!!sym(glue("{name}covars")) ~ !!pcovs))
  }

  # if (length(specifications) == 1) {
  #   nl_formulae <- c(nl_formulae, expr(!!sym(glue("{name}covars")) ~ 0 + !!specifications))
  # } else if (length(specifications) > 1) {
  #   pcovs <- expr(0 + !!specifications[[1]])
  #
  #   for (i in seq_along(specifications)[-1]) {
  #     pcovs <- expr(!!pcovs + !!specifications[[i]])
  #   }
  #
  #   nl_formulae <- c(nl_formulae, expr(!!sym(glue("{name}covars")) ~ !!pcovs))
  # }

  return(list(x, nl_formulae))
}

add_person_covars_unregular <- function(person_group, var_specs, dimension = NULL) {
  x <- expr(0 + (1 | !!person_group))
  covars <- union(var_specs$person_covariables_all_dimensions, eval(expr(`$`(var_specs, !!glue::glue('person_covariables_{dimension}'))))) %>%
    setdiff(eval(expr(`$`(var_specs, !!glue::glue('drop_person_covariables_{dimension}')))))

  if (!is.null(covars)) {
    x <- add_covars_linear(x, covars)
  }

  return(x)
}

add_person_covars_regular <- function(skillterm, person_group, var_specs, dimension = NULL) {
  x <- skillterm
  covars <- union(var_specs$person_covariables_all_dimensions, eval(expr(`$`(var_specs, !!glue::glue('person_covariables_{dimension}'))))) %>%
    setdiff(eval(expr(`$`(var_specs, !!glue::glue('drop_person_covariables_{dimension}')))))

  if (!is.null(covars)) {
    x <- add_covars_interaction(x, covars, dimension)
  }

  return(x)
}

#' Title
#'
#' @param x an expression
#' @param nl_formulae a list of expressions
#' @param var_specs a named list of symbols
#' @param add_common_dimension boolean
#'
#' @return list of expressions
#' @importFrom glue glue
#' @importFrom rlang expr
#' @importFrom rlang sym
add_skill_terms_1PL <- function(x, nl_formulae, var_specs, add_common_dimension) {
  if (is.null(var_specs$unregular_dimensions)) {
    stop('This model should get a linear formula.')
  }

  # sets person grouping term
  person_group <- set_person_grouping(var_specs)

  # adds a common dimension estimator (needed for e. g. testlet models)
  if(add_common_dimension || (is.null(var_specs$regular_dimensions) && length(var_specs$unregular_dimensions) == 1)) {
    x <- expr(!!x + commontheta)
    nl_formulae <- c(nl_formulae, expr(commontheta ~ !!add_person_covars_unregular(person_group, var_specs, 'common')))
  }

  # set skill estimator for each group of regular ordered dimensions (c. f. build_formula_linear)
  counter_dimension <- 1

  if (!is.null(var_specs$regular_dimensions)) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + !!sym(glue("theta{counter_dimension}")))
      y <- expr(0 + (0 + !!var_specs$regular_dimensions[[i]] | !!person_group))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ !!add_person_covars_regular(y, person_group, var_specs, var_specs$regular_dimensions[[i]])))

      counter_dimension <- counter_dimension + 1
    }
  }

  # set skill estimator for each unregular dimension
  if (!is.null(var_specs$unregular_dimensions)) {
    for (i in seq_along(var_specs$unregular_dimensions)) {
      x <- expr(!!x + !!var_specs$unregular_dimensions[[i]] * !!sym(glue("theta{counter_dimension}")))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ !!add_person_covars_unregular(person_group, var_specs, var_specs$unregular_dimensions[[i]])))

      counter_dimension <- counter_dimension + 1
    }
  }

  return(list(x, nl_formulae))
}

#' Title
#'
#' @param x an expression
#' @param nl_formulae a list of expressions
#' @param var_specs a named list of symbols
#' @param add_common_dimension boolean
#'
#' @return list of expressions
#' @importFrom glue glue
#' @importFrom rlang expr
#' @importFrom rlang sym
add_skill_terms_2PL <- function(x, nl_formulae, var_specs, add_common_dimension) {
  # sets person grouping term
  person_group <- set_person_grouping(var_specs)

  # sets item grouping term
  item_group <- set_item_grouping(var_specs)

  alpha_formulae <- list()
  # adds a common dimension estimator (needed for e. g. testlet models)
  if (is.null(var_specs$regular_dimensions) && is.null(var_specs$unregular_dimensions)) {
    x <- expr(!!x + exp(logalpha) * theta)
    nl_formulae <- c(nl_formulae, expr(theta ~ !!add_person_covars_unregular(person_group, var_specs, 'common')))
    alpha_formulae <- c(alpha_formulae, expr(logalpha ~ 1 + (1 | !!item_group)))
  } else if(add_common_dimension || (is.null(var_specs$regular_dimensions) && length(var_specs$unregular_dimensions) == 1)) {
    x <- expr(!!x + exp(commonlogalpha) * commontheta)
    nl_formulae <- c(nl_formulae, expr(commontheta ~ !!add_person_covars_unregular(person_group, var_specs, 'common')))
    alpha_formulae <- c(alpha_formulae, expr(commonlogalpha ~ 1 + (1 | !!item_group)))
  }

  # set skill estimator for each group of regular ordered dimensions (c. f. build_formula_linear)
  counter_dimension <- 1

  if (!is.null(var_specs$regular_dimensions)) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + exp(!!sym(glue("logalpha{counter_dimension}"))) * !!sym(glue("theta{counter_dimension}")))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ 0 + (0 + !!var_specs$regular_dimensions[[i]] | !!person_group)))
      alpha_formulae <- c(alpha_formulae, expr(!!sym(glue("logalpha{counter_dimension}")) ~ 1 + (1 | !!var_specs$regular_dimensions[[i]]/!!item_group)))

      counter_dimension <- counter_dimension + 1
    }
  }

  # set skill estimator for each unregular dimension
  if (!is.null(var_specs$unregular_dimensions) > 1) {
    for (i in seq_along(var_specs$unregular_dimensions)) {
      x <- expr(!!x + !!var_specs$unregular_dimensions[[i]] * exp(!!sym(glue("logalpha{counter_dimension}"))) * !!sym(glue("theta{counter_dimension}")))
      nl_formulae <- c(nl_formulae, expr(!!sym(glue("theta{counter_dimension}")) ~ !!add_person_covars_unregular(person_group, var_specs, var_specs$unregular_dimensions[[i]])))
      alpha_formulae <- c(alpha_formulae, expr(!!sym(glue("logalpha{counter_dimension}")) ~ 1 + (1 | !!item_group)))

      counter_dimension <- counter_dimension + 1
    }
  }

  nl_formulae <- c(nl_formulae, alpha_formulae)

  return(list(x, nl_formulae))
}

add_skill_terms_linear <- function(x, var_specs, add_common_dimension) {
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
  if (!is.null(var_specs$regular_dimensions)) {
    for (i in seq_along(var_specs$regular_dimensions)) {
      x <- expr(!!x + (0 + !!var_specs$regular_dimensions[[i]] | !!person_group))
    }
  } else {
    x <- expr(!!x + (1 | !!person_group))
  }

  return(x)
}

add_skillintercept <- function(skillintercept = NULL) {
  if (is.null(skillintercept)) {
    x <- expr(1)
  } else {
    x <- expr(0 + !!skillintercept[[1]])
  }
}

#' Title
#'
#' @param var_specs a named list of symbols
#' @param add_common_dimension boolean
#' @param item_parameter_number numeric (more precise integer)
#'
#' @return brmsformula
#' @importFrom glue glue
#' @importFrom rlang expr
#' @importFrom zeallot %<-%
build_formula_nonlinear <- function(var_specs, add_common_dimension = FALSE, item_parameter_number) {

  # common intercept helps to reduce SD for all variables
  # Attention!: different intercepts for different dimensions would model a difference in the mean skill value but seems to lead to big uncertainty
  x <- expr(skillintercept)
  nl_formulae <- list(expr(skillintercept ~ !!add_skillintercept(var_specs$skillintercept)))

  # adds person skill related terms (theta and possibly alpha)
  if (item_parameter_number == 1) {
    c(x, nl_formulae) %<-% add_skill_terms_1PL(x, nl_formulae, var_specs, add_common_dimension)
  } else if (item_parameter_number %in% c(2, 3, 4)) {
    c(x, nl_formulae) %<-% add_skill_terms_2PL(x, nl_formulae, var_specs, add_common_dimension)
  }

  # sets item grouping term
  item_group <- set_item_grouping(var_specs)

  # sets item terms and terms for DIF if requested (c. f. build_formula_linear)
  x <- expr(!!x + beta)

  if (is.null(var_specs$uniform_dif)) {
    nl_formulae <- c(nl_formulae, expr(beta ~ 0 + (1 | !!item_group)))
  } else {
    nl_formulae <- c(nl_formulae, expr(beta ~ 0 + (0 + !!var_specs$uniform_dif | !!item_group) + !!var_specs$uniform_dif))
  }

  # sets terms for person covariables
  person_covariables <- var_specs$person_covariables_main_effect
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, person_covariables)

  # sets terms for item covariables
  item_covariables_intercept <- var_specs$item_covariables_intercept
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, item_covariables_intercept)

  # sets terms for situation covariables
  situation_covariables <- var_specs$situation_covariables
  c(x, nl_formulae) %<-% add_covars_nonlinear(x, nl_formulae, situation_covariables)

  if (item_parameter_number %in% c(1, 2)) {
    # creating the formula should be the last step,
    # because otherwise adding new terms would require stats::update.formula()
    # due to parenthesis creation / term order issues
    main_formula <- expr(!!var_specs$response[[1]] ~ !!x)

    form <- brms::bf(formula = main_formula, nl = TRUE, flist = nl_formulae, family = brms::brmsfamily("bernoulli", link = "logit"))

  } else if (item_parameter_number %in% c(3, 4)) {

    # sets main formula for 3PL or 4PL
    if (item_parameter_number == 3) {
      main_formula <- expr(!!var_specs$response[[1]] ~ gamma + (1 - gamma) * inv_logit(!!x))
    } else {
      main_formula <- expr(!!var_specs$response[[1]] ~ gamma + (1 - psi - gamma) * inv_logit(!!x))
    }

    # sets the group for which the pseudo guessing parameter should vary (e. g. a one parameter for each item, each testlet or a single one for the whole test)
    if (is.null(var_specs$pseudo_guess_dimension)) {
      pseudo_guess_grouping <- 1
    } else if (length(var_specs$pseudo_guess_dimension) == 1) {
      pseudo_guess_grouping <- expr(1 + (1 | !!var_specs$pseudo_guess_dimension[[1]]))
    } else stop(glue('Only one dimension is allowed for the pseudo guess parameter. You specified:\n{glue::glue_collapse(var_specs$pseudo_guess_dimension, sep = ", ")}'))

    form <- brms::bf(formula = main_formula, nl = TRUE, flist = nl_formulae, family = brms::brmsfamily("bernoulli", link = "identity")) +
      brms::lf(expr(logitgamma ~ !!pseudo_guess_grouping)) + brms::nlf(gamma ~ inv_logit(logitgamma))

    if (item_parameter_number == 4) {

      # sets the group for which the careless error parameter should vary (e. g. a one parameter for each item, each testlet or a single one for the whole test)
      if (is.null(var_specs$careless_error_dimension)) {
        careless_error_grouping <- 1
      } else if (length(var_specs$careless_error_dimension) == 1) {
        careless_error_grouping <- expr(1 + (1 | !!var_specs$careless_error_dimension[[1]]))
      } else stop(glue('Only one dimension is allowed for the pseudo guess parameter. You specified:\n{glue::glue_collapse(var_specs$careless_error_dimension, sep = ", ")}'))

      form <- form + brms::lf(expr(logitpsi ~ !!careless_error_grouping)) + brms::nlf(psi ~ inv_logit(logitpsi))
    }
  }

  return(form)
}

#' Title
#'
#' @param var_specs a named list of symbols
#' @param add_common_dimension boolean
#'
#' @return brmsformula
#' @importFrom rlang expr
build_formula_linear <- function(var_specs, add_common_dimension = FALSE) {
  # a linear formula model specification is only possible for one-parametric models
  # a linear formula model specification is only possible if there are no unregular dimensions
  # a linear formula is modeled faster by Stan

  if (length(var_specs$unregular_dimensions) > 0) {
    stop('This model should get a nonlinear formula.')
  }

  # common intercept helps to reduce SD for all variables
  # in a basic model (without regression coefficients etc.) the intercept can be interpreted as the item's mean difficulty
  # e. g. if set describes content knowledge and contains (chemistry, physics, biology) than there mustn't be 'chemistry' in a second set as well
  # Attention!: different intercepts for different dimensions would model a difference in the mean skill value but seems to lead to big uncertainty
  x <- add_skillintercept(var_specs$skillintercept)

  # adds person skill related terms
  x <- add_skill_terms_linear(x, var_specs, add_common_dimension)

  # sets item grouping term
  item_group <- set_item_grouping(var_specs)

  # sets item terms and terms for DIF if requested
  # DIF value is the difference between the two item estimators (so non of these is the "reference" category)
  # for analysis show the (absolute?) difference
  if (is.null(var_specs$uniform_dif)) {
    x <- expr(!!x + (1 | !!item_group))
  } else {
    x <- expr(!!x + (0 + !!var_specs$uniform_dif | !!item_group) + !!var_specs$uniform_dif)
  }

  # sets terms for person covariables
  x <- add_covars_linear(x, var_specs$person_covariables_main_effect)

  # sets terms for item covariables / characteristics
  # e. g. a word count (numeric) or something catergorial (factors/strings) which gets dummy coded ("Did the item included a picture or video?")
  # technically these are modeled not different from person covariables but it might be beneficial to think about the type of predictors included
  # anyway, this distinction might be necessary for the model inspection in the upcoming Shiny app
  x <- add_covars_linear(x, var_specs$item_covariables_intercept)

  # sets terms for situation covariables / characteristics / description
  # e. g. day time of test taking, time since last break, teacher in class
  # technically these are modeled not different from person covariables (cf. item covariable comment)
  x <- add_covars_linear(x, var_specs$situation_covariables)

  # creating the formula should be the last step,
  # because otherwise adding new terms would require stats::update.formula()
  # due to parenthesis creation / term order issues
  main_formula <- expr(!!var_specs$response[[1]] ~ !!x)

  form <- brms::bf(formula = main_formula, nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit"))
  return(form)
}

#' Title
#'
#' @param string_list named list of strings and characters
#'
#' @return list of symbols
#' @importFrom rlang sym
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

enlist_syms <- function(sym_list) {
  # necessary so later no if-else if for term specification is needed so just a single for-loop can be specified
  for (i in seq_along(sym_list)) {
    if (length(sym_list[[i]]) == 1) sym_list[[i]] <- c(sym_list[[i]])
  }

  return(sym_list)
}
