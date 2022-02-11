
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
      zeal_temp <- create_droplist(eval(expr(`$`(specifications, !!glue::glue('person_covariables_{i}')))))
      keep <- zeal_temp[[1]]
      drop <- zeal_temp[[2]]
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

  if (is.numeric(specifications$fixed_pseudo_guess)) specifications$fixed_pseudo_guess <- as.character(specifications$fixed_pseudo_guess)
  if (is.numeric(specifications$fixed_careless_error)) specifications$fixed_careless_error <- as.character(specifications$fixed_careless_error)

  invisible(specifications)
}

get_reference_names <- function(specifications, type) {
  # defines valid specification names
  valid_names_variable <- c('response', 'item', 'person', 'regular_dimensions', 'unregular_dimensions',
                            'person_covariables_main_effect', 'person_covariables_all_dimensions', 'person_covariables_common',
                            'item_covariables_intercept', 'situation_covariables', 'uniform_dif',
                            'person_grouping', 'item_grouping', 'skillintercept',
                            'pseudo_guess_dimension', 'careless_error_dimension',
                            'fixed_pseudo_guess', 'fixed_careless_error')
  valid_names_model <- c('response_type', 'item_parameter_number', 'dimensionality_type', 'add_common_dimension',
                         'model_unique_alpha_groups_on_regular_dimensions')

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
                      add_common_dimension = FALSE, model_unique_alpha_groups_on_regular_dimensions = TRUE)

  # sets the specification list to compare with depending on input for type
  # ("variable" or "model")
  reference_specs <- eval(eval(expr(sym(glue("{type}_specs")))))

  # overrides reference settings
  for (i in names(specifications)) {
    reference_specs[i] <- specifications[i]
  }

  return(reference_specs)
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
      sym_list[[i]] <- rlang::parse_expr(sym_list[[i]])
    } else {
      sym_list[[i]] <- ensym_list(as.list(sym_list[[i]])) # as.list allows to pass vectors or lists of parameters (e. g. for covariables)
    }
  }

  return(sym_list)
}

enlist_syms <- function(sym_list) {
  # necessary so later no if-else if for term specification is needed so just a single for-loop can be specified
  for (i in seq_along(sym_list)) {
    if (is.language(sym_list[[i]])) {
      sym_list[[i]] <- c(sym_list[[i]])
    }
  }

  return(sym_list)
}
