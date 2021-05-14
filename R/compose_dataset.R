#' Composes dataset
#'
#' Composes a dataset in long format for the usage in birtms from
#' 1st) a dataset that contains the responsedata linked to the person identifiers (and possibly person covariables)
#' 2nd) a dataset that contains the person covariables linked to the person identifiers
#' 3rd) a dataset that contains the item covariables linked to the item identifiers (the column names from the responsedataset)
#' and 4th) a dataset that contains covariables that vary by person and item (called situation covariables)
#' linked to person and item identifiers (e. g. ordering of the items in a test that randomizes the item order)
#'
#'
#' @param response_data A tibble or dataframe with person identifiers and responsdata (and possibly person covariables).
#' @param response_columns A character or symbol vector with all columns that represent item responses or a range of columns specified corresponding to dplyr::select() rules (e. g.: item01:item25).
#' @param variable_specifications Named list of characters or strings.
#' @param person_data A tibble or dataframe with person identifiers linked to person covariables.
#' @param item_data A tibble or dataframe with item identifiers linked to item covariables.
#' @param situation_data A tibble or dataframe with person and item identifiers linked to situation covariables.
#'
#' @return A tibble.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' response_data <- tibble::tribble(
#' ~person, ~i1, ~i2,
#' 'a', 1, 0,
#' 'b', 1, 1,
#' 'c', 0, 0,
#'  )
#'
#' variable_specs <- list(response = 'response', item ='item', person = 'person')
#'
#' compose_dataset(response_data, i1:i2, variable_specs)
compose_dataset <- function(response_data, response_columns, variable_specifications = NULL,
                            person_data = NULL, item_data = NULL, situation_data = NULL) {

  browser()

  response_data <- response_data %>% dplyr::ungroup()

  variable_specifications <- check_and_set_specifications(variable_specifications)

  if(!is.null(person_data)) {
    person_data <- person_data %>% dplyr::ungroup()
    if (length(intersect(names(person_data), names(response_data))) > 1) stop(glue::glue('There are columns with the same name in response_data and person_data!\nThe colliding names are: {glue::glue_collapse(setdiff(intersect(names(person_data), names(response_data)),variable_specifications$person), sep = ", ")}'))
    response_data <- response_data %>% dplyr::left_join(person_data, by = variable_specifications$person)
  }

  person_covariables <- names(variable_specifications) %>% stringr::str_detect('person_covariables') %>% purrr::keep(.x = variable_specifications) %>%
    unlist(use.names = FALSE) %>% unique() %>% c(variable_specifications$uniform_dif)

  response_columns <- tryCatch(
    error = function(cnd) {
      response_columns <- rlang::enquo(response_columns)
      response_columns <- response_data %>% dplyr::select(!!response_columns) %>% names()
    },
    {
      is.character(response_columns)
      response_columns <- response_columns
    }
  )

  dataset <- response_data %>% dplyr::select(response_columns, variable_specifications$person, person_covariables) %>%
    tidyr::pivot_longer(cols = response_columns, names_to = variable_specifications$item, values_to = variable_specifications$response)

  item_covariables <- NULL
  if (!is.null(item_data)) {
    item_data <- item_data %>% dplyr::ungroup()
    item_covariables <- names(variable_specifications) %>% stringr::str_detect('item_covariables') %>% purrr::keep(.x = variable_specifications) %>%
      unlist(use.names = FALSE) %>% unique()
    item_data <- item_data %>% dplyr::select(variable_specifications$item, item_covariables, variable_specifications$regular_dimensions, variable_specifications$unregular_dimensions)

    if (length(intersect(names(item_data), names(dataset))) > 1) stop(glue::glue('There are columns with the same name in response_data or person_data and item_data!\nThe colliding names are: {glue::glue_collapse(setdiff(intersect(names(item_data), names(dataset)),variable_specifications$item), sep = ", ")}'))

    dataset <- dataset %>% dplyr::left_join(item_data, by = variable_specifications$item)
  }

  situation_covariables <- NULL
  if (!is.null(situation_data)) {
    situation_data <- situation_data %>% dplyr::ungroup()
    situation_covariables <- names(variable_specifications) %>% stringr::str_detect('situation_covariables') %>% purrr::keep(.x = variable_specifications) %>%
      unlist(use.names = FALSE) %>% unique()
    situation_data <- situation_data %>% dplyr::select(variable_specifications$item, variable_specifications$person, situation_covariables)

    if (length(intersect(names(situation_data), names(dataset))) > 2) stop(glue::glue('There are columns with the same name in response_data or person_data or item_data and situation_data!\nThe colliding names are: {glue::glue_collapse(setdiff(intersect(names(situation_data), names(dataset)), c(variable_specifications$person, variable_specifications$item)), sep = ", ")}'))

    dataset <- dataset %>% dplyr::left_join(situation_data, by = c(variable_specifications$person, variable_specifications$item))
  }

  dataset <- dataset[which(!is.na(dataset[variable_specifications$response])),]

  # correct_names <- c(variable_specifications$item, variable_specifications$response, variable_specifications$person,
  #                    person_covariables, item_covariables, situation_covariables)
  # if(!identical(sort(names(dataset)), sort(correct_names))) stop(glue::glue('Unspecified columns found: {glue::glue_collapse(setdiff(names(dataset), correct_names), sep = ", ")}\nThere might be a name conflict during dataset composition. Are all covariable names unique?'))

  return(dataset)
}
