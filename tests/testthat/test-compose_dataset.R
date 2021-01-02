response_data <- tibble::tribble(
  ~person, ~i1, ~i2,
  'a', 1, 0,
  'b', 1, 1,
  'c', 0, 0,
)

response_data2 <- tibble::tribble(
  ~person, ~i1, ~i2, ~cov1, ~cov2,
  'a', 1, 0, 'm', 1,
  'b', 1, 1, 'f', 2,
  'c', 0, 0, 'd', 4,
)

person_data <- tibble::tribble(
  ~person, ~cov1, ~cov2,
  'a', 'm', 1,
  'b', 'f', 2,
  'c', 'd', 4,
)

item_data <- tibble::tribble(
  ~item, ~icov1, ~cov1,
  'i1', 'blue', 10,
  'i2', 'red', 13,
)

situation_data <- tibble::tribble(
  ~item, ~person, ~scov1, ~scov2,
  'i1', 'a', 1, 'noon',
  'i2', 'a', 2, 'morning',
  'i1', 'b', 2, 'noon',
  'i2', 'b', 1, 'morning',
)

test_that("data shifts into long format", {
  result <- tibble::tribble(
    ~person, ~item, ~response,
    'a', 'i1', 1,
    'a', 'i2', 0,
    'b', 'i1', 1,
    'b', 'i2', 1,
    'c', 'i1', 0,
    'c', 'i2', 0,
  )

  variable_specs <- list(response = 'response', item ='item', person = 'person')

  expect_equal(compose_dataset(response_data, i1:i2, variable_specs), result)
  expect_equal(compose_dataset(response_data, c(i1:i2), variable_specs), result)

  response_columns <- c('i1', 'i2')
  expect_equal(compose_dataset(response_data, response_columns, variable_specs), result)
  response_columns <- names(response_data)[-1]
  expect_equal(compose_dataset(response_data, response_columns, variable_specs), result)
})

test_that("personcovars are added correctly", {
  result <- tibble::tribble(
    ~person, ~item, ~response, ~cov1, ~cov2,
    'a', 'i1', 1, 'm', 1,
    'a', 'i2', 0, 'm', 1,
    'b', 'i1', 1, 'f', 2,
    'b', 'i2', 1, 'f', 2,
    'c', 'i1', 0, 'd', 4,
    'c', 'i2', 0, 'd', 4,
  ) %>% dplyr::relocate(cov1, cov2, .before = 2)

  variable_specs <- list(response = 'response', item ='item', person = 'person', person_covariables_main_effect = c('cov1', 'cov2'))

  expect_equal(compose_dataset(response_data2, i1:i2, variable_specs), result)
  expect_equal(compose_dataset(response_data, i1:i2, variable_specs, person_data = person_data), result)

  response_columns <- names(response_data)[-1]
  expect_equal(compose_dataset(response_data2, response_columns, variable_specs), result)

  variable_specs <- list(response = 'response', item ='item', person = 'person', person_covariables_main_effect = c('cov1', 'cov2'), person_covariables_test = c('cov2'), item_covariables_intercept = 'quak')
  expect_equal(compose_dataset(response_data2, i1:i2, variable_specs), result)
  expect_equal(compose_dataset(response_data2, response_columns, variable_specs), result)

  variable_specs <- list(response = 'response', item ='item', person = 'person', person_covariables_main_effect = c('cov1', 'cov2'))
})

test_that("only specified personcovars are added", {
  result <- tibble::tribble(
    ~person, ~item, ~response, ~cov1,
    'a', 'i1', 1, 'm',
    'a', 'i2', 0, 'm',
    'b', 'i1', 1, 'f',
    'b', 'i2', 1, 'f',
    'c', 'i1', 0, 'd',
    'c', 'i2', 0, 'd',
  ) %>% dplyr::relocate(cov1, .before = 2)

  variable_specs <- list(response = 'response', item ='item', person = 'person', person_covariables_main_effect = c('cov1'))

  expect_equal(compose_dataset(response_data2, i1:i2, variable_specs), result)
  expect_equal(compose_dataset(response_data, i1:i2, variable_specs, person_data = person_data), result)

  response_columns <- c('i1', 'i2')
  expect_equal(compose_dataset(response_data2, response_columns, variable_specs), result)
})

test_that("itemcovars are added correctly", {
  result <- tibble::tribble(
    ~person, ~item, ~response, ~cov1, ~cov2, ~icov1,
    'a', 'i1', 1, 'm', 1, 'blue',
    'a', 'i2', 0, 'm', 1, 'red',
    'b', 'i1', 1, 'f', 2, 'blue',
    'b', 'i2', 1, 'f', 2, 'red',
    'c', 'i1', 0, 'd', 4, 'blue',
    'c', 'i2', 0, 'd', 4, 'red',
  ) %>% dplyr::relocate(cov1, cov2, .before = 2)

  variable_specs <- list(response = 'response', item ='item', person = 'person',
                         person_covariables_main_effect = c('cov1', 'cov2'),
                         item_covariables_intercept = 'icov1')

  expect_equal(compose_dataset(response_data, i1:i2, variable_specs, person_data = person_data, item_data = item_data), result)

  variable_specs <- list(response = 'response', item ='item', person = 'person',
                         person_covariables_main_effect = c('cov1', 'cov2'),
                         item_covariables_intercept = 'cov1')

  expect_error(compose_dataset(response_data, i1:i2, variable_specs, person_data = person_data, item_data = item_data), 'Unspecified columns found')
})

test_that("situationcovars are added correctly", {
  result <- tibble::tribble(
    ~person, ~item, ~response, ~cov1, ~cov2, ~scov1,
    'a', 'i1', 1, 'm', 1, 1,
    'a', 'i2', 0, 'm', 1, 2,
    'b', 'i1', 1, 'f', 2, 2,
    'b', 'i2', 1, 'f', 2, 1,
    'c', 'i1', 0, 'd', 4, NA,
    'c', 'i2', 0, 'd', 4, NA,
  ) %>% dplyr::relocate(cov1, cov2, .before = 2)

  variable_specs <- list(response = 'response', item ='item', person = 'person',
                         person_covariables_main_effect = c('cov1', 'cov2'),
                         situation_covariables = 'scov1')

  expect_equal(compose_dataset(response_data, i1:i2, variable_specs, person_data = person_data, situation_data = situation_data), result)
  expect_error(compose_dataset(response_data, i1:i2, variable_specs, person_data = person_data, situation_data = item_data), 'exist')

})
