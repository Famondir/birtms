test_that("odds are calculated correctly", {
  result <- tibble::tribble(
    ~x, ~y, ~z,
    1, 1, 1,
    0, 0, 1,
  ) %>% calculate_odds_ratio()

  expect_equal(result[[1,1]], Inf)
  expect_equal(result[[1,2]], NaN)
  expect_equal(result[[1,3]], NaN)

  result <- tibble::tribble(
    ~x, ~y,
    0, 1,
    1, 0,
    1, 1,
    0, 0,
    1, 0
  ) %>% calculate_odds_ratio()

  expect_equal(result[[1,1]], 0.5)
})

test_that(".draw colum is handeled right", {
  # tibble
  result <- tibble::tribble(
    ~.draw, ~x, ~y,
    7, 0, 1,
    7, 1, 0,
    7, 1, 1,
    7, 0, 0,
    7, 1, 0,
  ) %>% calculate_odds_ratio()

  expect_equal(result[[1,2]], 0.5)
})

test_that("different inputtypes are handled well", {
  # three dimensional array
  result <- tibble::tribble(
    ~.draw, ~x, ~y,
    7, 0, 1,
    7, 1, 0,
    7, 1, 1,
    7, 0, 0,
    7, 1, 0,
    8, 0, 1,
    8, 1, 0,
    8, 1, 1,
    8, 0, 0,
    8, 1, 0
  ) %>% dplyr::group_by(.draw) %>% dplyr::group_split(.keep = TRUE) %>%
    list2array() %>% calculate_odds_ratio()

  expect_equal(result[[1,2]], 0.5)

  # list od tibbles
  result <- tibble::tribble(
    ~.draw, ~x, ~y,
    7, 0, 1,
    7, 1, 0,
    7, 1, 1,
    7, 0, 0,
    7, 1, 0,
    8, 0, 1,
    8, 1, 0,
    8, 1, 1,
    8, 0, 0,
    8, 1, 0
  ) %>% dplyr::group_by(.draw) %>% dplyr::group_split(.keep = TRUE) %>%
    calculate_odds_ratio()

  expect_equal(result[[1,2]], 0.5)

  # pseudo three dimensional array
  result <- tibble::tribble(
    ~.draw, ~x, ~y,
    7, 0, 1,
    7, 1, 0,
    7, 1, 1,
    7, 0, 0,
    7, 1, 0,
  ) %>% dplyr::group_by(.draw) %>% dplyr::group_split(.keep = TRUE) %>%
    list2array() %>% calculate_odds_ratio()

  expect_equal(result[[1,2]], 0.5)

  # data.frame
  result <- tibble::tribble(
    ~.draw, ~x, ~y,
    7, 0, 1,
    7, 1, 0,
    7, 1, 1,
    7, 0, 0,
    7, 1, 0,
  ) %>% as.data.frame() %>% calculate_odds_ratio()

  expect_equal(result[[1,2]], 0.5)
})
