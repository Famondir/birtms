test_that("list2array works", {
  t <- tibble::tribble(
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
  )
  result <- t %>% dplyr::group_by(.draw) %>% dplyr::group_split(.keep = TRUE) %>%
    list2array()

  row.names <- c("1","2","3",'4','5')
  column.names <- c(".draw","x","y")
  matrix.names <- NULL
  ary <- array(c(7,7,7,7,7,0,1,1,0,1,1,0,1,0,0,8,8,8,8,8,0,1,1,0,1,1,0,1,0,0), dim = c(5,3,2),
               dimnames = list(row.names, column.names, matrix.names))

  expect_equal(result, ary)
})

test_that("rep_dataframe works", {
  t <- tibble::tribble(
    ~x, ~y, ~z,
    1, 1, 1,
    0, 0, 1,
  )

  result <- t %>% rep_dataframe(2)

  expect_equal(result, rbind(t,t))

  t <- tibble::tribble(
    ~x, ~y, ~z,
    1, 2, 'x',
  )

  result <- t %>% rep_dataframe(2)

  expect_equal(result, rbind(t,t))
})
