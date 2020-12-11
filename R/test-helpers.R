expect_equal_bf <- function(...) {
  if (edition_get() >= 3) {
    expect_equal(..., ignore_formula_env = TRUE, ignore_function_env = TRUE)
  } else {
    stop('This function requires 3e of testthat.')
  }
}
