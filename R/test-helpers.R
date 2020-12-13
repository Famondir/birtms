expect_equal_bf <- function(...) {
  if (edition_get() >= 3) {
    expect_equal(..., ignore_formula_env = TRUE, ignore_function_env = TRUE)
  } else {
    stop('This function requires 3e of testthat.')
  }
}

expect_equal_deparsing <- function(object, expected) {
  if (edition_get() >= 3) {
    # multiple tests are performed to the builded formula because generated
    # formulas create the same stan_code (and get deparsed equally) but diffent in
    # their lobstr::ast()s
    expect_equal_bf(object$formula, expected$formula)
    for (i in seq_along(form_2PL$pforms)) {
      expect_equal_bf(deparse(object$pforms[[i]]), deparse(expected$pforms[[i]]))
    }

  } else {
    stop('This function requires 3e of testthat.')
  }
}
