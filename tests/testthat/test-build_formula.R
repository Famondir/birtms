test_that("formulas are generated correctly", {
  form_1PL <- brms::bf(response ~ 1 + (1 | person) - (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit"))
  expect_equal(build_formula(), form_1PL)
})
