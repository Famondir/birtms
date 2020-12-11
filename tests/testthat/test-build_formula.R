test_that("unimplemented features throw an error", {
  # polytomous data
  model_specs <- rlang::list2(response_type = 'polytom',
                              )
  expect_error(build_formula(model_specifications = model_specs), "only models for dichotomous response")

  # noncompensatory models
  model_specs <- rlang::list2(dimensionality_type = 'multidimensional_noncompensatory',
  )
  expect_error(build_formula(model_specifications = model_specs), "noncompensatory models are not implemented")
})

test_that("basic formulas are generated correctly for different parameter numbers", {
  # 1PL
  model_specs <- rlang::list2(item_parameter_number = 1,
                       )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
                       )
  expect_equal_bf(build_formula(), form_1PL)
  expect_equal_bf(build_formula(model_specifications = model_specs), form_1PL)

  # 2PL
  model_specs <- rlang::list2(item_parameter_number = 2,
                       )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha) * theta + beta,
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
                       )
  expect_equal_bf(build_formula(model_specifications = model_specs), form_2PL)

  # 3PL
  model_specs <- rlang::list2(item_parameter_number = 3,
                              )
  form_3PL <- brms::bf(response ~ gamma + (1 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
                       )
  expect_equal_bf(build_formula(model_specifications = model_specs), form_3PL)

  # 4PL
  model_specs <- rlang::list2(item_parameter_number = 4,
  )
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       logitpsi ~ 1,
                       brms::nlf(psi ~ inv_logit(logitpsi)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(model_specifications = model_specs), form_4PL)
})

test_that("person covariates are specified correctly", {

})

test_that("skill intercept is specified correctly", {
  # 1PL
  variable_specs <- rlang::list2(skillintercept = 'knowledge',
                                 )
  form_1PL <- brms::bf(response ~ knowledge + (1 | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs), form_1PL)

  # 2PL
  variable_specs <- rlang::list2(skillintercept = 'knowledge',
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha) * theta + beta,
                       skillintercept ~ knowledge,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})
