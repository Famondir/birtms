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

test_that("invalid specification names throw an error", {
  # variable specifications
  variable_specs <- rlang::list2(keks_quatsch = c('eins', 'zwei'),
  )
  expect_error(build_formula(variable_specs), "variable specifications contain an invalid name")

  # model specification
  model_specs <- rlang::list2(pony_glitter = 'multidimensional_noncompensatory',
  )
  expect_error(build_formula(model_specifications = model_specs), "model specifications contain an invalid name")
})

test_that("standard settings get overwritten", {
  variable_specs <- rlang::list2(response = 'resp', item ='task', person = 'ID',
  )
  form_1PL <- brms::bf(resp ~ 1 + (1 | ID) + (1 | task),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs), form_1PL)
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

test_that("regular dimensions are specified correctly", {
  # 1PL regular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ 1 + (0 + knowledge | person) + (0 + scientific_inquiry | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(regular_dimensions = 'testlet',
  )
  form_1PL <- brms::bf(response ~ 1 + (0 + testlet | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (0 + testlet | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 1PL unregular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 unregular_dimensions = c('eins', 'zwei'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ skillintercept + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person),
                       theta2 ~ 0 + (0 + scientific_inquiry | person),
                       theta3 ~ 0 + (1 | person),
                       theta4 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2PL
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha1) * theta1 + exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person),
                       theta2 ~ 0 + (0 + scientific_inquiry | person),
                       logalpha1 ~ 1 + (1 | item),
                       logalpha2 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

  variable_specs <- rlang::list2(regular_dimensions = c('testlet'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha1) * theta1 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + testlet | person),
                       logalpha1 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("unregular dimensions are specified correctly", {
  # 1PL
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ skillintercept + eins * theta1 + zwei * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person),
                       theta2 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # if there is no regular dimension and only 1 unregular dimension not all
  # items are linked to a skill dimension or the unregular mapping vector is the
  # unity vector and the model could be specified linear so commontheta is added
  variable_specs <- rlang::list2(unregular_dimensions = c('eins'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + commontheta + eins * theta1 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person),
                       theta1 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2 PL
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  form_2PL <- brms::bf(response ~ skillintercept + eins * exp(logalpha1) * theta1 + zwei * exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person),
                       theta2 ~ 0 + (1 | person),
                       logalpha1 ~ 1 + (1 | item),
                       logalpha2 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

  # if there is no regular dimension and only 1 unregular dimension not all
  # items are linked to a skill dimension or the unregular mapping vector is the
  # unity vector and the model could be specified linear so commontheta is added
  variable_specs <- rlang::list2(unregular_dimensions = c('eins'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(commonlogalpha) * commontheta + eins * exp(logalpha1) * theta1 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person),
                       theta1 ~ 0 + (1 | person),
                       commonlogalpha ~ 1 + (1 | item),
                       logalpha1 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("commontheta is not added to one dimensional model due to user misspecification", {

})

test_that("person covariates are specified correctly", {

})

test_that("person grouping is specified correctly", {

})

test_that("item grouping is specified correctly", {

})
