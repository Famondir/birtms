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
  form_1PL <- brms::bf(response ~ 0 + knowledge + (1 | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs), form_1PL)

  # 2PL
  variable_specs <- rlang::list2(skillintercept = 'knowledge',
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha) * theta + beta,
                       skillintercept ~ 0 + knowledge,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("dimensions names are unique and do not are equal to: common", {
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 unregular_dimensions = c('knowledge', 'scientific_inquiry'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  expect_error(build_formula(variable_specs, model_specs), 'Compare regular and unregular dimensions.')

  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'knowledge'),
  )
  expect_error(build_formula(variable_specs, model_specs), 'Dimension name was used twice')

  variable_specs <- rlang::list2(regular_dimensions = c('common', 'knowledge'),
  )
  expect_error(build_formula(variable_specs, model_specs), 'is reserved!')
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
                              # model_unique_alpha_groups_on_regular_dimensions = TRUE
  )
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha1) * theta1 + exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person),
                       theta2 ~ 0 + (0 + scientific_inquiry | person),
                       logalpha1 ~ 1 + (1 | knowledge/item),
                       logalpha2 ~ 1 + (1 | scientific_inquiry/item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

  variable_specs <- rlang::list2(regular_dimensions = c('testlet'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha1) * theta1 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + testlet | person),
                       logalpha1 ~ 1 + (1 | testlet/item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

  # don't model unique alpha groups
  model_specs <- rlang::list2(item_parameter_number = 2,
                              model_unique_alpha_groups_on_regular_dimensions = FALSE
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
})

test_that("unregular dimensions are specified correctly", {
  # 1PL unregular
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
})

test_that("commontheta is not added to one dimensional model due to user misspecification", {

  # 1PL
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(), form_1PL)
  expect_equal_bf(build_formula(model_specifications = model_specs), form_1PL)

  # 2PL
  model_specs <- rlang::list2(item_parameter_number = 2,
                              add_common_dimension = TRUE,
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha) * theta + beta,
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(model_specifications = model_specs), form_2PL)
})

test_that("commontheta is added automatically when needed", {
  # if there is no regular dimension and only 1 unregular dimension not all
  # items are linked to a skill dimension or the unregular mapping vector is the
  # unity vector and the model could be specified linear so commontheta is added

  # 1 PL
  variable_specs <- rlang::list2(unregular_dimensions = c('eins'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
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
  variable_specs <- rlang::list2(unregular_dimensions = c('eins'),
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
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

test_that("commontheta is added when specified by user (and approriate)", {
  # 1PL regular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (0 + knowledge | person) + (0 + scientific_inquiry | person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 1PL unregular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 unregular_dimensions = c('eins', 'zwei'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ skillintercept + commontheta + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person),
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
                              add_common_dimension = TRUE,
                              # model_unique_alpha_groups_on_regular_dimensions = TRUE
  )
  variable_specs <- rlang::list2(regular_dimensions = c('testlet'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(commonlogalpha) * commontheta + exp(logalpha1) * theta1 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person),
                       theta1 ~ 0 + (0 + testlet | person),
                       commonlogalpha ~ 1 + (1 | item),
                       logalpha1 ~ 1 + (1 | testlet/item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

})

test_that("person grouping is specified correctly", {
  # 1PL regular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_grouping = c('class'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | class/person) + (0 + knowledge | class/person) + (0 + scientific_inquiry | class/person) + (1 | item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2 PL
  # tests multiple groups as well
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_grouping = c('class', 'school'),
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  # stan_code for theta1 ~ 0 + (1 | school/(class/person)) is equal to stan_code
  # for theta1 ~ 0 + (1 | school/class/person)
  form_2PL <- brms::bf(response ~ skillintercept + eins * exp(logalpha1) * theta1 + zwei * exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | school/(class/person)),
                       theta2 ~ 0 + (1 | school/(class/person)),
                       logalpha1 ~ 1 + (1 | item),
                       logalpha2 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_deparsing(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("item grouping is specified correctly", {
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 item_grouping = c('testlet'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (0 + knowledge | person) + (0 + scientific_inquiry | person) + (1 | testlet/item),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(suppressMessages(build_formula(variable_specs, model_specs)), form_1PL)
  expect_message(build_formula(variable_specs, model_specs), 'Please consider if pooling on item groups')

  # 2 PL
  # tests multiple groups as well
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 item_grouping = c('x', 'y'),
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  # stan_code for beta ~ 0 + (1 | y/(x/item)) is equal to stan_code
  # for beta ~ 0 + (1 | y/x/item)
  form_2PL <- brms::bf(response ~ skillintercept + eins * exp(logalpha1) * theta1 + zwei * exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person),
                       theta2 ~ 0 + (1 | person),
                       logalpha1 ~ 1 + (1 | y/(x/item)),
                       logalpha2 ~ 1 + (1 | y/(x/item)),
                       beta ~ 0 + (1 | y/(x/item)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_deparsing(suppressMessages(build_formula(variable_specs, model_specs)), form_2PL)
})

test_that("main effect person covariates are specified correctly", {
  # 1PL regular
  variable_specs <- rlang::list2(person_covariables_main_effect = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (1 | item) + intelligence + anger,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 1PL unregular
  variable_specs <- rlang::list2(person_covariables_main_effect = c('intelligence', 'anger'),
                                 unregular_dimensions = c('eins', 'zwei'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + eins * theta1 + zwei * theta2 + beta + personcovars,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person),
                       theta2 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       personcovars ~ 0 + intelligence + anger,
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2PL
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  variable_specs <- rlang::list2(person_covariables_main_effect = c('intelligence', 'anger'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha) * theta + beta + personcovars,
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       personcovars ~ 0 + intelligence + anger,
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("item intercept covariates are specified correctly", {
  # 1PL regular
  variable_specs <- rlang::list2(item_covariables_intercept = c('pictures', 'characters'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (1 | item) + pictures + characters,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 1PL unregular
  variable_specs <- rlang::list2(item_covariables_intercept = c('pictures', 'characters'),
                                 unregular_dimensions = c('eins', 'zwei'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + eins * theta1 + zwei * theta2 + beta + itemcovars,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person),
                       theta2 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       itemcovars ~ 0 + pictures + characters,
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2PL
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  variable_specs <- rlang::list2(item_covariables_intercept = c('pictures', 'characters'),
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(logalpha) * theta + beta + itemcovars,
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       itemcovars ~ 0 + pictures + characters,
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("moderated person covariates are specified correctly for regular dimensions", {
  # # 1PL regular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = FALSE,
  )
  form_1PL <- brms::bf(response ~ 1 + (0 + knowledge | person) + (0 + scientific_inquiry | person) + (1 | item) +
                         knowledge*intelligence + knowledge*anger + scientific_inquiry*intelligence + scientific_inquiry*anger,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_knowledge = c('intelligence', 'anger'),
  )
  form_1PL <- brms::bf(response ~ 1 + (0 + knowledge | person) + (0 + scientific_inquiry | person) + (1 | item) +
                         knowledge*intelligence + knowledge*anger,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # common theta covars have same effect as main effect covars and can be mixed
  # but all dimensions won't work for common dimension here (since there is not interaction effect)
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
                                 person_covariables_common = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (0 + knowledge | person) + (1 | item) +
                         intelligence + anger + knowledge*intelligence + knowledge*anger,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(regular_dimensions = c('knowledge'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
                                 person_covariables_common = 'anger',
                                 person_covariables_main_effect = 'intelligence'
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (0 + knowledge | person) + (1 | item) +
                         intelligence + anger + knowledge*intelligence + knowledge*anger,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 1PL unregular
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ skillintercept + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person) + knowledge*intelligence + knowledge*anger,
                       theta2 ~ 0 + (0 + scientific_inquiry | person) + scientific_inquiry*intelligence + scientific_inquiry*anger,
                       theta3 ~ 0 + (1 | person) + intelligence + anger,
                       theta4 ~ 0 + (1 | person) + intelligence + anger,
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_knowledge = c('intelligence', 'anger'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person) + knowledge*intelligence + knowledge*anger,
                       theta2 ~ 0 + (0 + scientific_inquiry | person),
                       theta3 ~ 0 + (1 | person),
                       theta4 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2PL

})

test_that("moderated person covariates are specified correctly for unregular dimensions", {
  # 1PL unregular
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ skillintercept + eins * theta1 + zwei * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person) + intelligence + anger,
                       theta2 ~ 0 + (1 | person) + intelligence + anger,
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_covariables_eins = c('intelligence', 'anger'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + eins * theta1 + zwei * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person) + intelligence + anger,
                       theta2 ~ 0 + (1 | person),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ skillintercept + commontheta + eins * theta1 + zwei * theta2 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person) + intelligence + anger,
                       theta1 ~ 0 + (1 | person) + intelligence + anger,
                       theta2 ~ 0 + (1 | person) + intelligence + anger,
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # 2PL
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
  )
  form_2PL <- brms::bf(response ~ skillintercept + eins * exp(logalpha1) * theta1 + zwei * exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person) + intelligence + anger,
                       theta2 ~ 0 + (1 | person) + intelligence + anger,
                       logalpha1 ~ 1 + (1 | item),
                       logalpha2 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_covariables_zwei = c('intelligence', 'anger'),
  )

  form_2PL <- brms::bf(response ~ skillintercept + eins * exp(logalpha1) * theta1 + zwei * exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (1 | person),
                       theta2 ~ 0 + (1 | person) + intelligence + anger,
                       logalpha1 ~ 1 + (1 | item),
                       logalpha2 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)

  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
  )
  model_specs <- rlang::list2(item_parameter_number = 2,
                              add_common_dimension = TRUE,
  )
  form_2PL <- brms::bf(response ~ skillintercept + exp(commonlogalpha) * commontheta + eins * exp(logalpha1) * theta1 + zwei * exp(logalpha2) * theta2 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person) + intelligence + anger,
                       theta1 ~ 0 + (1 | person) + intelligence + anger,
                       theta2 ~ 0 + (1 | person) + intelligence + anger,
                       commonlogalpha ~ 1 + (1 | item),
                       logalpha1 ~ 1 + (1 | item),
                       logalpha2 ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_2PL)
})

test_that("person covariates can be added and dropped for single dimensions and are not added multiple times", {
  # # 1PL regular
  variable_specs <- rlang::list2(regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
                                 person_covariables_knowledge = '-anger'
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = FALSE,
  )
  form_1PL <- brms::bf(response ~ 1 + (0 + knowledge | person) + (0 + scientific_inquiry | person) + (1 | item) +
                         knowledge*intelligence + scientific_inquiry*intelligence + scientific_inquiry*anger,
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  #1 PL unregular
  # covariables are not added twice due to missspecification
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
                                 person_covariables_knowledge = c('intelligence', 'anger'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person) + knowledge*intelligence + knowledge*anger,
                       theta2 ~ 0 + (0 + scientific_inquiry | person) + scientific_inquiry*intelligence + scientific_inquiry*anger,
                       theta3 ~ 0 + (1 | person) + intelligence + anger,
                       theta4 ~ 0 + (1 | person) + intelligence + anger,
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # you can remove covars that are set with person_covariables_all_dimensions by
  # adding a - to the term in the dimension spoecific term, e.g.
  # person_covariables_zwei =  c('-anger')
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
                                 person_covariables_knowledge = c('-intelligence'),
                                 person_covariables_zwei = c('-anger'),
  )
  form_1PL <- brms::bf(response ~ skillintercept + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       theta1 ~ 0 + (0 + knowledge | person) + knowledge*anger,
                       theta2 ~ 0 + (0 + scientific_inquiry | person) + scientific_inquiry*intelligence + scientific_inquiry*anger,
                       theta3 ~ 0 + (1 | person) + intelligence + anger,
                       theta4 ~ 0 + (1 | person) + intelligence,
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  # works also for common dimension
  variable_specs <- rlang::list2(unregular_dimensions = c('eins', 'zwei'),
                                 regular_dimensions = c('knowledge', 'scientific_inquiry'),
                                 person_covariables_all_dimensions = c('intelligence', 'anger'),
                                 person_covariables_common = c('-intelligence', 'taste'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
                              add_common_dimension = TRUE,
  )
  form_1PL <- brms::bf(response ~ skillintercept + commontheta + theta1 + theta2 + eins * theta3 + zwei * theta4 + beta,
                       skillintercept ~ 1,
                       commontheta ~ 0 + (1 | person) + anger + taste,
                       theta1 ~ 0 + (0 + knowledge | person) + knowledge*intelligence + knowledge*anger,
                       theta2 ~ 0 + (0 + scientific_inquiry | person) + scientific_inquiry*intelligence + scientific_inquiry*anger,
                       theta3 ~ 0 + (1 | person) + intelligence + anger,
                       theta4 ~ 0 + (1 | person) + intelligence + anger,
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)
})

test_that("3PL formulae are specified correct", {
  variable_specs <- rlang::list2(pseudo_guess_dimension = 'item')
  model_specs <- rlang::list2(item_parameter_number = 3,
  )
  form_3PL <- brms::bf(response ~ gamma + (1 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1 + (1 | item),
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_3PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = 'guess')
  form_3PL <- brms::bf(response ~ gamma + (1 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       brms::nlf(gamma ~ guess),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_3PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = .25)
  form_3PL <- brms::bf(response ~ .25 + .75 * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_3PL)

  variable_specs <- rlang::list2(pseudo_guess_dimension = 'item', fixed_pseudo_guess = .25)
  form_3PL <- brms::bf(response ~ .25 + .75 * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_3PL)

  variable_specs <- rlang::list2(pseudo_guess_dimension = 'item', fixed_pseudo_guess = 'guess')
  form_3PL <- brms::bf(response ~ gamma + (1 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       brms::nlf(gamma ~ guess),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_3PL)
})

test_that("4PL formulae are specified correct", {
  variable_specs <- rlang::list2(careless_error_dimension = 'item')
  model_specs <- rlang::list2(item_parameter_number = 4,
  )
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       logitpsi ~ 1 + (1 | item),
                       brms::nlf(psi ~ inv_logit(logitpsi)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_careless_error = 'error')
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       brms::nlf(psi ~ error),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_careless_error = .05)
  form_4PL <- brms::bf(response ~ gamma + (.95 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(careless_error_dimension = 'item', fixed_careless_error = .05)
  form_4PL <- brms::bf(response ~ gamma + (.95 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(careless_error_dimension = 'item', fixed_careless_error = 'error')
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1,
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       brms::nlf(psi ~ error),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)
})

test_that("4PL formulae mixed with 3PL arguments are specified correct", {
  model_specs <- rlang::list2(item_parameter_number = 4,
  )
  variable_specs <- rlang::list2(pseudo_guess_dimension = 'item', fixed_careless_error = 'error')
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitgamma ~ 1 + (1 | item),
                       brms::nlf(gamma ~ inv_logit(logitgamma)),
                       brms::nlf(psi ~ error),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = 'guess', fixed_careless_error = 'error')
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       brms::nlf(gamma ~ guess),
                       brms::nlf(psi ~ error),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = 'guess', careless_error_dimension = 'item')
  form_4PL <- brms::bf(response ~ gamma + (1 - psi - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       brms::nlf(gamma ~ guess),
                       logitpsi ~ 1 + (1 | item),
                       brms::nlf(psi ~ inv_logit(logitpsi)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = .25, careless_error_dimension = 'item')
  form_4PL <- brms::bf(response ~ .25 + (.75 - psi) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       logitpsi ~ 1 + (1 | item),
                       brms::nlf(psi ~ inv_logit(logitpsi)),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = .25, fixed_careless_error = 'error')
  form_4PL <- brms::bf(response ~ .25 + (.75 - psi) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       brms::nlf(psi ~ error),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = .25, fixed_careless_error = .05)
  form_4PL <- brms::bf(response ~ .25 + .70 * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)

  variable_specs <- rlang::list2(fixed_pseudo_guess = 'guess', fixed_careless_error = .05)
  form_4PL <- brms::bf(response ~ gamma + (.95 - gamma) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                       skillintercept ~ 1,
                       theta ~ 0 + (1 | person),
                       logalpha ~ 1 + (1 | item),
                       beta ~ 0 + (1 | item),
                       brms::nlf(gamma ~ guess),
                       nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
  )
  expect_equal_bf(build_formula(variable_specifications = variable_specs, model_specifications = model_specs), form_4PL)
})

test_that("monotonous praedictors can be specified", {
  # 1PL regular
  variable_specs <- rlang::list2(person_covariables_main_effect = c('mo(intelligence)', 'inv_logit(anger)'),
  )
  model_specs <- rlang::list2(item_parameter_number = 1,
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (1 | item) + mo(intelligence) + inv_logit(anger),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

  variable_specs <- rlang::list2(person_covariables_main_effect = 'mo(intelligence)',
  )
  form_1PL <- brms::bf(response ~ 1 + (1 | person) + (1 | item) + mo(intelligence),
                       nl = FALSE, family = brms::brmsfamily("bernoulli", link = "logit")
  )
  expect_equal_bf(build_formula(variable_specs, model_specs), form_1PL)

})

test_that("monotonous praedictors can be specified", {
  # 1PL regular
  variable_specs <- rlang::list2(uniform_dif = c('test'),
  )
  expect_error(build_formula(variable_specifications = variable_specs), "dif parameter is not implemented")

})
