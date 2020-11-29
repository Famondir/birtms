library(brms)

irt_data <- readRDS('../irt_data.RDS')

formula <- bf(
  response ~ beta + exp(logalpha1) * map_alpha1 * theta1 +
                                               exp(logalpha2) * map_alpha2 * theta2 +
                                               exp(logalpha3) * map_alpha3 * theta3,
  nl = TRUE,
  theta1 ~ 0 + (1| person),
  theta2 ~ 0 + (1| person),
  theta3 ~ 0 + (1| person),
  beta ~ 1 + (1 | item),
  logalpha1 ~ 1 + (1 | item),
  logalpha2 ~ 1 + (1 | item),
  logalpha3 ~ 1 + (1 | item),
  family = brmsfamily("bernoulli", link = "logit")
)

prior <-
  prior("normal(0, 2)", class = "b", nlpar = "beta") + # analog zu Fujimotot
  prior("normal(0, 0.5)", class = "b", nlpar = "logalpha1") +
  prior("normal(0, 0.5)", class = "b", nlpar = "logalpha2") +
  prior("normal(0, 0.5)", class = "b", nlpar = "logalpha3") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "beta") + # analog zu BÃ¼rkner; Fujimoto wÃ¤hlt hier constant(5)
  prior("normal(0, 0.5)", class = "sd", group = "item", nlpar = "logalpha1") +
  prior("normal(0, 0.5)", class = "sd", group = "item", nlpar = "logalpha2") +
  prior("normal(0, 0.5)", class = "sd", group = "item", nlpar = "logalpha3") +
  prior("normal(1, 0.5)", class = "sd", group = "person", nlpar = "theta1") +
  prior("normal(1, 0.5)", class = "sd", group = "person", nlpar = "theta2") +
  prior("normal(1, 0.5)", class = "sd", group = "person", nlpar = "theta3")

iter = 4000

fit_2PL_3D_20items_theta3D_all_freeThetaSD <- brm(formula = formula,
           data = irt_data,
           prior = prior,
           chains = 2,
           iter = iter,
           warmup = 1500,
           cores = 2,
           refresh = max(1, iter/100)
)
