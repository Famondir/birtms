library(brms)

formula_tina_2pl_translation <- birtms::build_formula(model_specifications = list(item_parameter_number = 2),
                                               variable_specifications = list(person = 'token',
                                                                              item_covariables_intercept = c('itemtype_model1')
                                               ))

data_tina_long_translation <- birtms::compose_dataset(data_tina, response_columns = SuHT3.binary:KaKST1.binary,
                                               item_data = items_tina,
                                               variable_specifications = list(person = 'token',
                                                                              item_covariables_intercept = c('itemtype_model1')
                                               ))

brms::get_prior(formula_tina_2pl_translation, data_tina_long_translation)


priors_1d_2pl_token_translation <- prior("normal(0, 3)", nlpar = "skillintercept") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 5)", class = "b", nlpar = "itemcovars") +
  prior("constant(0)", class = "b", nlpar = "itemcovars", coef = "itemtype_model11") + # wichtig, dass ein Level der nominalen covs je subformel (person, item, situation) 0 gesetzt wird (so es nominale covs gibt)
  prior("constant(1)", class = "sd", group = "token", nlpar = "theta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

fit_1d_2pl_tina1_translation <-
  birtms::birtm_aio(response_data = data_tina,
                    item_data = items_tina,
                    response_columns = SuHT3.binary:KaKST1.binary,
                    prior = priors_1d_2pl_token_translation,
                    iter = 4000, warmup = 1000,
                    thin = 2,
                    file = "models/gdcp/fit_1d_2pl_tina1_translation",
                    model_specifications = list(item_parameter_number = 2),
                    variable_specifications = list(person = 'token',
                                                   item_covariables_intercept = c('itemtype_model1')
                    ),
                    refit = glob_refit)

post <- fit_1d_2pl_tina1_translation %>% fixef(summary = FALSE) %>% as.data.frame()

g1 <- post$itemcovars_itemtype_model12 %>% birtms::plot_ppmc_distribution() + labs(title = "itemtype_model12")
g2 <- post$itemcovars_itemtype_model14 %>% birtms::plot_ppmc_distribution() + labs(title = "itemtype_model14")
g3 <- (post$itemcovars_itemtype_model12-post$itemcovars_itemtype_model14) %>%
  birtms::plot_ppmc_distribution() + labs(title = "itemtype_model12-itemtype_model14")

cowplot::plot_grid(g1,g2,g3, nrow = 3)

trip_plot <- function(v1, v2, lab1, lab2, xlim, ylim, ci_width = .95) {
  g1 <- v1 %>% birtms::plot_ppmc_distribution(ci_width = ci_width) + labs(title = lab1) + coord_cartesian(xlim = xlim, ylim = ylim) + ylab("W.dichte") + xlab("logit-Skala")
  g2 <- v2 %>% birtms::plot_ppmc_distribution(ci_width = ci_width) + labs(title = lab2) + coord_cartesian(xlim = xlim, ylim = ylim) + ylab("W.dichte") + xlab("logit-Skala")
  g3 <- (v1-v2) %>%
    birtms::plot_ppmc_distribution(ci_width = ci_width) + labs(title = paste0(lab1, "-", lab2)) + coord_cartesian(xlim = xlim, ylim = ylim) + ylab("W.dichte") + xlab("logit-Skala")

  cowplot::plot_grid(g1,g2,g3, nrow = 3)
}

trip_plot(post$itemcovars_itemtype_model15, post$itemcovars_itemtype_model13, "itemtype_model15", "itemtype_model13", c(-2,1.5), c(0,1.2))
