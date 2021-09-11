priors_1d_1pl <- prior("normal(0, 3)", class = "sd", group = "person")

spm_fit_nonpooled <- brms::brm(formula = response ~ 1 + (1 | person) + item,
          data = data_spm_long,
          family = brmsfamily("bernoulli", "logit"),
          prior = priors_1d_1pl,
          file = "models/gdcp/fit_1d_1pl_spm_full1_nonpooled")

tibble(nonpooled = fixef(spm_fit_nonpooled)[,1]+c(0,rep(fixef(spm_fit_nonpooled)[1],11)),
       pooled = rep(fixef(fit_1d_1pl_spm_full1)[1],12)+ranef(fit_1d_1pl_spm_full1)$item[,1,1]
) %>% mutate(item = names(ranef(fit_1d_1pl_spm_full1)$item[,1,1])) %>%
  pivot_longer(cols = -item) %>%
  ggplot(aes(x = item, y = value, color = name)) +
  geom_point()

priors_1d_1pl_strongpool <- prior("normal(0, 3)", class = "sd", group = "person") +
  prior("normal(0, 0.1)", class = "sd", group = "item")

fit_1d_1pl_spm_full1_strongpool <- birtms::birtm_aio(response_data = data_spm, response_columns = i1:i12,
                                          prior = priors_1d_1pl_strongpool,
                                          file = "models/gdcp/fit_1d_1pl_spm_full1_stronpool",
                                          refit = FALSE)

tibble(nonpooled = fixef(spm_fit_nonpooled)[,1]+c(0,rep(fixef(spm_fit_nonpooled)[1],11)),
       pooled = rep(fixef(fit_1d_1pl_spm_full1_strongpool)[1],12)+ranef(fit_1d_1pl_spm_full1_strongpool)$item[,1,1]
) %>% mutate(item = names(ranef(fit_1d_1pl_spm_full1_strongpool)$item[,1,1])) %>%
  pivot_longer(cols = -item) %>%
  ggplot(aes(x = item, y = value, color = name)) +
  geom_point()

birtms::timeit(fit_1d_1pl_spm_full1x
               <- birtms::birtm_aio(response_data = data_spm, response_columns = i1:i12,
                                                         prior = priors_1d_1pl,
                                                        ))
