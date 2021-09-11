fit_1d_2pl_tina <- readRDS("models/gdcp/fit_1d_2pl_tina1.rds")

if (file.exists("models/gdcp/post_responses_1pl_tina.rds")) {
  posterior_responses_tina_2pl <- readRDS("models/gdcp/post_responses_1pl_tina.rds")
} else {
  posterior_responses_tina_2pl <- birtms::get_postdata(fit_1d_2pl_tina)
  posterior_responses_tina_2pl %>% saveRDS("models/gdcp/post_responses_1pl_tina.rds")
}

if (file.exists("models/gdcp/ppmcdata_m_2pl_tina.rds")) {
  ppmc_data_tina_2pl_mixed <- readRDS("models/gdcp/ppmcdata_m_2pl_tina.rds")
} else {
  ppmc_data_tina_2pl_mixed <- fit_1d_2pl_tina %>% birtms::get_ppmcdatasets(
    ppmcMethod = 'M', post_responses = posterior_responses_tina_2pl, n_samples = 1000)
  ppmc_data_tina_2pl_mixed %>% saveRDS("models/gdcp/ppmcdata_m_2pl_tina.rds")
}

if (file.exists("models/gdcp/ppmcdata_m_2pl_tina_outfit.rds")) {
  ppmc_data_tina_2pl_outfit_mixed <- readRDS("models/gdcp/ppmcdata_m_2pl_tina_outfit.rds")
} else {
  ppmc_data_tina_2pl_outfit_mixed <- birtms::get_ppmccriteria(model = fit_1d_2pl_tina,
                                                              ppmcdata = ppmc_data_tina_2pl_mixed, ppmcMethod = 'M', criteria = 'outfit')
  ppmc_data_tina_2pl_outfit_mixed %>% saveRDS("models/gdcp/ppmcdata_m_2pl_tina_outfit.rds")
}

if (file.exists("models/gdcp/ppmcdata_c_2pl_tina.rds")) {
  ppmc_data_tina_2pl <- readRDS("models/gdcp/ppmcdata_c_2pl_tina.rds")
} else {
  ppmc_data_tina_2pl <- fit_1d_2pl_tina %>% birtms::get_ppmcdatasets(
    ppmcMethod = 'C', post_responses = posterior_responses_tina_2pl, n_samples = 1000)
  ppmc_data_tina_2pl %>% saveRDS("models/gdcp/ppmcdata_c_2pl_tina.rds")
}

if (file.exists("models/gdcp/ppmcdata_c_2pl_tina_outfit.rds")) {
  ppmc_data_tina_2pl_outfit <- readRDS("models/gdcp/ppmcdata_c_2pl_tina_outfit.rds")
} else {
  ppmc_data_tina_2pl_outfit <- birtms::get_ppmccriteria(model = fit_1d_2pl_tina,
                                                       ppmcdata = ppmc_data_tina_2pl, ppmcMethod = 'C', criteria = 'outfit')
  ppmc_data_tina_2pl_outfit %>% saveRDS("models/gdcp/ppmcdata_c_2pl_tina_outfit.rds")
}

ppmc_infit_data <- setNames(data.frame(
  ppmc_data_tina_2pl_outfit[,c(1,5)],ppmc_data_tina_2pl_outfit_mixed[,c(5)]),
  c('item', 'c', 'm')) %>% mutate(item = as.numeric(as.factor(item)))

ggplot(data = ppmc_infit_data %>% filter(item <=12)) +
  geom_density(aes(c), colour = '#8b7d6b70', fill = '#8b7d6b70', alpha = 0.3) +
  geom_density(aes(m), colour = '#008b4570', fill = '#008b4570', alpha = 0.3) +
  facet_wrap("item", scales = "free")
