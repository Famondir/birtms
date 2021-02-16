library(tidyverse)
# library(tictoc)
library(brms)
library(tidybayes)
library(rio)

# ----- KFT

X19_12_17_Rohdaten_KV_und_TF <- rio::import("../../../../Tina/Analyse/database_Rohdaten/19-12-17_Rohdaten_KV_und_TF.sav")
full_data <- X19_12_17_Rohdaten_KV_und_TF %>% rename(person = Codenr)

# var_specs <- list(response = 'response', item ='item', person = 'person')

var_specs_kft <- list(person_covariables_main_effect = 'Schultyp')
data_kft <- compose_dataset(full_data, KFT_1:KFT_25, variable_specifications = var_specs_kft)
formula_kft <- build_formula()
formula_kft_reg <- build_formula(variable_specifications = var_specs_kft)

sum_score <- data_kft %>% group_by(person, Schultyp) %>% summarise(sum = sum(response))

fit <- brm(formula = response ~ 1, data = data_kft, family = brms::brmsfamily("bernoulli", link = "logit"))
fit2 <- brm(formula = response ~ 1 + Schultyp, data = data_kft, family = brms::brmsfamily("bernoulli", link = "logit"))
fit_base <- birtm(data = data_kft, formula = formula_kft)
fit_reg <-  birtm(data = data_kft, formula = formula_kft_reg, variable_specifications = var_specs_kft)

R2_latent_regression(fit_reg, fit_base)

temp_data <- fit_reg$data %>% mutate(Schultyp = factor(Schultyp))
Y <- data.frame(Intercept = 1, Schultyp = (temp_data %>% group_by(person) %>% summarise(Schultyp = median(as.numeric(Schultyp)-1))  %>% ungroup %>% select(Schultyp))) %>% as.matrix
beta <- fixef(fit_reg)[,1] %>% as.matrix()
variance <- matrix(VarCorr(fit_reg)$person$sd[,1]^2)
res <- residuals(fit_reg)[,1]

tam_latent_regression_standardized_solution(variance, beta, Y)
# bayesian_latent_regression(res, beta, Y)

data_klass <- tibble(theta = ranef(fit_base)$person[,1,1], typ = full_data$Schultyp)
data_klass %>% ggplot(aes(x=typ, y=theta)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
summary(lm(data = data_klass, theta~typ))

mod <- TAM::tam(resp = full_data %>% select(KFT_1:KFT_25), pid = full_data$person)
wle <- TAM::tam.wle(mod)

data_klass <- data_klass %>% mutate(wle = wle$theta)
data_klass %>% ggplot(aes(x=theta, y=wle, color=typ)) + geom_point(alpha = .5) + geom_abline()
summary(lm(data = data_klass, wle~typ))

summary(lm(data = sum_score, scale(sum)~Schultyp))
summary(lme4::lmer(data = data_kft, response ~ (1|person) + Schultyp))

Schultyp <- full_data$Schultyp
dat <- full_data %>% select(KFT_1:KFT_25)
formulay1 <- ~Schultyp
lat2 <- TAM::tam.mml(dat, dataY = full_data$Schultyp, formulaY = formulay1)
se<- tam.se(lat2)
summary(lat2)
# lat1$latreg_stand$beta_stand
# lat1$latreg_stand$R2_theta

formula_kft <- build_formula(model_specifications = list(item_parameter_number = 2))
formula_kft$pforms$logalpha <- logalpha ~ 1
formula_kft$pforms$logalpha <- logalpha ~ 1 + (1 |i| item)
formula_kft$pforms$beta <- beta ~ 0 + (1 |i| item)

# kft_1PL <- brm(formula = formula_kft,
#                data = data_kft,
#                prior = NULL,
#                sample_prior = TRUE,
#                save_pars = save_pars(all = TRUE),
#                cores = 4,
#                file = NULL
#                )

kft_1PL_free_alpha <- birtm(data = data_kft, formula = formula_kft, file = 'models/kft_1PL_freealpha', iter = 2000, prior = p2PL)
kft_1PL_free_alpha_fixed_sdTheta <- birtm(data = data_kft, formula = formula_kft, file = 'models/kft_1PL_freealpha_fixed_sdTheta', iter = 2000, prior = p2PL +
                              brms::prior("constant(1)", class = "sd", group = "person", nlpar = "theta"))

p2PL_fixalpha <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("constant(0)", class = "b", nlpar = "logalpha")
kft_1PL_fixed_alphaMean <- birtm(data = data_kft, formula = formula_kft, file = 'models/kft_1PL_fixalphamean', iter = 2000, prior = p2PL_fixalpha)

kft_2PL_fixed_alphaMean <- birtm(data = data_kft, formula = build_formula(model_specifications = list(item_parameter_number = 2)),
                                 file = 'models/kft_2PL_fixalphamean', iter = 2000, prior = p2PL_fixalpha)
kft_2PL_fixed_sdTheta <- birtm(data = data_kft, formula = build_formula(model_specifications = list(item_parameter_number = 2)),
                                 file = 'models/kft_2PL_fixedThetaSD', iter = 2000, prior = p2PL +
                                   brms::prior("constant(1)", class = "sd", group = "person", nlpar = "theta"), refit = TRUE)


kft_1PL <- birtm(data = data_kft, formula = formula_kft, file = 'models/kft_1PL', iter = 2000)
# temp_kft_1PL <- birtm(data = data_kft, formula = formula_kft, iter = 1000)

# kft_1PL_2 <- birtm_aio(response_data = full_data, response_columns = KFT_1:KFT_5, iter = 1000)
# items <- c('KFT_1', 'KFT_2', 'KFT_3', 'KFT_4', 'KFT_5')
# kft_1PL_3 <- birtm_aio(response_data = full_data, response_columns = items, iter = 1000)

p2PL <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha")

# tic()
kft_2PL2 <- birtm_aio(response_data = full_data, response_columns = KFT_1:KFT_25,
                     model_specifications = list(item_parameter_number = 2),
                     prior = p2PL, file = 'models/kft_2PL_nocorr2')
# timer <- toc() # 1159.34 sec elapsed

kft_2PL_corr2 <- birtm(data = data_kft, formula = formula_kft,
                      model_specifications = list(item_parameter_number = 2),
                      prior = p2PL, file = 'models/kft_2PL_corr2')

full_data_gernderdich <- full_data %>% filter(Geschlecht %in% c('m', 'w'))
kft_1PL_dif <- birtm_aio(response_data = full_data_gernderdich, response_columns = KFT_1:KFT_25,
                         variable_specifications = list(uniform_dif = 'Geschlecht'),
                         model_specifications = list(item_parameter_number = 1),
                         file = 'models/kft_1PL_dif_gender_all_items')
kft_1PL <- birtm_aio(response_data = full_data_gernderdich, response_columns = KFT_1:KFT_25,
                         # variable_specifications = list(uniform_dif = 'Geschlecht'),
                         model_specifications = list(item_parameter_number = 1),
                         file = 'models/kft_1PL_genderfiltered')
kft_1PL_gendercov <- birtm_aio(response_data = full_data_gernderdich, response_columns = KFT_1:KFT_25,
                     variable_specifications = list(person_covariables_main_effect = 'Geschlecht'),
                     model_specifications = list(item_parameter_number = 1),
                     file = 'models/kft_1PL_genderfiltered_perscov')
kft_1PL_schoolcov <- birtm_aio(response_data = full_data, response_columns = KFT_1:KFT_25,
                               variable_specifications = list(person_covariables_main_effect = 'Schultyp'),
                               model_specifications = list(item_parameter_number = 1),
                               file = 'models/kft_1PL_schoolcov')

# data_kft <- compose_dataset(full_data, KFT_1:KFT_25, variable_specifications = list(uniform_dif = 'Geschlecht'))
# formula_kft <- build_formula(variable_specifications = list(uniform_dif = 'Geschlecht'))

get_variables(kft_1PL_dif)
data <- kft_1PL_dif %>% spread_draws(r_item[item,gender])
dif <- data %>% compare_levels(r_item, gender) %>% median_qi()

data(RankCorr, package = "ggdist")
RankCorr %>%
  spread_draws(b[i,j])

# --------------- Transformation

X19_12_17_Rohdaten_KV_und_TF <- haven::read_sav("../../../../Tina/Analyse/database_Rohdaten/19-12-17_Rohdaten_KV_und_TF.sav")
data_VS <- X19_12_17_Rohdaten_KV_und_TF
rm(X19_12_17_Rohdaten_KV_und_TF) # l?scht die Verkn?pfung mit dem alten Namen
data_VS <- data_VS[-8,]
data_VS <- data_VS[-164,]
data_VS_TF <- data_VS %>% select(HKaT2:VSuT12) %>% select(-contains("Time"))

# Umkodierung: A = 1; D1, D2, D3 = 0
map_A   <- which(data_VS_TF == "A", arr.ind=TRUE)
map_D1  <- which(data_VS_TF == "D1", arr.ind=TRUE)
map_D2  <- which(data_VS_TF == "D2", arr.ind=TRUE)
map_D3  <- which(data_VS_TF == "D3", arr.ind=TRUE)

data_VS_TF_Umkodierung = data.frame(matrix(NA, nrow = 225, ncol = 108))

colnames(data_VS_TF_Umkodierung) <- colnames(data_VS_TF)

data_VS_TF_Umkodierung[map_A] <-1
data_VS_TF_Umkodierung[map_D1] <-0
data_VS_TF_Umkodierung[map_D2] <-0
data_VS_TF_Umkodierung[map_D3] <-0

data_VS_TF <- data_VS_TF_Umkodierung
rm(data_VS_TF_Umkodierung)

# TF 1) Modellrechnung TAM alle Items, Probanden = 225 ####

# Hinzufügen Codenummer zum Datensatz (data_VS_TF)
data_VS_TF_Code <- data.frame(data_VS_TF, data_VS$Codenr) %>% rename(Codenummer = data_VS.Codenr)

tf_1pl <- birtm_aio(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                            variable_specifications = list(person = 'Codenummer'), model_specifications = list())

tf_1pl_lat <- birtm_aio(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                        person_data = data_VS %>% mutate(Codenummer = Codenr, KFTsum = scale(Rohwertsumme_KFT_berechnet)) %>% select(Codenummer, KFTsum),
                        variable_specifications = list(person = 'Codenummer', person_covariables_main_effect = 'KFTsum'), model_specifications = list())

p2PL <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha") +
  brms::prior("constant(1)", class = "sd", group = "Codenummer", nlpar = "theta")

tf_2pl_constSD <- birtm_aio(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                    variable_specifications = list(person = 'Codenummer'),
                    model_specifications = list(item_parameter_number = 2),
                    prior = p2PL, control = list(adapt_delta = 0.85),
                    iter = 6000, thin = 5,
                    )

fct_count(data_VS$Sprachkenntnisse)
data_VS$Sprachkenntnisse = as.factor(data_VS$Sprachkenntnisse)
data_VS$Sprachkenntnisse[data_VS$Sprachkenntnisse == 4] = 3
droplevels(data_VS$Sprachkenntnisse)
fct_count(data_VS$Sprachkenntnisse)


data_VS2 <- data_VS %>% rename(Codenummer = Codenr)
tf_1pl_SRLS <- birtm_aio(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                         person_data = data_VS2 %>% select(Codenummer, Sprachkenntnisse),
                         variable_specifications = list(person = 'Codenummer', person_covariables_main_effect = 'Sprachkenntnisse'),
                         model_specifications = list())

f <- build_formula(variable_specifications = list(person = 'Codenummer', person_covariables_main_effect = 'Sprachkenntnisse'),
              model_specifications = list(item_parameter_number = 2))
d <- compose_dataset(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                     person_data = data_VS2 %>% select(Codenummer, Sprachkenntnisse),
                     variable_specifications = list(person = 'Codenummer', person_covariables_main_effect = 'Sprachkenntnisse'))
brms::get_prior(f, d)

p2PL_SRLS_main <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha") +
  brms::prior("constant(1)", class = "sd", group = "Codenummer", nlpar = "theta") +
  brms::prior("constant(0)", class = "b", nlpar = "personcovars", coef = "Sprachkenntnisse1")

tf_2pl_constSD_SRLS_main <- birtm_aio(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                                 person_data = data_VS2 %>% select(Codenummer, Sprachkenntnisse),
                            variable_specifications = list(person = 'Codenummer', person_covariables_main_effect = 'Sprachkenntnisse'),
                            model_specifications = list(item_parameter_number = 2),
                            prior = p2PL_SRLS_main, control = list(adapt_delta = 0.9),
                            iter = 6000, thin = 5,
)

f <- build_formula(variable_specifications = list(person = 'Codenummer', person_covariables_all_dimensions = 'Sprachkenntnisse'),
                   model_specifications = list(item_parameter_number = 2))
d <- compose_dataset(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                     person_data = data_VS2 %>% select(Codenummer, Sprachkenntnisse),
                     variable_specifications = list(person = 'Codenummer', person_covariables_all_dimensions = 'Sprachkenntnisse'))
brms::get_prior(f, d)

p2PL_SRLS_mediated <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha") +
  brms::prior("constant(1)", class = "sd", group = "Codenummer", nlpar = "theta") +
  brms::prior("constant(0)", class = "b", nlpar = "theta", coef = "Sprachkenntnisse1")

tf_2pl_constSD_SRLS_mediated <- birtm_aio(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                                      person_data = data_VS2 %>% select(Codenummer, Sprachkenntnisse),
                                      variable_specifications = list(person = 'Codenummer', person_covariables_all_dimensions = 'Sprachkenntnisse'),
                                      model_specifications = list(item_parameter_number = 2),
                                      prior = p2PL_SRLS_mediated, control = list(adapt_delta = 0.85),
                                      iter = 6000, thin = 5,
)

cor.test(x = brms::ranef(tf_1pl)$Codenummer[,1,1], y = as.numeric(data_VS$Sprachkenntnisse), method = 'pearson')
cor.test(x = brms::ranef(tf_1pl)$Codenummer[,1,1], y = as.numeric(data_VS$Sprachkenntnisse), method = 'kendall')

# ---- teste neue 3PL-fixed Funktion ----

f <- build_formula(variable_specifications = list(person = 'Codenummer', fixed_pseudo_guess = 'guess'),
                   model_specifications = list(item_parameter_number = 3))
d <- compose_dataset(response_data = data_VS_TF_Code, response_columns = HKaT2:VSuT12,
                     variable_specifications = list(person = 'Codenummer', fixed_pseudo_guess = 'guess')) %>% mutate(guess = 1/4)

p3PL <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha") +
  brms::prior("constant(1)", class = "sd", group = "Codenummer", nlpar = "theta")

fit_3PL_fixed_tf <- birtm(data = d, formula = f, prior = p3PL, control = list(adapt_delta = 0.90))

form_3PL <- brms::bf(response ~ guess + (1 - guess) * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                     skillintercept ~ 1,
                     theta ~ 0 + (1 | Codenummer),
                     logalpha ~ 1 + (1 | item),
                     beta ~ 0 + (1 | item),
                     nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
)
fit_3PL_fixed_tf2 <- birtm(data = d, formula = form_3PL, prior = p3PL, control = list(adapt_delta = 0.90))

f2 <- build_formula(variable_specifications = list(person = 'Codenummer'),
                   model_specifications = list(item_parameter_number = 3))
p3PL2 <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha") +
  brms::prior("constant(1)", class = "sd", group = "Codenummer", nlpar = "theta") +
  brms::prior("normal(-1, 0.5)", class = "b", nlpar = "logitgamma")
fit_3PL_fixed_tf3 <- birtm(data = d, formula = f2, prior = p3PL2, control = list(adapt_delta = 0.90))


form_3PL2 <- brms::bf(response ~ .25 + .75 * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                     skillintercept ~ 1,
                     theta ~ 0 + (1 | Codenummer),
                     logalpha ~ 1 + (1 | item),
                     beta ~ 0 + (1 | item),
                     nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
)
fit_3PL_fixed_tf4 <- birtm(data = d, formula = form_3PL2, prior = p3PL, control = list(adapt_delta = 0.90))

form_3PL3 <- brms::bf(response ~ guess + guess * inv_logit(skillintercept + exp(logalpha) * theta + beta),
                      skillintercept ~ 1,
                      theta ~ 0 + (1 | Codenummer),
                      logalpha ~ 1 + (1 | item),
                      beta ~ 0 + (1 | item),
                      brms::nlf(guess ~ .25),
                      nl = TRUE, family = brms::brmsfamily("bernoulli", link = "identity")
)
fit_3PL_fixed_tf5 <- birtm(data = d, formula = form_3PL3, prior = p3PL, control = list(adapt_delta = 0.90))

# ---- 4PL

x <- read.csv(file = 'interactive_scripts/dataset.csv')
answers <- c(7, 6, 8, 2, 1, 5, 1, 6, 3, 2, 4, 5)
names(answers) <- paste0("SPM", seq_along(answers))
spm_long <- x  %>%
  mutate(person = seq_len(n())) %>%
  gather("item", "response", SPM1:SPM12) %>%
  mutate(
    answer = answers[item],
    response2 = as.numeric(response == answer),
    item = as.factor(as.numeric(sub("^SPM", "", item)))
  )

spm <- spm_long %>% select(person, response2, item) %>% pivot_wider(values_from = response2, names_from = item)

f <- build_formula(variable_specifications = list(response = 'response2', pseudo_guess_dimension = 'item', careless_error_dimension = 'item'),
                   model_specifications = list(item_parameter_number = 4))
brms::get_prior(formula = f, data = spm_long)

p4PL <- brms::prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  brms::prior("normal(0, 0.5)", class = "b", nlpar = "logalpha") +
  brms::prior("constant(1)", class = "sd", group = "person", nlpar = "theta") +
  brms::prior("normal(-2, 0.5)", class = "b", nlpar = "logitgamma") +
  brms::prior("normal(-2, 0.5)", class = "b", nlpar = "logitpsi")

prior_4pl <-
  prior("normal(0, 2)", class = "b", nlpar = "skillintercept") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(-2, 0.5)", class = "b", nlpar = "logitgamma") +
  prior("normal(-2, 0.5)", class = "b", nlpar = "logitpsi") +
  prior("normal(0, 1)", class = "sd", group = "person", nlpar = "theta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "beta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logitgamma") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logitpsi")

fit_4PL_2 <- birtm(data = spm_long, formula = f, prior = prior_4pl, control = list(adapt_delta = 0.95), iter = 3000, warmup = 1000)

# variance explained in latent IRT regression models

form1 <- bf(response2 ~ 1,
           family = brms::brmsfamily("bernoulli", link = "logit"))

prio1 <- prior("normal(0, 3)", class = "Intercept")

fit1 <- brm(data = spm_long, formula = form1, prior = prio1, core = 4)
summary(fit1, prior = TRUE)
residuals(fit1) %>% as_tibble() %>% summarise(var_res = var(Estimate), sum2_res = sum(Estimate^2))
var_base <- residuals(fit1) %>% as_tibble() %>% summarise(var_res = var(Estimate)) %>% pull
bayes_R2(fit1)


form2 <- bf(response2 ~ 1 + (1 | person),
            family = brms::brmsfamily("bernoulli", link = "logit"))
fit2 <- brm(data = spm_long, formula =  form2, prior = prio1, core = 4)
summary(fit2, prior = TRUE)
residuals(fit2) %>% as_tibble() %>% summarise(var_res = var(Estimate), sum2_res = sum(Estimate^2))
var_class <- residuals(fit2) %>% as_tibble() %>% summarise(var_res = var(Estimate)) %>% pull
bayes_R2(fit2)
loo_R2(fit2)

form3 <- bf(response2 ~ 1 + (1 | item) + (1 | person),
            family = brms::brmsfamily("bernoulli", link = "logit"))
fit3 <- brm(data = spm_long, formula =  form3, prior = prio1, core = 4)
residuals(fit3) %>% as_tibble() %>% summarise(var_res = var(Estimate), sum2_res = sum(Estimate^2))
bayes_R2(fit3)
var_irt <- residuals(fit3) %>% as_tibble() %>% summarise(var_res = var(Estimate)) %>% pull
r2_irt <- 1 - var_irt/var_base

r2_new <- ((bayes_R2(fit3)-bayes_R2(fit2))/(1-bayes_R2(fit2)))[[1]]
r2_new_summ_robust <- ((bayes_R2(fit3, summary = FALSE)-bayes_R2(fit2, summary = FALSE))/(1-bayes_R2(fit2, summary = FALSE))) %>% as_tibble() %>%
  median_hdi()
r2_new_summ <- ((bayes_R2(fit3, summary = FALSE)-bayes_R2(fit2, summary = FALSE))/(1-bayes_R2(fit2, summary = FALSE))) %>% as_tibble() %>%
  summarise(Estimate = mean(R2), Est.Error = sd(R2), Q2.5 = quantile(R2, .025), Q97.5 = quantile(R2, .975))

r2_new_summ_robust_loo <- ((loo_R2(fit3, summary = FALSE)-loo_R2(fit2, summary = FALSE))/(1-loo_R2(fit2, summary = FALSE))) %>% as_tibble() %>%
  median_hdi()

# ---- latent R2 fuction ----

# R2_latent_regression <- function(fit, fit_basemodel, robust = TRUE, loo = FALSE) {
#   if (loo) {
#     R2_fit <- brms::loo_R2(fit, summary = FALSE)
#     R2_base <- brms::loo_R2(fit_basemodel, summary = FALSE)
#   } else {
#     R2_fit <- brms::bayes_R2(fit, summary = FALSE)
#     R2_base <- brms::bayes_R2(fit_basemodel, summary = FALSE)
#   }
#
#   frac <- (R2_fit-R2_base)/(1-R2_base)
#
#   if (robust) {
#     r2 <- frac %>% tibble::as_tibble() %>% tidybayes::median_hdi()
#   } else {
#     r2 <- frac %>% tibble::as_tibble() %>% tidybayes::mean_qi() %>% dplyr::mutate(Est.Error = sd(frac), .after = 1)
#   }
#
#   return(r2)
# }

#--------- biology IRT ----

library(TAM)

eirtdata <- read_csv("https://raw.githubusercontent.com/danielbkatz/EIRT/master/eirtdata.csv")
eirtdata <- eirtdata[-1]
eirtdata <- eirtdata %>% mutate(treat = ifelse(treat==1, "treat", "not-treat"))
eirtdata$treat <- as.factor(eirtdata$treat)
eirtdata$proflevel <- as.factor(eirtdata$proflevel)
eirtdata$abilcov <- as.factor(eirtdata$abilcov)

iter <- 2000
var_specs <- list(person = 'id')
fit_bio <- birtm_aio(response_data = eirtdata, response_columns = Math.1:MathWordProb.10,
                         variable_specifications = var_specs, refresh = max(iter/100, 1)
)
var_specs_reg <- list(person = 'id', person_covariables_main_effect = 'treat')
fit_bio_reg <- birtm_aio(response_data = eirtdata, response_columns = Math.1:MathWordProb.10,
          variable_specifications = var_specs_reg, refresh = max(iter/100, 1)
          )

fit_bio_reg2 <- update(fit_bio_reg,
                       newdata = birtms::compose_dataset(response_data = eirtdata, response_columns = Math.1:MathWordProb.10, variable_specifications = var_specs_reg),
                       prior = brms::prior("normal(0, 2)", class = "Intercept"), core = 4, refresh = max(iter/100, 1))

treat <- eirtdata$treat
latg <- tam.mml(eirtdata[2:40], group = treat)
summary(latg)

formulay1 <- ~treat
lat1 <- TAM::tam.mml(eirtdata[2:40], dataY = eirtdata$treat, formulaY = formulay1)
se<- tam.se(lat1)
summary(lat1)
lat1$latreg_stand$beta_stand
lat1$latreg_stand$R2_theta

Y <- lat1$Y
beta <- lat1$beta
variance <- lat1$variance

daty <- eirtdata %>% select(treat, proflevel, abilcov)
formulay2 <- ~ treat + proflevel
latreg2 <- tam.mml(eirtdata[2:40], dataY = daty, formulaY = formulay2)
summary(latreg2)

selatreg2 <- tam.se(latreg2)
selatreg2$beta

var_specs_reg2 <- list(person = 'id', person_covariables_main_effect = c('treat', 'proflevel'))
fit_bio_reg_twopreds <- birtm_aio(response_data = eirtdata, response_columns = Math.1:MathWordProb.10,
                         variable_specifications = var_specs_reg2, refresh = max(iter/100, 1),
                         prior = brms::prior("normal(0, 2)", class = "Intercept")
)

Q <- matrix(data = 0, nrow = 40, ncol = 3)
Q4 <- matrix(data = 0, nrow = 40, ncol = 4)
map <- itype[1:40] %>% as.factor() %>% as.numeric()
for (i in 1:40) {
  Q4[i,map[i]] <- 1
  if(map[i] == 4) map[i] <- 3
  Q[i,map[i]] <- 1
}

treat <- eirtdata$treat
formulay1 <- ~treat
multi <- TAM::tam.mml(eirtdata[2:41], dataY = eirtdata$treat, formulaY = formulay1, Q=Q, control=list(snodes=2000, maxiter=100))
multi2 <- TAM::tam.mml(eirtdata[2:41], dataY = eirtdata$treat, formulaY = formulay1, Q=Q4, control=list(snodes=2000, maxiter=100))
summary(multi)

Y <- multi$Y
beta <- multi$beta
variance <- multi2$variance

summary(lltm)
bayes_R2(fit_bio_reg_lltm)
MuMIn::r.squaredGLMM(lltmlmer) # passt mit den Werten von bayes_R2 nicht überein.
# Der Wert für R2m (nur Random effects nicht fixed) entspricht in etwa R2_latent/(pi^2/3) <-- Korrekturterm aus Nakagawa (2013) (müsste eigentlich im Nenner addiert werden)
R2_latent(variance5, beta5, Y5)[[1]][[1]]/(pi^2/3)

# ----

Y2 <- data.frame(Intercept = 1, treat = (fit_bio_reg2$data %>% group_by(id) %>% summarise(treat = median(as.numeric(treat)-1)) %>% ungroup %>% select(treat))) %>% as.matrix
beta2 <- fixef(fit_bio_reg2)[,1] %>% as.matrix() %>% t
variance2 <- matrix(VarCorr(fit_bio_reg2)$id$sd[,1]^2)
# res2 <- residuals(fit_bio_reg2)[,1]

tam_latent_regression_standardized_solution(variance2, beta2, Y2)
# bayesian_latent_regression(res2, beta2, Y2)

beta3 <- fixef(fit_bio_reg2, summary = FALSE) %>% as.matrix()
variance3 <- (VarCorr(fit_bio_reg2, summary = FALSE)$id %>% as.data.frame())^2 %>% as.matrix()

R2_vec <- rep(NA, nrow(beta3))
sd_vec <- rep(NA, nrow(beta3))
for (i in seq_along(beta3[,1])) {
  temp <- tam_latent_regression_standardized_solution(variance3[i,] %>% as.matrix(), beta3[i,] %>% as.matrix(), Y2)
  R2_vec[i] <- temp$R2_theta
  sd_vec[i] <- temp$sd_theta

}

median_hdi(R2_vec)
median_hdi(sd_vec)

Y4 <- data.frame(id = fit_bio_reg_twopreds$data$id, make_standata(data = fit_bio_reg_twopreds$data, formula = fit_bio_reg_twopreds$formula)[['X']]) %>%
  group_by(id) %>% summarise_all(~ median(as.numeric(.x))) %>%ungroup() %>% select(-id) %>% as.matrix()
beta4 <- fixef(fit_bio_reg_twopreds)[,1] %>% as.matrix() %>% t
variance4 <- matrix(VarCorr(fit_bio_reg_twopreds)$id$sd[,1]^2)

# function for latent R2


R2_latent(variance4, beta4, Y4)

Y <- data.frame(id = fit_bio_reg_twopreds$data$id, make_standata(data = fit_bio_reg_twopreds$data, formula = fit_bio_reg_twopreds$formula)[['X']]) %>%
  group_by(id) %>% summarise_all(~ median(as.numeric(.x))) %>%ungroup() %>% select(-id) %>% as.matrix()
Y <- data.frame(make_standata(data = fit_bio_reg_twopreds$data, formula = fit_bio_reg_twopreds$formula)[['X']]) %>% as.matrix()
beta <- fixef(fit_bio_reg_twopreds, summary = FALSE) %>% as.matrix()
variance <- matrix(VarCorr(fit_bio_reg_twopreds, summary = FALSE)$id$sd^2)

R2_latent(variance, beta, Y)

# LLTM

#change the name of the mathwordproblem items
names(eirtdata)[32:41] <- paste0("WordProb.", 1:10)

#note the nested ifelse statements.
#Basically, if each var contains a certain value, such as "Math,"
#it gets recoded into its own column using "Mutate" from dplyr.

eirtlong <- gather(eirtdata, key = "item", value = "response", Math.1:WordProb.10) %>%
  mutate(ittype = if_else(grepl(pattern = "Math.", x = item), 1,
                          if_else(grepl(pattern = "Science.", x = item), 2,
                                  if_else(grepl(pattern = "ELA.", x = item), 3,4))))

head(eirtlong)

formulaa <- ~ittype
facets <- as.data.frame(eirtlong[c(5,7)])

#just to make it easier to call
names(facets) <- c("item", "ittype")

#only want to use the response column. Have to add an id column
lltm <- tam.mml.mfr(eirtlong[6], facets = facets,
                    formulaA = formulaa, pid = eirtlong$id)

control <- lme4::glmerControl(optimizer = "optimx",
                        calc.derivs = F, optCtrl = list(method="nlminb", starttests = F, kkt=F))


#same thing but in lme4
lltmme1 <- lme4::glmer(data=eirtlong, response~-1 + as.factor(ittype) + (1|id),
                 family = "binomial", control = control)

#random item in lme4 (I don't know how to do this in TAM)
lltmlmer <- lme4::glmer(data=eirtlong, response~-1 + as.factor(ittype) + (1|id) + (1|item),
                        family = "binomial", control = control)

itype <- strsplit(names(eirtdata[2:41]), "[.]") %>% unlist() %>% matrix(ncol = 2, byrow = TRUE)
idata <- tibble(item = names(eirtdata[2:41]), itype = itype[,1])
var_specs_reg_lltm <- list(person = 'id', item_covariables_intercept = 'itype')
fit_bio_reg_lltm <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                              item_data = idata,
                                  variable_specifications = var_specs_reg_lltm, refresh = max(iter/100, 1),
                                  prior = brms::prior("normal(0, 2)", class = "Intercept")
)
fit_bio_reg_lltm_fixintercept <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                              item_data = idata,
                              variable_specifications = var_specs_reg_lltm, refresh = max(iter/100, 1),
                              prior = brms::prior("constant(0)", class = "Intercept")
)

Y5 <- data.frame(item = fit_bio_reg_lltm$data$item, make_standata(data = fit_bio_reg_lltm$data, formula = fit_bio_reg_lltm$formula)[['X']]) %>%
  group_by(item) %>% summarise_all(~ median(as.numeric(.x))) %>%ungroup() %>% select(-item) %>% as.matrix()
beta5 <- fixef(fit_bio_reg_lltm, summary = FALSE) %>% as.matrix()
variance5 <- matrix(VarCorr(fit_bio_reg_lltm, summary = FALSE)$item$sd^2) # muss hier die Itemvarianz übergeben werden?

R2_latent(variance5, beta5, Y5)

Y_all <- data.frame(make_standata(data = fit_bio_reg_lltm$data, formula = fit_bio_reg_lltm$formula)[['X']]) %>% as.matrix()
R2_latent(variance5, beta5, Y_all)

var_specs_reg_dualmode <- list(person = 'id', item_covariables_intercept = 'itype', person_covariables_main_effect = 'treat')
fit_bio_reg_dualmode <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                              item_data = idata,
                              variable_specifications = var_specs_reg_dualmode, refresh = max(iter/100, 1),
                              prior = brms::prior("normal(0, 2)", class = "Intercept")
)

Y_all <- data.frame(make_standata(data = fit_bio_reg_dualmode$data, formula = fit_bio_reg_dualmode$formula)[['X']]) %>% as.matrix()
Y6 <- data.frame(id = fit_bio_reg_dualmode$data$id, make_standata(data = fit_bio_reg_dualmode$data, formula = fit_bio_reg_dualmode$formula)[['X']]) %>%
  group_by(id) %>% summarise_all(~ median(as.numeric(.x))) %>%ungroup() %>% select(-id) %>% as.matrix()
Y7 <- data.frame(item = fit_bio_reg_dualmode$data$item, make_standata(data = fit_bio_reg_dualmode$data, formula = fit_bio_reg_dualmode$formula)[['X']]) %>%
  group_by(item) %>% summarise_all(~ median(as.numeric(.x))) %>%ungroup() %>% select(-item) %>% as.matrix()

beta_all <- fixef(fit_bio_reg_dualmode, summary = FALSE) %>% as.matrix()
variance_all <- matrix(VarCorr(fit_bio_reg_dualmode, summary = FALSE)$item$sd^2)

R2_latent(variance5, beta5, Y_all)

var_specs_reg_multidim <- list(person = 'id', regular_dimensions = 'itype', person_covariables_main_effect = 'treat')
fit_bio_reg_multidim <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                                  item_data = idata,
                                  variable_specifications = var_specs_reg_multidim, refresh = max(iter/100, 1),
                                  prior = brms::prior("normal(0, 2)", class = "Intercept")
)


var_specs_reg_multidim2 <- list(person = 'id', regular_dimensions = 'itype', person_covariables_all_dimensions = 'treat')
build_formula(variable_specifications = var_specs_reg_multidim2)
prio_multidim2 <- brms::prior("normal(0, 2)", class = "Intercept") +
  brms::prior("constant(0)", class = "b", coef ='itypeELA:treatnotMtreat') +
  brms::prior("constant(0)", class = "b", coef ='itypeMath:treatnotMtreat') +
  brms::prior("constant(0)", class = "b", coef ='itypeScience:treatnotMtreat') +
  brms::prior("constant(0)", class = "b", coef ='itypeWordProb:treatnotMtreat')

fit_bio_reg_multidim2 <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                                        item_data = idata,
                                        variable_specifications = var_specs_reg_multidim2, refresh = max(iter/100, 1),
                                        prior = prio_multidim2,
                                        chains = 4, core = 4
)

idata2 <- idata
idata2[idata2$itype == 'WordProb',2] <- 'Science'
idata2$itype %>% as.factor()

var_specs_reg_3dim <- list(person = 'id', regular_dimensions = 'itype', person_covariables_main_effect = 'treat')
prio_3dim <- brms::prior("normal(0, 2)", class = "Intercept")
get_prior(formula=build_formula(var_specs_reg_3dim), data = compose_dataset(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                                                                            item_data = idata2,
                                                                            variable_specifications = var_specs_reg_3dim))

fit_bio_reg_3dim <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                                   item_data = idata2,
                                   variable_specifications = var_specs_reg_3dim, refresh = max(iter/100, 1),
                                   prior = prio_3dim,
                                   chains = 4, core = 2,
                              iter = iter, warmup = 1000
)

var_specs_3dim <- list(person = 'id', regular_dimensions = 'itype')
prio_3dim <- brms::prior("normal(0, 2)", class = "Intercept")
get_prior(formula=build_formula(var_specs_3dim), data = compose_dataset(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                                                                            item_data = idata2,
                                                                            variable_specifications = var_specs_3dim))

fit_bio_3dim <- birtm_aio(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                              item_data = idata2,
                              variable_specifications = var_specs_3dim, refresh = max(iter/100, 1),
                              prior = prio_3dim,
                              chains = 4, core = 2,
                              iter = iter, warmup = 1000
)

variance_all <- matrix(VarCorr(fit_bio_reg_3dim, summary = FALSE)$item$sd^2)

R2_latent_birtms(fit_bio_reg_dualmode)
R2_latent_birtms(fit_bio_reg_3dim)
R2_latent_birtms(fit_bio_reg_lltm)
R2_latent_birtms(fit_bio_reg_twopreds)

Y_all <- data.frame(make_standata(data = fit_bio_reg$data, formula = fit_bio_reg$formula)[['X']]) %>% as.matrix()
beta_all <- fixef(fit_bio_reg, summary = FALSE) %>% as.matrix()
variance_all <- matrix(VarCorr(fit_bio_reg, summary = FALSE)$id$sd^2)
R2_latent(variance_all, beta_all, Y_all)

# ---- check different regression formulations ----

eirtdata_frac <- eirtdata %>% sample_frac(size = .1)
var_specs_frac <- list(person = 'id')
prio_frac <- brms::prior("normal(0, 2)", class = "Intercept")
fit_bio_frac <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                              item_data = idata,
                              variable_specifications = var_specs_frac, refresh = max(iter/20, 1),
                              prior = prio_frac,
                              chains = 4, core = 4,
                              iter = iter, warmup = 1000
)

var_specs_frac_treat <- list(person = 'id', person_covariables_main_effect = 'treat')
prio_frac <- brms::prior("normal(0, 2)", class = "Intercept")
fit_bio_frac_treat <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                          item_data = idata,
                          variable_specifications = var_specs_frac_treat, refresh = max(iter/20, 1),
                          prior = prio_frac,
                          chains = 4, core = 4,
                          iter = iter, warmup = 1000
)

var_specs_frac_treat_and_itype <- list(person = 'id', person_covariables_main_effect = c('treat'), item_covariables_intercept = 'itype')
prio_frac <- brms::prior("normal(0, 2)", class = "Intercept")
fit_bio_frac_treat_and_itype <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                item_data = idata,
                                variable_specifications = var_specs_frac_treat_and_itype, refresh = max(iter/20, 1),
                                prior = prio_frac,
                                chains = 4, core = 4,
                                iter = iter, warmup = 1000
)

var_specs_frac_treat_times_itype <- list(person = 'id', person_covariables_main_effect = c('treat*itype'))
fit_bio_frac_treat_times_itype <- birtm(data = compose_dataset(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                                               item_data = idata, variable_specifications = var_specs_frac_treat_and_itype),
                                        formula = build_formula(var_specs_frac_treat_times_itype),
                                        prior = prio_frac)

var_specs_frac_treat_by_itype <- list(person = 'id', person_covariables_main_effect = c('itype:treat'))
prio_frac_by <- brms::prior("normal(0, 2)", class = "Intercept") +
  brms::prior("constant(0)", class = "b", coef ='itypeELA:treatnotMtreat') +
  brms::prior("constant(0)", class = "b", coef ='itypeMath:treatnotMtreat') +
  brms::prior("constant(0)", class = "b", coef ='itypeScience:treatnotMtreat') +
  brms::prior("constant(0)", class = "b", coef ='itypeWordProb:treatnotMtreat')
fit_bio_frac_treat_by_itype <- birtm(data = compose_dataset(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                                               item_data = idata, variable_specifications = var_specs_frac_treat_and_itype),
                                        formula = build_formula(var_specs_frac_treat_by_itype),
                                     prior = prio_frac_by)

var_specs_frac_itype <- list(person = 'id', item_covariables_intercept = 'itype')
fit_bio_frac_itype <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                          item_data = idata,
                                          variable_specifications = var_specs_frac_itype, refresh = max(iter/20, 1),
                                          prior = prio_frac,
                                          chains = 4, core = 4,
                                          iter = iter, warmup = 1000
)

var_specs_frac_4d <- list(person = 'id', regular_dimensions = 'itype')
fit_bio_frac_4d <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                item_data = idata,
                                variable_specifications = var_specs_frac_4d, refresh = max(iter/20, 1),
                                prior = prio_frac,
                                chains = 4, core = 4,
                                iter = iter, warmup = 1000
)

var_specs_frac_4d_itype <- list(person = 'id', regular_dimensions = 'itype', item_covariables_intercept = 'itype')
fit_bio_frac_4d_itype <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                             item_data = idata,
                             variable_specifications = var_specs_frac_4d_itype, refresh = max(iter/20, 1),
                             prior = prio_frac,
                             chains = 4, core = 4,
                             iter = iter, warmup = 1000
)

var_specs_frac_4d_treat <- list(person = 'id', regular_dimensions = 'itype', person_covariables_main_effect = c('treat'))
fit_bio_frac_4d_treat <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                   item_data = idata,
                                   variable_specifications = var_specs_frac_4d_treat, refresh = max(iter/20, 1),
                                   prior = prio_frac,
                                   chains = 4, core = 4,
                                   iter = iter, warmup = 1000
)

var_specs_frac_4d_treat_times_itype <- list(person = 'id', regular_dimensions = 'itype', person_covariables_main_effect = c('treat*itype'))
fit_bio_frac_4d_treat_times_itype <- birtm(data = compose_dataset(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                                               item_data = idata, variable_specifications = var_specs_frac_treat_and_itype),
                                        formula = build_formula(var_specs_frac_4d_treat_times_itype),
                                        prior = prio_frac)

var_specs_frac_4d_treat_by_itype <- list(person = 'id', regular_dimensions = 'itype', person_covariables_main_effect = c('itype:treat'))
fit_bio_frac_4d_treat_by_itype <- birtm(data = compose_dataset(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                                                  item_data = idata, variable_specifications = var_specs_frac_treat_and_itype),
                                           formula = build_formula(var_specs_frac_4d_treat_by_itype),
                                           prior = prio_frac_by)

# can't add - terms to remove some predictors to get to : from *
var_specs_frac_4d_test_minus <- list(person = 'id', regular_dimensions = 'itype', person_covariables_all_dimensions = c('treat'), person_covariables_main_effect = '-itype')
fit_bio_frac_4d_treat_times_itype_min_itype <- birtm(data = compose_dataset(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                                                  item_data = idata, variable_specifications = var_specs_frac_treat_and_itype),
                                           formula = build_formula(var_specs_frac_4d_test_minus),
                                           prior = prio_frac)

get_prior(data = compose_dataset(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                                 item_data = idata, variable_specifications = var_specs_frac_treat_and_itype),
          formula = build_formula(var_specs_frac_4d_test_minus))

# ----  R2 with missings and heterogen answers ----

FDW_data <- rio::import('interactive_scripts/Gesamtdatensatz Querschnitt.sav')
kft_simon <- FDW_data %>% as_tibble %>% group_by(Usercode, Studienstand_FD_Chemie) %>% select(starts_with('KFT')) %>% select(ends_with('P')) %>% `[`(-c(1:17),) %>% ungroup %>%
  mutate(group = factor(Studienstand_FD_Chemie, labels = c('BA1','MA','BA2','MA1','MA2')))

kft_1PL_simon <- birtm_aio(response_data = kft_simon, response_columns = KFTA01P:KFTA25P,
                           variable_specifications = list(person = 'Usercode'))
kft_1PL_simon_reg <- birtm_aio(response_data = kft_simon, response_columns = KFTA01P:KFTA25P,
                           variable_specifications = list(person = 'Usercode', person_covariables_main_effect = 'group'))

R2_latent(kft_1PL_simon_reg, fast=FALSE)
R2_latent(kft_1PL_simon_reg)
# scheint beides identisch zu sein, obwohl unterschiedliche Leute unterschiedlich viele Beobachtungen beitrugen

grp <- kft_simon$group
latg <- tam.mml(kft_simon %>% select(starts_with('KFT')), group = grp)
summary(latg)

formulay1 <- ~grp
lat1 <- TAM::tam.mml(kft_simon %>% select(starts_with('KFT')), dataY = kft_simon$group, formulaY = formulay1)
se<- tam.se(lat1)
summary(lat1)


# ---- 2pl ----

eirtdata <- read_csv("https://raw.githubusercontent.com/danielbkatz/EIRT/master/eirtdata.csv")
eirtdata <- eirtdata[-1]
eirtdata <- eirtdata %>% mutate(treat = ifelse(treat==1, "treat", "not-treat"))
eirtdata$treat <- as.factor(eirtdata$treat)
eirtdata$proflevel <- as.factor(eirtdata$proflevel)
eirtdata$abilcov <- as.factor(eirtdata$abilcov)
names(eirtdata)[32:41] <- paste0("WordProb.", 1:10)

itype <- strsplit(names(eirtdata[2:41]), "[.]") %>% unlist() %>% matrix(ncol = 2, byrow = TRUE)
idata <- tibble(item = names(eirtdata[2:41]), itype = itype[,1])

iter <- 2000
idata2 <- idata
idata2[idata2$itype == 'WordProb',2] <- 'Science'

eirtdata_frac <- eirtdata %>% sample_frac(size = .5)
model_specs2pl = list(item_parameter_number = 2)
model_specs2pl_regalphagroups = list(item_parameter_number = 2, model_unique_alpha_groups_on_regular_dimensions = TRUE)

var_specs_3dim <- list(person = 'id', regular_dimensions = 'itype')
prio_3dim_2pl <- brms::prior("normal(0, 2)", nlpar = 'skillintercept', class = "b") +
  brms::prior("normal(0, 1)", nlpar = 'logalpha1', class = "b") +
  brms::prior("constant(1)", nlpar = 'theta1', class = "sd")
get_prior(formula=build_formula(var_specs_3dim, model_specifications = model_specs2pl),
          data = compose_dataset(response_data = eirtdata, response_columns = Math.1:WordProb.10,
                                 item_data = idata2, variable_specifications = var_specs_3dim))

fit_bio_3dim_2pl <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                          item_data = idata2,
                          variable_specifications = var_specs_3dim, refresh = max(iter/20, 1),
                          model_specifications = model_specs2pl_regalphagroups,
                          prior = prio_3dim_2pl,
                          chains = 4, core = 4,
                          iter = iter, warmup = 1000,
                          control = list(adapt_delta = .95)
)

fit_bio_3dim_2pl_singlealphagroup <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                              item_data = idata2,
                              variable_specifications = var_specs_3dim, refresh = max(iter/20, 1),
                              model_specifications = model_specs2pl,
                              prior = prio_3dim_2pl,
                              chains = 4, core = 4,
                              iter = iter, warmup = 1000
)

idata3 <- idata2
idata3 <- idata3 %>% bind_cols(map(unique(idata3$itype), ~(idata3 %>% transmute('{.x}' := ifelse(itype == .x, 1, 0))))) # entspricht der for-Schleife
# for (i in unique(idata3$itype)) {
#   idata3 <- idata3 %>% mutate('{i}' := ifelse(itype == i, 1, 0))
# }

var_specs_3dim_unreg <- list(person = 'id', unregular_dimensions = unique(idata3$itype))
prio_3dim_2pl_unreg <- brms::prior("normal(0, 2)", nlpar = 'skillintercept', class = "b") +
  brms::prior("normal(0, 1)", nlpar = 'logalpha1', class = "b") +
  brms::prior("normal(0, 1)", nlpar = 'logalpha2', class = "b") +
  brms::prior("normal(0, 1)", nlpar = 'logalpha3', class = "b") +
  brms::prior("constant(1)", nlpar = 'theta1', class = "sd") +
  brms::prior("constant(1)", nlpar = 'theta2', class = "sd") +
  brms::prior("constant(1)", nlpar = 'theta3', class = "sd")
fit_bio_3dim_2pl_unregular <- birtm_aio(response_data = eirtdata_frac, response_columns = Math.1:WordProb.10,
                              item_data = idata3,
                              variable_specifications = var_specs_3dim_unreg, refresh = max(iter/20, 1),
                              model_specifications = model_specs2pl,
                              prior = prio_3dim_2pl_unreg,
                              chains = 4, core = 4,
                              iter = iter, warmup = 1000
)

# ------- ordinal ------------

stemcell <- rio::import('interactive_scripts/GSS2006.sav') %>% transmute(belief = relevel(factor(FUND, levels = c(1,2,3),
                                                                                      labels = c('Fundamentalist', 'Moderate', 'Liberal')), ref = 2),
                                                                      rating = fct_rev(factor(SCRESRCH, levels = c(1:4),
                                                                                      labels = c('Definitely should fund such research',
                                                                                                 'Probably should fund such research',
                                                                                                 'Probably should not fund such research',
                                                                                                 'Definitely should not fund such research'), ordered = TRUE)),
                                                                      .before = 1) %>% filter(!is.na(rating) & !is.na(belief))

fit_sc0 <- brm(
  formula = rating ~ 1,
  data = stemcell,
  family = cumulative("probit"),
  core = 4
)

get_prior(data = stemcell, formula = rating ~ 1 + belief)

fit_sc1 <- brm(
  formula = rating ~ 1 + belief,
  data = stemcell,
  family = cumulative("probit"),
  core = 4
)

fit_sc1_log <- brm(
  formula = rating ~ 1 + belief,
  data = stemcell,
  family = cumulative("logit"),
  core = 4
)

conditional_effects( fit_sc1, "belief",
                  categorical = TRUE)
conditional_effects( fit_sc1, "belief",
                  categorical = FALSE)
conditional_effects( fit_sc1,
                     categorical = TRUE)

fit_sc1_num <- brm(
  formula = rating ~ 1 + belief,
  data = stemcell %>% mutate(belief = as.numeric(relevel(belief, ref = 2))),
  family = cumulative("probit"),
  core = 4
)

conditional_effects( fit_sc1_num, "belief",
                  categorical = TRUE)
conditional_effects( fit_sc1_num, "belief",
                  categorical = FALSE)
ce <- conditional_effects( fit_sc1_num,
                     categorical = TRUE)

plot(ce, plot = FALSE)[[1]] +
  scale_x_continuous(expand = c(0, 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)), limits = c(0, NA))

fit_sc2 <- brm(
  formula = rating ~ 1 + cs(belief),
  data = stemcell,
  family = acat("probit")
)

marginal_effects(fit_sc2, categorical = TRUE)

fit_sc4 <- brm(
  formula = bf(rating ~ 1 + belief) +
    lf(disc ~ 0 + belief, cmc = FALSE),
  data = stemcell,
  family = cumulative("probit")
)

#-------

mtcars_clean = mtcars %>%  mutate(cyl = ordered(cyl))

head(mtcars_clean)


m_cyl = brm(
  cyl ~ mpg,
  data = mtcars_clean,
  family = cumulative('probit'),
  seed = 58393
)

conditional_effects(m_cyl, categorical = TRUE)


# ----- poly trace plots -----

library(mirt)
library(tidyverse)
library(pander)

data(Science)

calculate_cumulative_icc <- function(thresholds, b, a, theta = NULL, b_is_easyness = TRUE, mult_all_by_a = FALSE) {
  # parts of this function coming from: https://aidenloe.github.io/irtplots.html

  # to calculate the base category probabilities for its traceplot/icc
  # function derived by Rost (2004, p. 209)
  p0 <- function(thresholds, a, b, theta, b_is_easyness, mult_all_by_a) {
    if(b_is_easyness) {
      sign <- 1
    } else {
      sign <- -1
    }

    sum_exp <- 0
    cum_threshold <- 0
    for (i in seq_along(thresholds)) {
      cum_threshold <- cum_threshold + thresholds[i]
      if (mult_all_by_a) {
        exp_term <- exp(a*(i*theta + sign*cum_threshold))
      } else {
        exp_term <- exp(a*i*theta + sign*cum_threshold)
      }
      sum_exp <- sum_exp + exp_term
    }

    x <- 1 / (1 + sum_exp)
    return(x)
  }

  # to calculate the threshold iccs probabilities
  twopl <- function(a, b, theta, b_is_easyness, mult_all_by_a){
    if(b_is_easyness) {
      sign <- 1
    } else {
      sign <- -1
    }

    if (mult_all_by_a) {
      x <- 1 / (1 + exp(-(a * (theta + sign*b))))
    } else {
      x <- 1 / (1 + exp(-(a*theta + sign*b)))
    }

    return(x)
  }

  if(is.null(theta)) {
    theta <- seq(-4,4,.1)
  }

  if(is.null(a)) {
    a <-  1
  }

  values <- tibble(theta = theta)
  for (i in seq_along(thresholds)) {
    values <- values %>% mutate('tcc{i}' := twopl(a = a, b = thresholds[i], theta, b_is_easyness, mult_all_by_a))
  }

  values <- values %>% mutate(cat1 = p0(thresholds, a, b, theta, b_is_easyness, mult_all_by_a))
  for (i in seq_along(thresholds)) {
    cat_i <- sym(glue::glue('cat{i}'))
    tcc_i <- sym(glue::glue('tcc{i}'))
    values <- values %>% mutate('cat{i+1}' := {{cat_i}}*{{tcc_i}}/(1-{{tcc_i}}))
  }

  return(values)
}

plot_cumulative_icc <- function(itemname = 'dummy', thresholds, b, a, theta = NULL, b_is_easyness = TRUE, mult_all_by_a = FALSE,
                                category_cc = TRUE, threshold_cc = FALSE, misc_lines = FALSE) {
  # parts of this function coming from: https://aidenloe.github.io/irtplots.html

  dat <- calculate_cumulative_icc(thresholds, b, a, theta, b_is_easyness, mult_all_by_a)

  # removes not requested data
  if (!category_cc) {
    dat <- dat %>% select(-starts_with('cat'))
  }
  if (!threshold_cc) {
    dat <- dat %>% select(-starts_with('tcc'))
  }

  longer.format <- dat %>% pivot_longer(cols = c(starts_with('tcc'), starts_with('cat')), names_to = 'curve', values_to = 'measurement')

  # title
  if(category_cc & threshold_cc) {
    types <- 'CCC and TCC'
  } else {
    if(category_cc) types <- 'CCC'
    if(threshold_cc) types <- 'TCC'
  }
  title <- glue::glue('{types} Plot for Item {itemname}')

  # plot chart
  g <- ggplot(longer.format, aes(theta, measurement, colour=curve)) +
    geom_line() +
    ggtitle(title) +
    xlab(expression(theta)) +
    ylab(expression(P(theta))) +
    theme_bw() # +
    # theme(text = element_text(size=16),
    #       axis.text.x=element_text(colour="black"),
    #       axis.text.y=element_text(colour="black"),
    #       legend.title=element_blank())

  if(misc_lines) {
    n <- ncol(dat)-1
    color_pal <- scales::hue_pal()(n)
    offset <- ifelse(category_cc & threshold_cc, n/2+1, 0)

    g <- g + geom_hline(aes(yintercept = 0.5), linetype = "dashed")
    for (i in seq_along(thresholds)) {
      g <- g + geom_vline(aes_(xintercept = thresholds[i]), color=color_pal[offset+i])
    }

  }

  return(g)
}

thresholds <- cfs[3,2:4]

p0 <- function(thresholds, theta, a) {
  sum_exp <- 0
  cum_threshold <- 0
  for (i in seq_along(thresholds)) {
    cum_threshold <- cum_threshold + thresholds[i]
    exp_term <- exp(a*(i*theta-cum_threshold))
    sum_exp <- sum_exp + exp_term
  }

  x <- 1 / (1 + sum_exp)
}

cat0 <- p0(thresholds, theta, a = cfs[3,1])

model <- mirt(Science, 1, itemtype="gpcm", verbose=FALSE)
cfs <- coef(model, IRTpars = TRUE, simplify=TRUE)$items

# 2pl
twopl <- function(a, b, theta){
  1 / (1 + exp(-(a * (theta - b))))}

# theta
theta <- seq(-4,4,.1)

# select item to display OCC
item <- 3

# create Operational characteristic curve
lst <- list()
for(i in 1:3) lst[[i]] <- twopl(a=cfs[item,1], b=cfs[item,i+1], theta=theta)

dat <- data.frame(theta, as.data.frame(lst))
names(dat) <- c('theta', 'b1', 'b2', 'b3')
dat <- dat %>% mutate(cat0 = .env$cat0 ,
                      # cat0 = 1-b1,
                      cat1 = cat0*b1/(1-b1),
                      cat2 = cat1*b2/(1-b2),
                      cat3 = cat2*b3/(1-b3)
)

plot_cumulative_icc(thresholds = cfs[3,2:4], b = 0, a = cfs[3,1], b_is_easyness = FALSE, mult_all_by_a = TRUE)

# wide to long format.
longer.format <- gather(dat,categorials,measurement,b1:cat4)

# # Plot item trace line (mirt package)
# plot(model, type="trace")
#
#
# # Item Parameter Estimates table
# itemPar <- cfs
# pander(itemPar, plain.ascii = TRUE, caption = "Item par estimates")


# plot chart
ggplot(longer.format, aes(theta, measurement, colour=categorials)) +
  geom_line() +
  ggtitle(paste('OCC Plot for Item', rownames(cfs)[item])) +
  xlab(expression(theta)) +
  ylab(expression(P(theta))) +
  geom_vline(aes(xintercept = cfs[item,2]), color='red') +
  geom_vline(aes(xintercept = cfs[item,3]), color="green") +
  geom_vline(aes(xintercept = cfs[item,4]), color='blue') +
  geom_hline(aes(yintercept = 0.5)) + theme_bw() +
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank())

# ---------

data(Science)
itemtype <- 'gpcm'
mod <- mirt(Science,1, itemtype=itemtype, verbose=FALSE)

# select item to display CRC and OCC
itemSelected <- 4

# Extract all items
# Compute the probability trace lines
# Put into a list
traceline <- NULL
for(i in 1:length(Science)){
  extr.2 <- extract.item(mod, i)
  Theta <- matrix(seq(-6,6, by = .1))
  traceline[[i]] <- probtrace(extr.2, Theta)
}

names(traceline) <- paste('item',1:length(traceline))

# rbind traceline
traceline.df <- do.call(rbind, traceline)

# create item names length based on length of theta provided
item <- rep(names(traceline),each=length(Theta))

# put them all together into a dataframe
l.format <- cbind.data.frame(Theta, item, traceline.df)

# wide to long format.
longer.format <- gather(l.format,categorials,measurement,P.1:P.4)
longer.format$item<-as.factor(longer.format$item)

# Selecting items
items <- c("item 1", "item 2", "item 3", "item 4")

item.format <-longer.format[longer.format$item == items[itemSelected],]

# item coefficient
cfs <- coef(mod, IRTpars = TRUE, simplify=TRUE)$items

# 2pl
twopl <- function(a, b, theta){
  1 / (1 + exp(-(a * (theta - b))))}

# theta
theta <- seq(-6,6,.1)

# create Operational characteristic curve
lst <- list()
for(i in 1:3) lst[[i]] <- twopl(a=cfs[itemSelected,1], b=cfs[itemSelected,i+1], theta=theta)

dat <- data.frame(theta, as.data.frame(lst))
names(dat) <- c('Theta', 'b1', 'b2', 'b3')
dat <- dat %>% mutate(cat1 = b1/(b1+b2+b3),
                      cat2 = b2/(b1+b2+b3),
                      cat3 = b3/(b1+b2+b3),
                      cat4 = (1-b1)/(1+b1+b2+b3)
)

# wide to long format.
longer.format <- gather(dat,categorials,measurement,b1:cat4)

# Item Parameter Estimates table
# itemPar <- cfs

# plot chart
ggplot() +
  geom_line(data=longer.format, aes(Theta, measurement, fill=categorials)) +
  geom_line(data=item.format, aes(Theta, measurement, colour = item,fill=categorials)) +
  ggtitle(paste("CRC + OCC Plot:",itemtype)) +
  xlab(expression(theta)) +
  ylab(expression(P(theta))) +
  geom_vline(aes(xintercept = cfs[itemSelected,2]), color='black') +
  geom_vline(aes(xintercept = cfs[itemSelected,3]), color="black") +
  geom_vline(aes(xintercept = cfs[itemSelected,4]), color='black') +
  geom_hline(aes(yintercept = 0.5), color="black") + theme_bw() +
  theme(text = element_text(size=16),
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"),
        legend.title=element_blank(),
        legend.position="hide") +
  geom_text(aes(x = -3.3, y = 0.55, label = "b1")) +
  geom_text(aes(x = -1.5, y = 0.55, label = "b2")) +
  geom_text(aes(x = 1.3, y = 0.55, label = "b3")) +
  geom_text(aes(x = -4, y = 0.75, label = "C1", color="P.1")) +
  geom_text(aes(x = -2, y = 0.58, label = "C2", color="P.2")) +
  geom_text(aes(x = 0.2, y = 0.63, label = "C3", color="P.3")) +
  geom_text(aes(x = 2.5, y = 0.75, label = "C4", color="P.4"))


# ------------- odds ratio ------

X19_12_17_Rohdaten_KV_und_TF <- rio::import("../../../../Tina/Analyse/database_Rohdaten/19-12-17_Rohdaten_KV_und_TF.sav")
full_data <- X19_12_17_Rohdaten_KV_und_TF %>% rename(person = Codenr)
data_kft <- full_data %>% select(KFT_1:KFT_25)

fit_kft_1pl <- rio::import('models/kft_1PL.rds')
y_rep <- rstan::extract(fit_kft_1pl$fit, pars = "log_lik", include = FALSE)

rep <- 500
J <- ncol(data_kft)

kft_list <- NULL
for (i in seq_len(rep)) {
  kft_list[[i]] <- data_kft
}
data_kft_array <- array(data = unlist(kft_list), dim = c(nrow(data_kft), J, rep))

discrepancy_measures <- list()
discrepancy_measures$or <- matrix(data = 0, nrow = rep, ncol = (J^2 - J)/2) %>% as.data.frame()

or_pre <- calculate_odds_ratio(data_kft)

y_rep <- posterior_predict_long(fit_kft_1pl, n_samples = 500)
ppe <- posterior_epred_long(fit_kft_1pl, n_samples = 500)
data_rep <- y_rep %>% select(-response) %>% pivot_wider(names_from = 'item', values_from = 'yrep') %>% select(-person) %>%
  group_by(.draw) %>% group_split(.keep = FALSE)

data_kft_array <- list2array(data_rep)
# data_kft_array <- list2array(kft_list)
or_post <- calculate_odds_ratio(data_kft_array)

or_pre_data <- or_pre %>% t() %>% as_tibble(rownames = ".variable") %>% rename(or_pre = V1)

# OR
# PPP
# slower with loop
# microbenchmark::microbenchmark({
#   dif <- or_post
#   or_pre2 <- as.numeric(or_pre)
#   for(i in 1:length(or_pre2)) {
#     dif[i] <- dif[i]-or_pre2[i]
#   }
# })

# faster via lapply
microbenchmark::microbenchmark({
  or_pre2 <- rep_dataframe(or_pre, nrow(or_post))
  dif <- or_post-or_pre2
})

dif_hdi <- dif %>% gather_variables() %>% mode_hdi(.width = .89) %>% mutate(includes_zero = .lower <= 0 & .upper >= 0,
                                                                            too_high = .lower >= 0+sd(or_pre_data$or_pre/10),
                                                                            too_low = .upper <= 0-sd(or_pre_data$or_pre/10),
                                                                            # prob_too_high = .lower >= 0 & !too_high,
                                                                            # prob_too_low = .upper <= 0 & !too_low
                                                                            )
orPPP <- colMeans(dif>0) %>% as_tibble(rownames = "itempair") %>% arrange(itempair)
ppp_data <- orPPP %>% mutate(itempair = str_remove(itempair, 'ItemPair')) %>% separate(itempair, into = c('i1', 'i2'), convert = TRUE) %>%
  mutate(radius = 0.5, group = seq_along(radius), OneMinorPPP = 1 - value) %>% rename(orPPP = value)
or_data <- dif_hdi %>% left_join(or_pre_data, by = '.variable') %>% mutate(.variable = str_remove(.variable, 'ItemPair')) %>%
  separate(.variable, into = c('i1', 'i2'), convert = TRUE) %>%
  rename(or = .value) %>% mutate(or_eval_z = ifelse(too_high | too_low, NA, scale(or)), or_z = scale(or), width = abs(.upper-.lower)) %>%
  left_join(ppp_data %>% select(i1, i2, orPPP), by = c('i1', 'i2'))
max <- max(or_data$or_eval, na.rm = TRUE)
min <- min(or_data$or_eval, na.rm = TRUE)
limit_fill <- round(max(abs(max), abs(min)), 1)

itemnumber <- length(unique(ppp_data$i1))

# Scatterpie
# Heatmap wesentlich schicker!
# ggplot() + scatterpie::geom_scatterpie(aes(x=i1, y=i2), data=ppp_data, cols=c("orPPP", "OneMinorPPP")) +
#   # scatterpie::geom_scatterpie(aes(x=i2, y=i1), data=ppp_data, cols=c('orPPP', 'OneMinorPPP')) +
#   scale_x_continuous(breaks=1:itemnumber,limits=c(0, itemnumber+1)) +
#   scale_y_continuous(breaks=1:itemnumber+1,limits=c(1, itemnumber+2)) +
#   coord_fixed() + xlab("Item A") + ylab("Item B") + theme(legend.position="none")

# (1) Plot OR-PPP-values
ppp <- ggplot(ppp_data, aes(i1, i2, fill = orPPP, label = orPPP)) +
  scale_x_continuous("Item A", expand=c(0,0), position = "top", breaks = seq(min(ppp_data$i1),max(ppp_data$i1),1)) +
  scale_y_continuous("Item B", expand=c(0,0), breaks = seq(min(ppp_data$i2),max(ppp_data$i2),1)) +
  ggtitle('Odds Ratio PPP') +
  geom_tile(color="black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = .5,
                                limit = c(0, 1), name = "PPP") +
  # geom_text(aes(i1, i2, fill = orPPP), color = "black", size = 3) +
  theme(panel.grid.major = element_blank(), panel.border= element_rect(size=2,color="black", fill=NA), axis.ticks = element_blank()) +
  coord_fixed()

# (2) Plot OR-dif values
# grau zeigt die Items, wo 0 nicht im CI ist, daher sehr angenehm zu interpretieren
# PPP-Plot hilft, die Richtung der Abweichung zu erkennen
# Buchstaben machen PPP-Plot aber vielelicht überflüssig.
# Die meisten Werte können nciht von 0 abgegrenzt werden. Aber auch nicht mit 0 gleichgesetzt werden.
# Sie haben HDI sowohl innerhalb und als auch außerhalb des ROPE.
# Einige sinf klar verschieden von 0. Kein Item hat ald ORdiff mit Sicherheit 0.
or_dif <- ggplot(or_data, aes(i1, i2, fill = or_eval_z, label = or_eval_z, height = 1, width = 1)) +
  scale_x_continuous("Item A", expand=c(0,0), position = "top", breaks = seq(min(or_data$i1),max(or_data$i1),1)) +
  scale_y_continuous("Item B", expand=c(0,0), breaks = seq(min(or_data$i2),max(or_data$i2),1)) +
  ggtitle('z-standardised Odds Ratio difference') +
  geom_tile(color="black") +
  # geom_tile(data = subset(or_data, too_low | too_high), fill = 'gray50', color="black") +
  scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "#f7f7f7",
                                  midpoint = 0, limit = c(-1, 1), name = "z(OR diff)", oob = scales::squish, na.value = 'gray50') +
  # geom_text(aes(item_i, item_j, fill = PPP), color = "black", size = 3) +
  geom_text(data = subset(or_data, too_low), aes(label = 'L'), size = 4) +
  geom_text(data = subset(or_data, too_high), aes(label = 'H'), size = 4) +
  # geom_text(data = subset(or_data, prob_too_high), aes(label = 'h'), size = 4) +
  # geom_text(data = subset(or_data, prob_too_low), aes(label = 'l'), size = 4) +
  theme(panel.grid.major = element_blank(), panel.border= element_rect(size=2, color="black", fill=NA), axis.ticks = element_blank()) +
  coord_fixed()
or_dif

# (3) Combine two plots: OR diff and PPP
gridExtra::grid.arrange(or_dif, ppp, ncol = 2)

# (2) Plot OR values
# kaum Aussagekraft für sich alleion, da keine Verteilung gegeben (und fast alles rot)
ggplot(or_data, aes(i1, i2, fill = or_pre, label = or_pre)) +
  scale_x_continuous("Item A", expand=c(0,0), position = "top", breaks = seq(min(or_data$i1),max(or_data$i1),1)) +
  scale_y_continuous("Item B", expand=c(0,0), breaks = seq(min(or_data$i2),max(or_data$i2),1)) +
  ggtitle('Odds Ratio difference') +
  geom_tile(color="black") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 2.5, limit = c(0, 7), name = "OR") +
  # geom_text(aes(item_i, item_j, fill = PPP), color = "black", size = 3) +
  theme(panel.grid.major = element_blank(), panel.border= element_rect(size=2,color="black", fill=NA), axis.ticks = element_blank()) +
  coord_fixed()

tibble::tribble(
  ~x, ~y,
  0, 1,
  1, 0,
  1, 1,
  0, 0,
  1, 0
) %>% calculate_odds_ratio()

or_data %>% plot_ppmc_or_heatmap()

# ----- odss ratio ci tests -----

or <- function(mat,corr=0) {
    or <- (mat[,1]+corr)*(mat[,2]+corr)/((mat[,3]+corr)*(mat[,4]+corr))
}

or <- function(mat,corr=0) {
  or <- (mat[,1])*(mat[,2])/((mat[,3]+corr)*(mat[,4]))
}

# {
# corrs <- unique(round(seq(1,500,1)/1000,10))
# m <- matrix(round(runif(40000, 0, 1000),0), ncol = 4)
# colnames(m) <- c('c00', 'c11', 'c01', 'c10')
# m_dat <- m %>% as_tibble() %>% mutate(odds = or(m,0))
# for (i in seq_along(corrs)) {
#   odd <- paste0('odds_',round(corrs[i],5))
#   m_dat <- m_dat %>% mutate({{odd}} := odds-or(m,corrs[i]))
# }
# # m_dat <- m_dat %>% filter(is.finite(odds))
# diff <- m_dat %>% select(starts_with('odds_')) %>% tidybayes::gather_variables() %>% group_by(.variable) %>%
#   tidybayes::median_hdi(.width = .5)
# diff <- diff %>% separate(.variable, '_', into = c('prefix', 'num'), convert = TRUE) %>% rename(diff = .value, correction = num) %>% select(-prefix)
# diff %>% ggplot(aes(x= correction, y = diff)) + geom_ribbon(aes(x= correction, ymin = .lower, ymax = .upper), fill='grey70') + geom_line()
# }

# Haldane-Korrektur Simulation
# Je größer die Stichprobe (Anzahl Bearbeitungen <=> Werte in der Kontingenztabelle), desto kleiner der Effekt des Korrekturterms
# Der Fehler wird mit kleinerem Korrektur Term ebenfalls kleiner; aber alles auf einer sehr niedrigen Ebene (wenn die Bearbeitungszahl groß ist)
# hingegen wird die Differenz zum Wert wenn eine 0 eigentlich eine 1 sein würde für kleine Werte exponentiell größer
corrs <- unique(round(seq(1,5000,1)/4000,10))
m1 <- matrix(round(runif(40000, 0, 100),0), ncol = 4)
m <- m1
m[,3] <- 0
m2 <- m1
m2[,3] <- 1

odds_reference <- or(m2, 0)
odds_reference[is.infinite(odds_reference)] <- 1000000
{
c <- map(corrs, .f = ~or(mat = m, corr = .x))# %>% as.data.frame() %>% setNames(paste0('corr_',corrs))
c1 <- map(.x = c, .f = ~(abs(odds_reference - .x)))
c2 <- map(.x = c1, .f = ~median(.x)) %>% as.data.frame() %>% t() %>% `colnames<-`('diff') %>% as_tibble(rownames = 'corr')
c3 <- map(.x = c1, .f = ~HDInterval::hdi(.x, credMass = .89)) %>% as.data.frame() %>% t() %>% `colnames<-`(c('lower89', 'upper89'))
c3b <- map(.x = c1, .f = ~HDInterval::hdi(.x, credMass = .6)) %>% as.data.frame() %>% t() %>% `colnames<-`(c('lower60', 'upper60'))
c3c <- map(.x = c1, .f = ~HDInterval::hdi(.x, credMass = .75)) %>% as.data.frame() %>% t() %>% `colnames<-`(c('lower75', 'upper75'))
c4 <- cbind(c2, c3, c3b, c3c) %>% as_tibble(rownames = NULL)%>% mutate(corr = corrs)
c4 %>% ggplot(aes(x= corr, y = diff)) + geom_ribbon(aes(x= corr, ymin = lower89, ymax = upper89), fill='grey89') +
  geom_ribbon(aes(x= corr, ymin = lower75, ymax = upper75), fill='grey75') +
  geom_ribbon(aes(x= corr, ymin = lower60, ymax = upper60), fill='grey60') +
  geom_line() + coord_cartesian(ylim = c(0,1000))
}

or2 <- function(mat,corr=0,ci=.89) {
  z <- qnorm(ci+(1-ci)/2)
  or <- (mat[,1]+corr)*(mat[,2]+corr)/((mat[,3]+corr)*(mat[,4]+corr))
  se <- sqrt(1/(mat[,1]+corr)+1/(mat[,2]+corr)+1/(mat[,3]+corr)+1/(mat[,4]+corr))
  upper <- exp(log(or)+z*se)
  lower <- exp(log(or)-z*se)

  return(list(or = or, lower = lower, upper = upper, ci = ci))
}

or_ci_uncond <- function(mat, ci = .89) {
  y1 <-  mat[,1] #n11
  y2 <- mat[,4] #n01
  n1 <- mat[,1] + mat[,3] #n11 + n10
  n2 <- mat[,4] + mat[,2] #n01 + n00

  return(PropCIs::orscoreci(y1, n1, y2, n2, ci))
}

# n11, n00, n10, n01
counts <- c(5,4,4,2)

# der Median des bayes mit Jeffrey prior entspricht eher dem OR des inferenzstat.
# Der Zugewinn von Bayes hauptsächlich in der 0 korrektur
# Sonst sind alle Werte sehr ähnlich
or_ci_uncond(matrix(counts, ncol = 4), .95)
# or_ci_uncond2(matrix(counts, ncol = 4), .95)
or_ci_bayes(matrix(counts, ncol = 4), .95, k = 0.5) # Jeffrey
or_ci_bayes(matrix(counts, ncol = 4), .95, k = 1) # uniform
or_ci_bayes_with_median(matrix(counts, ncol = 4), .95, k = 0.5)# Jeffrey
or_ci_bayes_with_median(matrix(counts, ncol = 4), .95, k = 1) # uniform

# vari <- matrix(NA, ncol = 5, nrow = 100) %>% as.data.frame()
# for (i in 1:100) {
#   for (j in 1:5) {
#     res <- or_ci_bayes_with_median(matrix(counts, ncol = 4), .95, k = 0.5, nsim = 100*(10^j))
#     vari[i,j] <- res$median
#   }
# }
#
# vari2 <- vari

# n11, n00, n10, n01
or2(matrix(counts, ncol = 4),0,.95)
or2(matrix(counts, ncol = 4),0.5,.95)

# n11, n00, n10, n01
ORtable<-matrix(c(counts[1],counts[3],counts[4],counts[2]),nrow = 2, ncol = 2)
ORtable

epitools::oddsratio.wald(ORtable, conf.level = .95)
epitools::oddsratio.small(ORtable, conf.level = .95)


# orscoreci2 <-
#   function(x1,n1,x2,n2,conf.level){
#     px = x1/n1
#     py = x2/n2
#     if(((x1==0) && (x2==0)) || ((x1==n1) && (x2==n2))){
#       ul = 1/0
#       ll = 0
#       theta = NaN
#     }
#     else if((x1==0) || (x2==n2)){
#       ll = 0
#       theta = 0.01/n2
#       ul = limit(x1,n1,x2,n2,conf.level,theta,1)
#     }
#     else if((x1==n1) || (x2==0)){
#       ul = 1/0
#       theta = 100*n1
#       ll = limit(x1,n1,x2,n2,conf.level,theta,0)
#     }
#     else{
#       theta = px/(1-px)/(py/(1-py))/1.1
#       ll = limit(x1,n1,x2,n2,conf.level,theta,0)
#       theta=px/(1-px)/(py/(1-py))*1.1
#       ul = limit(x1,n1,x2,n2,conf.level,theta,1)
#       theta = px/(1-px)/(py/(1-py))
#     }
#     cint <- c(ll, ul)
#     attr(cint, "conf.level") <- conf.level
#     rval <- list(conf.int = cint)
#     class(rval) <- "htest"
#     return(list(rval, theta))
#   }

or_ci_uncond2 <- function(mat, ci = .89) {
  x1 <-  mat[,1] #n11
  x2 <- mat[,4] #n01
  n1 <- mat[,1] + mat[,3] #n11 + n10
  n2 <- mat[,4] + mat[,2] #n01 + n00

  return(orscoreci2(x1, n1, x2, n2, ci))
}


p = seq(0,1, length=100)
plot(p, dbeta(p, 100, 100), ylab="density", type ="l", col=4)
lines(p, dbeta(p, 0.5, 0.5), type ="l", col=3)
lines(p, dbeta(p, 2, 2), col=2)
lines(p, dbeta(p, 1, 1), col=1)
legend(0.7,8, c("Be(100,100)","Be(0.5,0.5)","Be(2,2)", "Be(1,1)"),lty=c(1,1,1,1),col=c(4,3,2,1))

limit <-
  function(x,nx,y,ny,conflev,lim,t){

    z = qchisq(conflev,1)
    px = x/nx
    score= 0
    while ( score < z){
      a = ny*(lim-1)
      b = nx*lim+ny-(x+y)*(lim-1)
      c = -(x+y)
      p2d = (-b+sqrt(b^2-4*a*c))/(2*a)
      p1d = p2d*lim/(1+p2d*(lim-1))
      score = ((nx*(px-p1d))^2)*(1/(nx*p1d*(1-p1d))+1/(ny*p2d*(1-p2d)))*(nx+ny-1)/(nx+ny) ## added *(nx+ny-1)/(nx+ny)
      ci = lim
      if(t==0) { lim = ci/1.001 }
      else{ lim = ci*1.001 }
    }
    return(ci)
  }


or_ci_bayes <- function(mat, ci = .89, k) {
  y1 <-  mat[,1] #n11
  y2 <- mat[,4] #n01
  n1 <- mat[,1] + mat[,3] #n11 + n10
  n2 <- mat[,4] + mat[,2] #n01 + n00

  return(PropCIs::orci.bayes(y1, n1, y2, n2, k, k, k, k, ci))
}

or_ci_bayes_with_median <- function(mat, ci = .89, k, nsim = 10000000) {
  y1 <-  mat[,1] #n11
  y2 <- mat[,4] #n01
  n1 <- mat[,1] + mat[,3] #n11 + n10
  n2 <- mat[,4] + mat[,2] #n01 + n00

  return(orci.bayes_with_median(y1, n1, y2, n2, k, k, k, k, ci, nsim))
}

count_to_rate <- function(mat) {
  y1 <-  mat[1] #n11
  y2 <- mat[4] #n01
  n1 <- mat[1] + mat[3] #n11 + n10
  n2 <- mat[4] + mat[2] #n01 + n00

  return(c(y1, n1, y2, n2))
}

orci.bayes_with_median <- function(x1,n1,x2,n2,a,b,c,d,conf.level=0.95, nsim = 10000000)
{
  or.app<- function(a1,b1,c1,d1,conf.level,nsim=nsim)
  {
    z1 <- rf(nsim, 2*a1,2*b1)
    z2 <- rf(nsim, 2*c1,2*d1)
    a <- (d1/c1)/(b1/a1)
    z <- a*z1/z2
    z <- sort(z)
    return(z)
  }


  # Bayes tail interval with beta priors

  fct.F<- function(x,t,a1,b1,a2,b2){
    c <- (b2/a2)/(b1/a1)
    df(x,2*a2,2*b2)*pf(x*t/c,2*a1,2*b1)
  }

  or.F <- function(t,a1,b1,a2,b2)
  {
    return(integrate(fct.F,0,Inf,t=t,a1=a1,b1=b1,a2=a2,b2=b2)$value)
  }

  or.fct <- function(ab,a1,b1,c1,d1,conf.level)
  {
    abs(or.F(ab[2],a1,b1,c1,d1) - (1 - (1-conf.level)/2))+
      abs(or.F(ab[1],a1,b1,c1,d1) - (1-conf.level)/2)
  }

  # browser()

  if(x2!=n2){
    a1 <- a + x1
    b1 <- b + n1 - x1
    c1 <- c + x2
    d1 <- d + n2 - x2
    z <- or.app(a1,b1,c1,d1,conf.level,nsim)

    med <- median(z)
    lq <- nsim * (1-conf.level)/2
    uq <- nsim * (1 - (1-conf.level)/2)
    ci <- array(0,2)
    ci[1] <- z[lq]
    ci[2] <- z[uq]
    start <- ci
    tailci <- optim(start,or.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                    conf.level=conf.level,control=list(maxit=20000))$par
    if(tailci[1] < 0) tailci[1]  <- 0
    }
  else{
    a1 <- a + n1 - x1
    b1 <- b +  x1
    c1 <- c + n2 - x2
    d1 <- d + x2
    z <- or.app(a1,b1,c1,d1,conf.level,nsim)

    med <- median(1/z)
    lq <- nsim * (1-conf.level)/2
    uq <- nsim * (1 - (1-conf.level)/2)
    ci <- array(0,2)
    ci[1] <- z[lq]
    ci[2] <- z[uq]
    start <- ci
    tailci1 <- optim(start,or.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                     conf.level=conf.level,control=list(maxit=20000))$par
    # if(tailci1[1] < 0) tailci1[1]  <- ci[1]
    tailci <- array(0,2)
    tailci[1] <- 1/ tailci1[2]
    tailci[2] <- 1/ tailci1[1]

  }
  return(list(ci = tailci, median = med#, ci2 = c(1/ci[[2]], 1/ci[[1]]), mode = 1/get_mode(z), mean = 1/mean(z)
              )
         )
}

# ------ Mode tests- -----
library(modeest)

# modeest::hsm() works the way a mode is assumed to work in my eyes
allmodes <- function(z) {
  c(asselin(z),
    #grenander(x),
    #hrm(x),
    hsm(z),
    #lientz(z),
    #meanshift(z),
    shorth(z),
    #naive(z),
    #parzen(z),
    #tsybakov(x),
    venter(z),
    #vieu(z)
    get_mode(z)
    )
}

# ------ tyrcatch ------

log_list <- list()
suppressWarnings(
withCallingHandlers({
  for (i in 1:10) {
    x <- or_data3 %>% filter(item1 == 1, item2 == 2) %>% unnest(or_dif_samples) %>% hsm_hdi(or_dif)
    }
  }, warning = function(w) {log_list <<- c(log_list, w)})
)
w <- log_list %>% unlist() %>% unique()
warning(w)



aggregate_warnings({
  for (i in 1:10) {
    x <- or_data3 %>% filter(item1 == 1, item2 == 2) %>% unnest(or_dif_samples) %>% hsm_hdi(or_dif)
  }
  y <- 2
})
