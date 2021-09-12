library(future)
options(mc.cores=3)
plan(multisession)

fit_1d_1pl_spm <- readRDS("models/gdcp/fit_1d_1pl_spm_full1b.rds")

# kfol1 + kfold2 brauchten 6:17 min
# kfold1 <- brms::kfold(fit_1d_1pl_spm_full1, chains = 1)
# kfold2 <- brms::kfold(fit_1d_1pl_spm_full1, chains = 1,
#                       folds = "grouped", group = "person")
kfold3 <- brms::kfold(fit_1d_1pl_spm, chains = 1,
                      folds = "grouped", group = "person",
                      K = length(unique(fit_1d_1pl_spm$data$person)))
# 150 Modelle in 1 h; nach 16 h bei 466 von 499 Modellen
# 471 nach 16.5 h
# Arbeitsspeicher vollgelaufen

saveRDS(kfold3, "models/gdcp/kfold3.rds")
#
# loo1 <- brms::loo(fit_1d_1pl_spm_full1)
# # loo_marginal brauchte 40 s
# loo2 <- birtms::loo_marginal(fit_1d_1pl_spm_full1)
#
# kfold_1PL_cond <- kfold1
# kfold_1PL_marg <- kfold2
# loo_1PL_cond <- loo1
# loo_1PL_marg <- loo2
#
# ic_1pl <- list(kfold_1PL_cond, kfold_1PL_marg, loo_1PL_cond, loo_1PL_marg)
#
# saveRDS(ic_1pl, "models/gdcp/ic_1pl_spm.rds")
