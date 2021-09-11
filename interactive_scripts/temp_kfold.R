library(future)
options(mc.cores=4)
plan(multisession)

# kfol1 + kfold2 brauchten 6:17 min
kfold1 <- brms::kfold(fit_1d_1pl_spm_full1, chains = 1)
kfold2 <- brms::kfold(fit_1d_1pl_spm_full1, chains = 1,
                      folds = "grouped", group = "person")

loo1 <- brms::loo(fit_1d_1pl_spm_full1)
# loo_marginal brauchte 40 s
loo2 <- birtms::loo_marginal(fit_1d_1pl_spm_full1)

kfold_1PL_cond <- kfold1
kfold_1PL_marg <- kfold2
loo_1PL_cond <- loo1
loo_1PL_marg <- loo2

ic_1pl <- list(kfold_1PL_cond, kfold_1PL_marg, loo_1PL_cond, loo_1PL_marg)

saveRDS(ic_1pl, "models/gdcp/ic_1pl_spm.rds")
