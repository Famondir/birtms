library(brms)
library(rlang)

irt_data <- readRDS('../irt_data.RDS')

f <- expr(response ~ beta + theta1)
f_list <- exprs(response ~ beta + theta1, theta1 ~ 0 + (1| person), beta ~ 1 + (1 | item))

formula <- bf(
  f_list[[1]],
  nl = TRUE,
  flist = f_list[-1],
  family = brmsfamily("bernoulli", link = "logit")
)

iter = 1000

fit_1PL_1D_expr_list3 <- brm(formula = formula,
                                                  data = irt_data,
                                                  chains = 2,
                                                  iter = iter,
                                                  warmup = 500,
                                                  cores = 2,
                                                  refresh = max(1, iter/100)
)

# exp(alpha + gamma)
# expr_print(f)
# expr(!!f + gamma)
#
# f2 <- update.formula(f, ~ . + x2)
#
# f[[3]][[1]]
# ast(!!f)
#
# expr(`=`(nl, TRUE))
