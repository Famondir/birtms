R2_latent_birtms <- function(birtms_fit) {
  fit <- birtms_fit
  var_cor <- brms::VarCorr(fit, summary = FALSE)
  beta_all <- brms::fixef(fit, summary = FALSE) %>% as.matrix()

  # Y_all <- data.frame(make_standata(data = fit$data, formula = fit$formula)[['X']]) %>% as.matrix()
  # variance_all <- tibble(n_sample = c(1:nsamples(fit)))
  # for (i in seq_along(var_cor)) {
  #   if (length(var_cor[[i]]) == 1) variance_all <- variance_all %>% mutate('{names(var_cor)[i]}' := (var_cor[[i]][['sd']] %>% as.numeric())^2)
  #   else variance_all <- variance_all %>% mutate((var_cor[[i]][['sd']] %>% as.data.frame())^2)
  # }
  # variance_all <- variance_all %>% select(-n_sample) #%>% rowwise() %>% mutate(total = sum(c_across(where(is.numeric))))
  #
  # for (i in 1:ncol(variance_all)) {
  #   name <- names(variance_all)[i]
  #   R2[[name]] <- R2_latent(variance_all[i], beta_all, Y_all)
  # }

  person <- fit$var_specs$person
  item <- fit$var_specs$item

  person_covars <- fit$var_specs[str_detect(names(fit$var_specs), 'person_covariables') | str_detect(names(fit$var_specs), 'situation_covariables')] %>% unlist(use.names = FALSE)
  item_covars <- fit$var_specs[str_detect(names(fit$var_specs), 'item_covariables') | str_detect(names(fit$var_specs), 'situation_covariables')] %>% unlist(use.names = FALSE)

  beta_person <- beta_all %>% as_tibble() %>% select(Intercept, map(person_covars, starts_with, vars = colnames(.)) %>% unlist()) %>% as.matrix()
  Y_person <- tibble('{person}' := fit$data[[person]]) %>% mutate(as.data.frame(make_standata(data = fit$data, formula = fit$formula)[['X']])) %>%
    group_by_(person) %>% summarise_all(~ median(as.numeric(.x))) %>% ungroup() %>% select(-!!person) %>%
    select(colnames(beta_person)) %>% as.matrix()

  beta_item <- beta_all %>% as_tibble() %>% select(Intercept, map(item_covars, starts_with, vars = colnames(.)) %>% unlist()) %>% as.matrix()
  Y_item <- tibble('{item}' := fit$data[[item]]) %>% mutate(as.data.frame(make_standata(data = fit$data, formula = fit$formula)[['X']])) %>%
    group_by_(item) %>% summarise_all(~ median(as.numeric(.x))) %>% ungroup() %>% select(-!!item) %>%
    select(colnames(beta_item)) %>% as.matrix()

  variance_person <- (var_cor[[person]]$sd %>% as.data.frame())^2
  variance_item <- (var_cor[[item]]$sd %>% as.data.frame())^2

  R2 <- list()
  for (i in 1:ncol(variance_person)) {
    name <- glue::glue('{person}.{colnames(variance_person)[i]}')
    R2[[name]] <- R2_latent(variance_person[i], beta_person, Y_person)
  }

  for (i in 1:ncol(variance_item)) {
    name <- glue::glue('{item}.{colnames(variance_item)[i]}')
    R2[[name]] <- R2_latent(variance_item[i], beta_item, Y_item)
  }

  return(R2)
}

R2_latent <- function(variance, beta, Y) {
  calc_latent_regression_coefs <- function(variance, beta, Y) # extracted from tam_latent_regression_standardized_solution
  {
    res <- NULL

    N <- nrow(Y)
    ND <- ncol(beta)
    Y_exp <- matrix(0, nrow=N, ncol=ND)
    var_y_exp <- rep(NA,ND)

    for (dd in 1:ND){
      Y_exp[,dd] <- Y %*% beta[,dd]
      var_y_exp[dd] <- stats::var( Y_exp[,dd] )
    }

    sd_theta <- sqrt( var_y_exp + diag(variance) )
    R2_theta <- var_y_exp / sd_theta^2

    #--- output
    res <- list(R2_theta=R2_theta, sd_theta=sd_theta)

    return(res)
  }

  R2_vec <- rep(NA, nrow(beta))
  sd_vec <- rep(NA, nrow(beta))

  for (i in seq_along(beta[,1])) {
    temp <- calc_latent_regression_coefs(variance[i,] %>% as.matrix(), beta[i,] %>% as.matrix(), Y)
    R2_vec[i] <- temp$R2_theta
    sd_vec[i] <- temp$sd_theta
  }

  if(nrow(beta) > 1) return(list(R2 = tidybayes::median_hdi(R2_vec), sd_theta = tidybayes::median_hdi(sd_vec)))
  return(list(R2_theta = R2_vec, sd_theta = sd_vec))
}
