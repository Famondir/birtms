#' marginal_loglik
#' Calculates the marginal loglikelihood for an IRT model
#'
#' @param fit birtmsfit object
#' @param n_nodes int; number of nodes where the loglik gets evaluated; some models need more nodes to reach convergence of loglik
#' @param cores int; number of CPU cores used for parallel processing
#'
#' @return list
#' @export
#'
#' @examples
marginal_loglik <- function(fit, n_nodes = 11, cores = 4) {
  mll_fun <- choose_mll_fun(fit)

  mll_fun(fit = fit, n_nodes = n_nodes, cores = cores)
}

#' Caclulates marginal loo
#'
#' @param fit birtmsfit object
#' @param ... parameters passed to birms::marginal_loglik
#'
#' @return loo
#' @export
#'
#' @examples
loo_marginal <- function(fit, ...) {
  ll_marginal <- marginal_loglik(fit, ...)

  return(brms::loo(ll_marginal$ll, r_eff = loo::relative_eff(ll_marginal$ll, ll_marginal$chain)))
}

#' Checks marginal loglik for different numbers of nodes
#'
#' @param fit birtmsfit object
#' @param min_nodes int; minimal nodenumber is 2^(min_nodes+1)+1
#' @param max_nodes int; maximum nodenumber is 2^(max_nodes+1)+1
#' @param cores int; number of CPU cores used for parallel processing
#'
#' @return list to use with birtms::plot_check_n_nodes
#' @export
#'
#' @examples
check_n_nodes <- function(fit, min_nodes = 1, max_nodes = 8, cores = 4) {
  mll_fun <- choose_mll_fun(fit)

  person <- sym(fit$var_specs$person)

  if (fit$model_specs$item_parameter_number == 1) {
    sd_person__Intercept <- sym(paste0('sd_', {{person}}, '__Intercept'))
  } else if (fit$model_specs$item_parameter_number == 2) {
    sd_person__Intercept <- sym(paste0('sd_', {{person}}, '__theta_Intercept'))
  }

  chain <- fit %>% tidybayes::spread_draws(!!sd_person__Intercept) %>% pull(.chain)
  results <- tibble::tibble("n_nodes" = rep(NA_integer_, max_nodes),
                            "elpd" = rep(NA_real_, max_nodes),
                            "n_greater_0.7" = rep(NA_integer_, max_nodes),
                            "n_greater_1.0" = rep(NA_integer_, max_nodes))

  # marg_lls <- list()
  loos <- list()
  for (i in max_nodes:min_nodes) {
    n_nodes = 2^(1+i)+1
    timestamp()
    print(paste0("Iteration: ",max_nodes+1-i,", n_nodes: ", n_nodes))

    ll_marg <- mll_fun(fit, n_nodes = n_nodes, cores = cores)

    loo_ll_marg_brms <- loo(ll_marg$ll, r_eff = loo::relative_eff(ll_marg$ll, chain))
    results[i,1] <- n_nodes
    results[i,2] <- loo_ll_marg_brms$estimates[1,1]
    results[i,4] <- sum(loo_ll_marg_brms$diagnostics$pareto_k>1)
    results[i,3] <- sum(loo_ll_marg_brms$diagnostics$pareto_k>.7) - results[i,4]

    # marg_lls[i] <- list(ll_marg)
    loos[i] <- list(loo_ll_marg_brms)
  }

  results$loo <- loos

  return(results)
}

#' Plots the marginal loglik and number of problematic pareto k values for different numbers of nodes
#'
#' @param check_n_nodes_object list from birtms::check_n_nodes
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_check_n_nodes <- function(check_n_nodes_object) {
  res <- check_n_nodes_object
  res$loo <- NULL

  ylim1 <- c(min(res$elpd), max(res$elpd))
  ylim2 <- c(min(c(res$n_greater_0.7, res$n_greater_1.0)), max(c(res$n_greater_0.7, res$n_greater_1.0)))
  b <- diff(ylim1)/diff(ylim2)
  a <- ylim1[1] - b*ylim2[1]

  res %>%
    pivot_longer(names_to = "criteria", values_to = "prob_k", cols = -(n_nodes:elpd)) %>%
    ggplot(aes(x=n_nodes)) +
    geom_line(aes(y = elpd, color = "elpd")) +
    geom_line(aes(y = a + prob_k * b, color = criteria)) +
    geom_point(aes(y = elpd)) +
    geom_point(aes(y = a + prob_k*b, color = criteria)) +
    scale_y_continuous(
      # Features of the first axis
      name = "loo elpd",
      # Add a second axis and specify its features
      sec.axis = sec_axis(~ (. - a)/b, name="number of problematic pareto k's")
    ) + theme(legend.position="bottom") +
    scale_color_manual(values=c("black", "blue", "red")) #+  coord_cartesian(ylim = ylim1)
}

choose_mll_fun <- function(fit) {
  stopifnot(fit$model_specs$response_type == 'dichotom')
  stopifnot(fit$model_specs$add_common_dimension == FALSE)
  stopifnot(fit$model_specs$dimensinality_type == 'unidimensional')

  # choosing suiting fnction depending on itemparameternumber
  if (fit$model_specs$item_parameter_number == 1) {
    mll_fun <- mll_parallel_brms_1pl
  } else if (fit$model_specs$item_parameter_number == 2) {
    mll_fun <- mll_parallel_brms_2pl
  } else stop('Currently only 1pl and 2pl models are supported.')

}

mll_parallel_brms_1pl <- function(fit, n_nodes = 11, best_only = FALSE, cores = 4) {

  MFUN_brms_1pl <- function(node, r, iter, data_list2, draws2, linear_terms) {
    #browser()

    resp_numbers <- data_list2$resp_number[data_list2$person_number == r]
    y <- data_list2$response[resp_numbers]
    base_term <- linear_terms[iter, resp_numbers] - draws2$theta[[iter, r]]

    p2 <- brms::inv_logit_scaled(matrix(rep(base_term, length(node)), nrow = length(node), byrow = TRUE) + node)
    rowSums(dbinom(matrix(rep(y, length(node)), nrow = length(node), byrow = TRUE), 1, p2, log = TRUE))
  }

  # ----- create a temporary logging file ----
  logFile <- tempfile()
  if (rstudioapi::isAvailable()) {
    viewer <- getOption("viewer")
    viewer(logFile)
  }

  # ----- initialise multiple workers ----
  cl <- parallel::makeCluster(cores, outfile = logFile)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl)) # terminate workes when finished

  person <- sym(fit$var_specs$person)
  sd_person__Intercept <- sym(paste0('sd_', {{person}}, '__Intercept'))
  r_person <- sym(paste0('r_',{{person}}))
  r_person_vec <- str2lang(paste0({{r_person}},'[',{{person}},',]'))

  # we get the dataset from the brms fit and add row and person numbers instead of passing it to the function
  data_list2 <- fit$data %>% mutate(resp_number = row_number(),
                                    person_number = as.integer(factor({{person}}, levels = unique({{person}}))))

  draws2 <- list(sd = tidybayes::spread_draws(fit, !!sd_person__Intercept) %>%
                   rename(sd_person = {{sd_person__Intercept}}) %>%
                   select(!starts_with('.')) %>% as.matrix(),
                 theta = tidybayes::spread_draws(fit, !!r_person_vec) %>%
                   pivot_wider(values_from = {{r_person}}, names_from = {{person}}) %>%
                   select(!starts_with('.')) %>% as.matrix()
  )

  n_iter2 <- nsamples(fit)

  post_means2 <- map(draws2, ~matrix(colMeans(.), nrow = 1))

  # Seperate out draws for residuals and their SD
  resid2 <- ranef(fit)[[{{person}}]][,1,1]
  stddev2 <- ranef(fit)[[{{person}}]][,2,1]

  n_persons <- length(resid2)

  # Get standard quadrature points
  std_quad <- statmod::gauss.quad.prob(n_nodes, "normal", mu = 0, sigma = 1)
  std_log_weights <- log(std_quad$weights)

  linear_terms <- fitted(fit, scale = 'linear', summary = FALSE)
  linear_terms_mean <- matrix(colMeans(linear_terms), nrow = 1)

  start = 1
  if (best_only) start = n_iter2

  # Extra iteration is to evaluate marginal log-likelihood at parameter means.
  ll <- foreach::`%dopar%`(foreach::foreach(i = start:(n_iter2 + 1), .combine = rbind,
                                            .packages = "matrixStats"
  ),
  {
    my_options <- options(digits.secs = 3)
    on.exit(options(my_options))

    if(i %% 100 == 0 ) {
      print(paste(i, "/", n_iter2, ":", strptime(Sys.time(), "%Y-%m-%d %H:%M:%OS") ))
    }

    ll_j <- matrix(NA, nrow = 1, ncol = n_persons)

    for(j in 1:n_persons) {

      # Set up adaptive quadrature using SD for residuals either from draws or
      # posterior mean (for best_ll).
      sd_i <- ifelse(i <= n_iter2, draws2$sd[[i]], post_means2$sd[[1]])
      adapt_nodes <- resid2[[j]] + stddev2[[j]] * std_quad$nodes
      log_weights <- log(sqrt(2*pi)) + log(stddev2[[j]]) + std_quad$nodes^2/2 +
        dnorm(adapt_nodes, sd = sd_i, log = TRUE) + std_log_weights

      # Evaluate mll with adaptive quadrature. If at n_iter + 1, evaluate
      # marginal likelihood at posterior means.
      if(i <= n_iter2) {
        loglik_by_node <- MFUN_brms_1pl(adapt_nodes,  r = j, iter = i,
                                        data_list = data_list2, draws = draws2, linear_terms = linear_terms)

        weighted_loglik_by_node <- loglik_by_node + log_weights
        ll_j[1,j] <- matrixStats::logSumExp(weighted_loglik_by_node)
      } else {
        loglik_by_node <- MFUN_brms_1pl(adapt_nodes,  r = j, iter = 1,
                                        data_list = data_list2, draws = post_means2, linear_terms = linear_terms_mean)
        weighted_loglik_by_node <- loglik_by_node + log_weights
        ll_j[1,j] <- matrixStats::logSumExp(weighted_loglik_by_node)
      }

    }

    ll_j

  })

  chain <- fit %>% tidybayes::spread_draws({{sd_person__Intercept}}) %>% dplyr::pull(.chain)

  if(best_only) {
    return(ll[nrow(ll), ])
  } else {
    return(list(ll = ll[-nrow(ll), ], best_ll = ll[nrow(ll), ], chain = chain))
  }

}

mll_parallel_brms_2pl <- function(fit, MFUN, n_nodes = 11, best_only = FALSE, cores = 4) {

  MFUN_brms_2pl <- function(node, person, iter, data_list2, draws2, linear_terms, alphas) {

    resp_numbers <- data_list2$resp_number[data_list2$person_number == person]
    items <- data_list2$item_number[resp_numbers]

    y <- data_list2$response[resp_numbers]
    base_term <- linear_terms[iter, resp_numbers] - alphas[iter,items]*draws2$theta[[iter, person]] # get information which item was edited

    p2 <- brms::inv_logit_scaled(matrix(rep(base_term, length(node)), nrow = length(node), byrow = TRUE) + node %*% t(alphas[iter,items]))
    rowSums(dbinom(matrix(rep(y, length(node)), nrow = length(node), byrow = TRUE), 1, p2, log = TRUE))
  }

  # ----- create a temporary logging file ----
  logFile <- tempfile()
  if (rstudioapi::isAvailable()) {
    viewer <- getOption("viewer")
    viewer(logFile)
  }

  # ----- initialise multiple workers ----
  cl <- parallel::makeCluster(cores, outfile = logFile)
  doParallel::registerDoParallel(cl)
  on.exit(parallel::stopCluster(cl)) # terminate workes when finished

  person <- sym(fit$var_specs$person)
  sd_person__Intercept <- sym(paste0('sd_', {{person}}, '__theta_Intercept'))
  r_person <- sym(paste0('r_',{{person}},'__theta'))
  r_person_vec <- str2lang(paste0({{r_person}},'[',{{person}},',]'))

  # we get the dataset from the brms fit and add row and person numbers instead of passing it to the function
  data_list2 <- fit$data %>% mutate(resp_number = row_number(),
                                    person_number = as.numeric(factor({{person}}, levels = unique({{person}}))))

  draws2 <- list(sd = tidybayes::spread_draws(fit, !!sd_person__Intercept) %>%
                   rename(sd_person = {{sd_person__Intercept}}) %>%
                   select(!starts_with('.')) %>% as.matrix(),
                 theta = tidybayes::spread_draws(fit, !!r_person_vec) %>%
                   pivot_wider(values_from = {{r_person}}, names_from = {{person}}) %>%
                   select(!starts_with('.')) %>% as.matrix()
  )

  n_iter2 <- nsamples(fit)

  post_means2 <- purrr::map(draws2, ~matrix(colMeans(.), nrow = 1))

  # Seperate out draws for residuals and their SD
  resid2 <- ranef(fit)[[{{person}}]][,1,1]
  stddev2 <- ranef(fit)[[{{person}}]][,2,1]

  n_persons <- length(resid2)

  # Get standard quadrature points
  std_quad <- statmod::gauss.quad.prob(n_nodes, "normal", mu = 0, sigma = 1)
  std_log_weights <- log(std_quad$weights)

  linear_terms <- fitted(fit, scale = 'linear', summary = FALSE)
  linear_terms_mean <- matrix(colMeans(linear_terms), nrow = 1)

  itemparams <- fit %>% tidybayes::spread_draws(#r_item__beta[item,],
    r_item__logalpha[item,], b_logalpha_Intercept) %>%
    mutate(r_item__alpha = exp(b_logalpha_Intercept + r_item__logalpha)) %>% as.data.frame()

  # safer version (better trackable); gets just executed once, so speed isn't so important at all
  alphas <- itemparams %>% select(item, r_item__alpha, .draw) %>%
    pivot_wider(names_from = item, values_from = r_item__alpha) %>% select(-.draw) %>% as.matrix()

  # alphas <- matrix(itemparams$r_item__alpha, nrow = n_iter2) # faster version (3 vs 20 ms)
  # colnames(alphas) <- unique(itemparams$item)

  alphas_mean <- matrix(colMeans(alphas), nrow = 1)
  colnames(alphas_mean) <- colnames(alphas)

  item_key <- 1:length(colnames(alphas))
  names(item_key) <- colnames(alphas)
  data_list2 <- data_list2 %>% mutate(item_number = item_key[item])

  start = 1
  if (best_only) start = n_iter2

  # Extra iteration is to evaluate marginal log-likelihood at parameter means.
  ll <- foreach::`%dopar%`(foreach::foreach(i = start:(n_iter2 + 1), .combine = rbind,
                                            .packages = "matrixStats"
  ),
  {
    my_options <- options(digits.secs = 3)
    on.exit(options(my_options))

    if(i %% 100 == 0 ) {
      print(paste(i, "/", n_iter2, ":", strptime(Sys.time(), "%Y-%m-%d %H:%M:%OS") ))
    }

    ll_j <- matrix(NA, nrow = 1, ncol = n_persons)

    for(j in 1:n_persons) {

      # Set up adaptive quadrature using SD for residuals either from draws or
      # posterior mean (for best_ll).
      sd_i <- ifelse(i <= n_iter2, draws2$sd[[i]], post_means2$sd[[1]])
      adapt_nodes <- resid2[[j]] + stddev2[[j]] * std_quad$nodes
      log_weights <- log(sqrt(2*pi)) + log(stddev2[[j]]) + std_quad$nodes^2/2 +
        dnorm(adapt_nodes, sd = sd_i, log = TRUE) + std_log_weights

      # Evaluate mll with adaptive quadrature. If at n_iter + 1, evaluate
      # marginal likelihood at posterior means.
      if(i <= n_iter2) {
        loglik_by_node <- MFUN_brms_2pl(node = adapt_nodes,  person = j, iter = i,
                                        data_list = data_list2, draws = draws2, linear_terms = linear_terms,
                                        alphas = alphas)

        weighted_loglik_by_node <- loglik_by_node + log_weights
        ll_j[1,j] <- matrixStats::logSumExp(weighted_loglik_by_node)
      } else {
        loglik_by_node <- MFUN_brms_2pl(adapt_nodes,  person = j, iter = 1,
                                        data_list = data_list2, draws = post_means2, linear_terms = linear_terms_mean,
                                        alphas = alphas_mean)
        weighted_loglik_by_node <- loglik_by_node + log_weights
        ll_j[1,j] <- matrixStats::logSumExp(weighted_loglik_by_node)
      }

    }

    ll_j

  })

  chain <- fit %>% tidybayes::spread_draws({{sd_person__Intercept}}) %>% dplyr::pull(.chain)

  if(best_only) {
    return(ll[nrow(ll), ])
  } else {
    return(list(ll = ll[-nrow(ll), ], best_ll = ll[nrow(ll), ], chain = chain))
  }
}


