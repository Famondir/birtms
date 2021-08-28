get_ppmcdatasets <- function(model, ppmcMethod, crit, group = 'item', post_responses = NULL, sd = 1) {
  if (is.null(post_responses)) {
    post_responses <- get_postdata(model = model, subset = model$subset)
  }

  ppe <-  make_post_longer(model = model, postdata = post_responses, 'ppe') %>%
    mutate(item.id = get.item.id(.))

  item_key <- ppe %>% select(item.id, item) %>% group_by(item) %>% summarise_all(mean) %>% ungroup()
  key <- item_key$item.id %>% as.integer()
  names(key) <- item_key$item

  if (ppmcMethod == 'C' | ppmcMethod == 'all') {

    yrep <-  make_post_longer(model = model, postdata = post_responses, 'yrep')
    ppe2 <- ppe
  } else if (ppmcMethod == 'M' | ppmcMethod == 'all') {
    temp <- get.mixed_ppmc_data(model, subset = model$subset, ppmcMethod = ppmcMethod, sd = sd) %>%
      mutate(item.id = key[item]) %>% arrange(.draw, item.id)

    ppe <- ppe %>% arrange(.draw, item.id)

    ppe2 <- list()
    yrep <- list()

    ppe2$ppe = temp$ppe
    yrep$yrep = temp$yrep
  } else if (ppmcMethod == 'MM' # | ppmcMethod == 'all'
             ) {
    stop('Mixed PPMC for testlet models not implemented yet.')

    # if (is.null(model$ppmcData$ppe_mm)) {
    #   model$ppmcData$ppe_mm <- get.ppe_ppmc(model, subset = model$subset, ppmcMethod = ppmcMethod)
    #   model$ppmcData$yrep_mm <- get.yrep_ppmc(model$ppmcData$ppe_mm)
    # }
    #
    # ppe = model$ppmcData$ppe_mm
    # yrep = model$ppmcData$yrep_mm
  } else {
    stop('Fehler. Ungültige PPMC Methode!')
  }

  data <- ppe %>% mutate(yrep = yrep$yrep, ppe2= ppe2$ppe) # should be sorted in the right order so no join needed

  # prevents recalculation for fitdatasets,
  # scine this function is called twice, cause model is altered in observe function
  # print(calculated_ppmc_datasets)
  if (group == 'item') {
    fitData <- fit_statistic(criterion = crit, group = item, data = data)
  } else if (group == 'ID') {
    fitData <- fit_statistic(criterion = crit, group = ID, data = data)
  }

  return(fitData)
}

fit_statistic <- function(criterion, group, data) {
  group <- enquo(group)

  message('calculating fitstatistic')

  fitdata <- data %>%
    mutate(
      crit = criterion(response, ppe, .),
      crit_rep = criterion(yrep, ppe2, .)
    ) %>%
    group_by(!!group, .draw) %>%
    summarise(
      crit = sum(crit),
      crit_rep = sum(crit_rep),
      crit_diff = crit_rep - crit,
      .groups = 'drop'
    ) %>%
    mutate(draw = as.numeric(sub("^V", "", .draw))) %>%
    arrange(!!group, .draw)

  print('finished')

  return(fitdata)
}

infit <- function(y, p, data, ...) {
  sum_var_p <- data %>% mutate(var_p = (ppe*(1-ppe))) %>% group_by(item, draw) %>%
    mutate(sum_var = sum(var_p)) %>% ungroup() %>% select(sum_var)

  (y - p)^2 / sum_var_p %>%
    return()
}

outfit <- function(y, p, data, ...) {
  N <- length(unique(data$ID))

  (y - p)^2 / N / (p*(1-p)) %>%
    return()
}

ll <- function(y, p, ...) {
  y * log(p) + (1 - y) * log(1 - p) %>%
    return()
}

Q1 <- function(y, p, data, ...) {
  stop('Q1 not implemented yet')
}

G2 <- function(y, p, data, ...) {
  stop('G2 not implemented yet')
}

plot_fit_statistic <- function(model, data, units = c(1,9), group = 'item', ppmcMethod = 'C', hdi_width = .89) {
  if (ppmcMethod == 'C') {
    color = "#8b7d6b70"
  } else if (ppmcMethod == 'M') {
    color = "#008b4570"
  } else if (ppmcMethod == 'MM') {
    color = "#ff634770"
  }

  person <- model$var_specs$person
  personsym <- sym(person)

  if (group == 'item') {
    g <- data %>% mutate(item_id = get.item.id(.)) %>% filter(item_id <= units[2] & item_id >= units[1]) %>%
      group_by(item) %>%
      ggplot(aes(x = crit_diff, y = 0, fill = stat(quantile))) +
      ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi_custWidth, quantiles = hdi_width, vline_linetype = 2) +
      # geom_density(data = item_fit2_1pl_testlets_mm[1:(1600*9),], aes(crit_diff), colour = 'steelblue1', fill = 'steelblue1', alpha = 0.3) +
      facet_wrap("item", scales = "free") +
      scale_fill_manual(values = c("transparent", color, "transparent"), guide = "none") +
      xlab("itemfit criteria difference between predicted and observed responses.")
  } else if (group == person) {
    g <- data %>% mutate(person_id = get.person.id(., person = {{person}})) %>%
      filter(person_id <= units[2] & person_id >= units[1]) %>%
      mutate({{person}} := paste('Person', {{personsym}})) %>% group_by({{personsym}}) %>%
      ggplot(aes(x = crit_diff, y = 0, fill = stat(quantile))) +
      ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = HDInterval::hdi, vline_linetype = 2) +
      # geom_density(data = item_fit2_1pl_testlets_mm[1:(1600*9),], aes(crit_diff), colour = 'steelblue1', fill = 'steelblue1', alpha = 0.3) +
      facet_wrap({{person}}, scales = "free") +
      scale_fill_manual(values = c("transparent", color, "transparent"), guide = "none") +
      xlab("Log-likelihood difference between predicted and observed responses.")
  }

  return(g)
}

hdi_custWidth <- function(...) {
  dots <- list(...)
  quantiles <- dots[[2]]
  hdi_width <- quantiles[[length(quantiles)]] # uses the last entry if its a vector which should be the biggest one; better pass a single double < 1.0
  if (is.na(hdi_width)) hdi_width <- .89 # happens is quantiles = 1L
  message(paste0('HDI credible interval width = ', hdi_width))
  HDInterval::hdi(dots[[1]], credMass = hdi_width)
}

get.person.id <- function(data_long, person) {
  personsym <- sym(person)

  personnames <- unique(model$data[[person]])
  person_key <- seq_along(personnames)
  names(person_key) <- personnames

  data_long %>% mutate(person.id = person_key[person], .after = person) %>% pull(person.id) %>%
    return()
}

get.mixed_ppmc_data <- function(model, subset = NULL, ppmcMethod = "MC", sd = 1) { # 40 s statt 53 s bzw. 48 s
  # speed up further with sparse matrix multiplication

  person <- model$var_specs$person
  personsym <- sym(person)

  # tictoc::tic()
  data_long <- model$data %>% mutate(item.id = get.item.id(.))

  if (is.null(subset)) {
    subset <- c(1:nsamples(model))
  }

  message('Calculating probabilities')

  itempars <- spread.draws(model, pars = 'delta') %>% relocate(delta, .after = last_col())

  if (model$model_specs$item_parameter_number == 2) {
    alpha1 <- spread.draws(model, pars = 'alpha1')
    # itempars <- itempars %>% inner_join(alpha1) # should be in the right order so no join needed
    itempars <- itempars %>% ungroup() %>% mutate(alpha1 = alpha1$alpha1)
  } else {
    itempars <- itempars %>% mutate(alpha1 = 1)
  }

  if (model$model_specs$item_parameter_number == 3) {
    stop('Function not implemented for 3pl model')
    # gamma <- spread.draws(model, pars = 'gamma')
    # itempars <- itempars %>% inner_join(gamma)
  } else {
    itempars <- itempars %>% mutate(gamma = 0)
  }

  itempars <- itempars %>% filter(.draw %in% subset) #%>% left_join(testlet)

  # using N(0,1) or prior (for 1 pl) insted?
  # theta_rep <- purrr::map(brms::VarCorr(model, summary = FALSE)[[person]][["sd"]],
  #                         .f = ~rnorm(length(unique(model$data$person)), mean = 0, sd = .x)) %>%
  #   as.data.frame() %>% t()
  theta_rep <- rnorm(brms::nsamples(model)*length(unique(model$data$person)), mean = 0, sd = sd) %>%
    matrix(ncol = length(unique(model$data$person)))

  ppmc_data <- calc.probability(itempars, theta_rep)

  ppmc_data["yrep"] <- rbinom(n = nrow(ppmc_data), size = 1, ppmc_data$ppe)

  # message('finished')
  # tictoc::toc()

  return(ppmc_data)
}

calc.probability <- function(itempars, theta_rep) {

  d <- itempars %>% select(item, delta, .draw) %>%
    pivot_wider(names_from = .draw, values_from = delta)
  item_names <- d %>% pull(item)
  d <- d %>% ungroup() %>%  select(-item) %>%
    as.matrix() %>% as.numeric()
  a <- itempars %>% select(item, alpha1, .draw) %>%
    pivot_wider(names_from = .draw, values_from = alpha1) %>% ungroup() %>%  select(-item) %>%
    as.matrix()
  g <- itempars %>% select(item, gamma, .draw) %>%
    pivot_wider(names_from = .draw, values_from = gamma) %>% ungroup() %>%  select(-item) %>%
    as.matrix() %>% as.numeric()

  n_items <- nrow(a)
  reps <- ncol(a)
  n_pers <- ncol(theta_rep)

  A <- matrix(rep(0, reps^2*n_items), nrow = n_items*reps)
  # A <- Matrix(rep(0, reps^2*n_items), nrow = n_items*reps, sparse = TRUE)

  for (i in 1:reps) {
    A[,i] <- c(rep(0, (i-1)*nrow(a)), a[,i], rep(0, (reps-i)*nrow(a)))
  }

  A <- Matrix::Matrix(A, sparse = TRUE)

  person_terms <- A %*% theta_rep
  terms <- as.matrix(person_terms) + d
  ppe <- g + (1-g)*inv_logit_scaled(terms)
  colnames(ppe) <- paste("Persondummy",1:n_pers)

  ppe <- ppe %>% as_tibble() %>% mutate(item = rep(item_names, ncol(a)),
                                        .draw = rep(1:ncol(a), each = length(item_names)), .before = 1) %>%
    pivot_longer(values_to = "ppe", names_to = "person", cols = c(-item, -.draw))


  return(ppe)
}
