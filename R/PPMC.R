# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('.iteration', 'j', 'yrep', 'ppe2', 'crit', 'crit_rep', 'data', 'testlet_thetas',
                                                        '.draw', 'draw', 'person', 'var_p', 'sum_var', 'item_id', 'crit_diff'
))

#' Generates data for posterior predictive model checks
#'
#' @param model birtmsfit object
#' @param ppmcMethod char; either 'C' for consevative checks or 'M' for mixed ppmc with independently drawn theta distribution
#' @param post_responses data.frame; data generated with birtms::get_postdata
#' @param sd double; standard deviation prior for theta distrubution (may be adjusted if person covars are used)
#' @param n_samples integer; number of posterior samples to use (using all posterior samples might lead to memory overflow)
#'
#' @return data.frame to use with birtms::get_ppmccriteria
#' @export
#'
#' @examples
get_ppmcdatasets <- function(model, ppmcMethod, post_responses = NULL, sd = 1, n_samples = NULL) {
  if (is.null(post_responses)) {
    post_responses <- get_postdata(model = model)
  }

  if(!is.null(n_samples)) {
    subset <- sample(brms::nsamples(model), n_samples, replace = FALSE)
    post_responses$yrep <- post_responses$yrep[subset,]
    post_responses$ppe <- post_responses$ppe[subset,]
    post_responses$subset <- subset
  }

  person <- model$var_specs$person
  symperson <- sym(person)

  personkey <- get.person.id(model$data, model = model)
  personkey <- personkey[unique(names(personkey))]

  ppe <-  make_post_longer(model = model, postdata = post_responses, 'ppe') %>%
    mutate(item.id = get.item.id(.)) # %>% rename(person = {{symperson}})

  item_key <- ppe %>% select(item.id, item) %>% group_by(item) %>% dplyr::summarise_all(mean) %>% ungroup()
  key <- item_key$item.id %>% as.integer()
  names(key) <- item_key$item

  if (ppmcMethod == 'C' | ppmcMethod == 'all') {

    yrep <-  make_post_longer(model = model, postdata = post_responses, 'yrep')

    data <- ppe %>% mutate(yrep = yrep$yrep, ppe2 = ppe)
  } else if (ppmcMethod == 'M' | ppmcMethod == 'all') {
    if (any(stringr::str_detect(names(model$var_specs), "item_covariables"))) stop("PPMC for models with item covars not implemented yet.")

    temp <- get.mixed_ppmc_data(model, subset = post_responses$subset, ppmcMethod = ppmcMethod, sd = sd) %>%
      mutate(item.id = key[item]) %>% dplyr::arrange(.draw, item.id)

    if (nrow(temp) > nrow(ppe)) {
      warning('Rownumber of posterior predictions differing. Does the model have missings by design?')

      ppe <- ppe %>% mutate(person.id = personkey[{{symperson}}]) # %>% dplyr::arrange(.draw, item.id, person.id)
      temp <- temp %>% mutate(person.id = personkey[person]) %>%
        select(.draw, item.id, ppe, yrep, person.id) %>% rename(ppe2 = ppe) # %>% dplyr::arrange(.draw, item.id, person.id)

      data <- ppe %>% left_join(temp, by = c(".draw", "item.id", "person.id"))

    } else {
      ppe <- ppe %>% dplyr::arrange(.draw, item.id)

      data <- ppe %>% mutate(yrep = temp$yrep, ppe2= temp$ppe) # should be sorted in the right order so no join needed
    }

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
    stop('Fehler. Ung\u00FCltige PPMC Methode!')
  }

  return(data)
}

#' Calculates posterior predictive check for specific criteria
#'
#' @param ppmcdata data.frame; data generated with birtms::get_ppmcdatasets
#' @param crit function; infit, outfit or ll
#' @param ppmcMethod char; either 'C' for consevative checks or 'M' for mixed ppmc with independently drawn theta distribution
#' @param group char; 'item' or column name of person identifier (e.g. 'person', 'id', 'token')
#'
#' @return data.frame to use with birtms::plot_fit_statistic
#' @export
#'
#' @examples
get_ppmccriteria <- function(ppmcdata, crit, ppmcMethod, group = 'item') {
  if (group == 'item') {
    fitData <- fit_statistic(criterion = crit, group = "item", data = ppmcdata)
  } else if (group == person) {
    if (ppmcMethod != 'C') stop("Mixed PPMC only suitable for items.")

    fitData <- fit_statistic(criterion = crit, group = person, data = ppmcdata)
  }

  return(fitData)
}

#' Densityplots of posterior predicitve checks
#'
#' @param model birtmsfit object
#' @param data data.frame; data generated with birtms::get_ppmccriteria
#' @param units integer vector of length 2; first and last item/person to generate a plot for
#' @param group char; 'item' or column name of person identifier (e.g. 'person', 'id', 'token')
#' @param ppmcMethod char; either 'C' for consevative checks or 'M' for mixed ppmc with independently drawn theta distribution
#' @param hdi_width double; sets width of credibility interval
#'
#' @return ggplot object
#' @export
#'
#' @examples
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
      ggplot2::ggplot(aes(x = crit_diff, y = 0, fill = ggplot2::stat(quantile))) +
      ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = hdi_custWidth, quantiles = hdi_width, vline_linetype = 2) +
      # geom_density(data = item_fit2_1pl_testlets_mm[1:(1600*9),], aes(crit_diff), colour = 'steelblue1', fill = 'steelblue1', alpha = 0.3) +
      ggplot2::facet_wrap("item", scales = "free") +
      ggplot2::scale_fill_manual(values = c("transparent", color, "transparent"), guide = "none") +
      ggplot2::xlab("itemfit criteria difference between predicted and observed responses.")
  } else if (group == person) {
    g <- data %>% mutate(person_id = get.person.id(., model = model)) %>%
      filter(person_id <= units[2] & person_id >= units[1]) %>%
      mutate({{person}} := paste('Person', {{personsym}})) %>% group_by({{personsym}}) %>%
      ggplot2::ggplot(aes(x = crit_diff, y = 0, fill = ggplot2::stat(quantile))) +
      ggridges::geom_density_ridges_gradient(quantile_lines = TRUE, quantile_fun = HDInterval::hdi, vline_linetype = 2) +
      # geom_density(data = item_fit2_1pl_testlets_mm[1:(1600*9),], aes(crit_diff), colour = 'steelblue1', fill = 'steelblue1', alpha = 0.3) +
      ggplot2::facet_wrap({{person}}, scales = "free") +
      ggplot2::scale_fill_manual(values = c("transparent", color, "transparent"), guide = "none") +
      ggplot2::xlab("Log-likelihood difference between predicted and observed responses.")
  }

  return(g)
}

fit_statistic <- function(criterion, group, data) {
  group <- sym(group)

  message('calculating fitstatistic')

  fitdata <- data %>%
    mutate(
      crit = criterion(response, ppe, .),
      crit_rep = criterion(yrep, ppe2, .)
    ) %>%
    group_by({{group}}, .draw) %>%
    summarise(
      crit = sum(crit),
      crit_rep = sum(crit_rep),
      crit_diff = crit_rep - crit,
      .groups = 'drop'
    ) # %>%
    # mutate(draw = as.numeric(sub("^V", "", .draw))) %>%
    # dplyr::arrange({{group}}, .draw)

  # message('finished')

  return(fitdata)
}

infit <- function(y, p, data, ...) {
  sum_var_p <- data %>% mutate(var_p = (ppe*(1-ppe))) %>% group_by(item, .draw) %>%
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

hdi_custWidth <- function(...) {
  dots <- list(...)
  quantiles <- dots[[2]]
  hdi_width <- quantiles[[length(quantiles)]] # uses the last entry if its a vector which should be the biggest one; better pass a single double < 1.0
  if (is.na(hdi_width)) hdi_width <- .89 # happens is quantiles = 1L
  message(paste0('HDI credible interval width = ', hdi_width))
  HDInterval::hdi(dots[[1]], credMass = hdi_width)
}

get.person.id <- function(data_long, model) {
  person <- model$var_specs$person
  personsym <- sym(person)

  personnames <- unique(model$data[[person]])
  person_key <- seq_along(personnames)
  names(person_key) <- personnames

  data_long %>% mutate(person.id = person_key[{{personsym}}], .after = person) %>% dplyr::pull(person.id) %>%
    return()
}

get.mixed_ppmc_data <- function(model, subset = NULL, ppmcMethod = "MC",
                                sd = 1, sequential = FALSE) {

  person <- model$var_specs$person
  personsym <- sym(person)

  # tictoc::tic()
  # data_long <- model$data %>% mutate(item.id = get.item.id(.))

  if (is.null(subset)) {
    subset <- c(1:brms::nsamples(model))
  }

  message('Calculating probabilities')

  itempars <- spread.draws(model, pars = 'delta') %>% dplyr::relocate(delta, .after = tidyselect::last_col())

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
  #                         .f = ~stats::rnorm(length(unique(model$data$person)), mean = 0, sd = .x)) %>%
  #   as.data.frame() %>% t()

  person_ids <- unique(model$data[[person]])
  reps <- ifelse(is.null(subset), brms::nsamples(model), length(subset))
  theta_rep <- stats::rnorm(reps*length(person_ids), mean = 0, sd = sd) %>%
    matrix(ncol = length(person_ids))

  if (!sequential) {
    ppmc_data <- calc.probability(itempars, theta_rep, person_ids)
  } else {
    theta_rep <- theta_rep %>% as.data.frame() %>%
      stats::setNames(person_ids) %>%
      tibble::as_tibble() %>% mutate(.draw = dplyr::row_number(), .before = 1) %>% filter(.draw %in% subset) %>%
      tidyr::pivot_longer(cols = -.draw, values_to = "theta", names_to = "person")

    # theta_rep <- model %>% spread_draws(theta_rep[ID]) %>% filter(.draw %in% subset)

    itempars <- itempars %>% group_by(.draw) %>% tidyr::nest() %>% rename(itempars = data)
    theta_rep <- theta_rep %>% group_by(.draw) %>% tidyr::nest() %>% rename(theta_rep = data)
    ppe <- itempars %>% left_join(theta_rep, by = ".draw")

    ppe <- ppe %>% mutate(testlet_thetas = NA)

    ppmc_data <- ppe %>% mutate(ppe = purrr::pmap(list(.draw, itempars, theta_rep, testlet_thetas),
                                           calc.probability_sequential)) %>%
      select(.draw, ppe) %>% group_by(.draw) %>% tidyr::unnest(cols = c(ppe)) %>% select(-draw)
  }

  ppmc_data["yrep"] <- stats::rbinom(n = nrow(ppmc_data), size = 1, ppmc_data$ppe)

  # message('finished')
  # tictoc::toc()

  return(ppmc_data)
}

calc.probability <- function(itempars, theta_rep, person_ids) {

  d <- itempars %>% select(item, delta, .draw) %>%
    tidyr::pivot_wider(names_from = .draw, values_from = delta)
  item_names <- d %>% dplyr::pull(item)
  d <- d %>% ungroup() %>%  select(-item) %>%
    as.matrix() %>% as.numeric()
  a <- itempars %>% select(item, alpha1, .draw) %>%
    tidyr::pivot_wider(names_from = .draw, values_from = alpha1) %>% ungroup() %>%  select(-item) %>%
    as.matrix()
  g <- itempars %>% select(item, gamma, .draw) %>%
    tidyr::pivot_wider(names_from = .draw, values_from = gamma) %>% ungroup() %>%  select(-item) %>%
    as.matrix() %>% as.numeric()

  n_items <- nrow(a)
  reps <- ncol(a)
  n_pers <- ncol(theta_rep)

  A <- Matrix::sparseMatrix(i = 1:(n_items*reps), j = rep(1:reps, each = n_items),
                             x = as.numeric(a), dims = list(n_items*reps, reps))

  person_terms <- A %*% theta_rep
  terms <- as.matrix(person_terms) + d
  ppe <- g + (1-g)*brms::inv_logit_scaled(terms)
  colnames(ppe) <- person_ids

  ppe <- ppe %>% tibble::as_tibble() %>% mutate(item = rep(item_names, ncol(a)),
                                        .draw = rep(1:ncol(a), each = length(item_names)), .before = 1) %>%
    tidyr::pivot_longer(values_to = "ppe", names_to = "person", cols = c(-item, -.draw))

  return(ppe)
}

calc.probability_sequential <- function(.draw, itempars, theta_rep, testlet_thetas = NA) {
  person <- theta_rep$person
  theta_rep <- theta_rep$theta

  itemnames <- itempars$item

  delta <- itempars$delta
  alpha1 <- itempars$alpha1
  gamma <- itempars$gamma

  if (!is.na(testlet_thetas)) {
    testlet <- itempars$testlet

    testlet_thetas <- testlet_thetas %>% select(-c(person, .iteration, .chain))

    ppe <- gamma + (1-gamma)*brms::inv_logit_scaled(
      delta + (alpha1 %*% t(theta_rep)) + t(testlet_thetas[,testlet]))

  } else {
    ppe <- gamma + (1-gamma)*brms::inv_logit_scaled(delta + (alpha1 %*% t(theta_rep)))
  }

  ppe <- ppe %>% t() %>% as.data.frame() %>% stats::setNames(itemnames) %>%
    mutate(person = person, draw = .draw, .before = 1) %>%
    tidyr::pivot_longer(cols = c(3:ncol(.)), names_to = 'item', values_to = 'ppe')

  return(ppe)
}
