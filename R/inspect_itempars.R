# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('person_id', 'alpha1', 'adjusted_alpha'))

#' Plots cateye or dot plots for item parameters
#'
#' @param model birtmsfit object
#' @param pars character; either 'slope', 'easyness', 'delta', 'beta', 'difficulty' or 'pseudoguess'
#' @param style character; either "halfeye" or "dots"
#' @param items integer vector of length 2; indicates first and last item to plot the parameter distributions for
#' @param thresholds double vector of length 2; crops the plot horizontally
#' @param alphacut double vector of length 3;
#' @param betacut double vector of length 2;
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_itemparameter <- function(model, pars, style, items = c(1,5), thresholds = c(.2, 2),
                               alphacut = c(.2,.3, 2), betacut = c(-2,2)) {

  data <- model %>% spread.draws(pars) %>% filter(item_nr >= items[1], item_nr <= items[2])

  if (pars == 'slope') {
    g <- data %>%
      ggplot2::ggplot(aes(y = item, x = alpha1, fill = ggplot2::after_stat(x > alphacut[1] & x < alphacut[3]))) +
      ggplot2::geom_vline(xintercept = alphacut[1], linetype = "dashed") +
      ggplot2::geom_vline(xintercept = alphacut[2], linetype = "dotted") +
      ggplot2::scale_fill_manual(values = c("gray80", "skyblue")) +
      ggplot2::xlab("Trennsch\u00E4rfe") +
      ggplot2::labs(fill = "Innerhalb der Grenzen")
  } else if (pars == 'adjusted slope') {
    g <- data %>%
      ggplot2::ggplot(aes(y = item, x = adjusted_alpha, fill = ggplot2::after_stat(x > alphacut[1] & x < alphacut[3]))) +
      ggplot2::geom_vline(xintercept = alphacut[1], linetype = "dashed") +
      ggplot2::geom_vline(xintercept = alphacut[2], linetype = "dotted") +
      ggplot2::scale_fill_manual(values = c("gray80", "skyblue")) +
      ggplot2::xlab("Trennsch\u00E4rfe") +
      ggplot2::labs(fill = "Innerhalb der Grenzen")
  } else if (pars %in% c('easyness', 'delta', 'beta', 'difficulty')) {
    g <- data %>%
      ggplot2::ggplot(aes(y = item, x = delta, fill = ggplot2::after_stat(x > betacut[1] & x < betacut[2]))) +
      ggplot2::scale_fill_manual(values = c("gray80", "skyblue"))  +
      ggplot2::labs(fill = "Innerhalb der Grenzen")
    if(pars != 'difficulty') {
      g <- g + ggplot2::xlab("Einfachheit")
    } else {
      g <- g + ggplot2::xlab("Schwierigkeit")
    }
  } else if (pars == 'pseudoguess') {
    g <- data %>%
      ggplot2::ggplot(aes(y = item, x = gamma, fill = ggplot2::after_stat(x < 1/15 | x > 1/2))) +
      ggplot2::scale_fill_manual(values = c("skyblue", "gray80")) +
      ggplot2::xlab("Pseuderatewahrscheinlichkeit")
  } else {
    stop('Dieser Parameter wurde noch nicht implementiert.')
  }

  if (style == "halfeye") {
    g <- g + ggdist::stat_halfeye()
  } else if (style == "dots") {
    g <- g + ggdist::stat_dots(quantiles = 100)
  }

  g <- g + ggplot2::coord_cartesian(xlim = c(min(thresholds[1]*1.25, 0), thresholds[2]*1.25))

  return(g)
}

spread.draws <- function(model, pars) {
  stopifnot(model$model_specs$response_type == 'dichotom')
  stopifnot(model$model_specs$add_common_dimension == FALSE)
  stopifnot(model$model_specs$dimensinality_type == 'unidimensional')

  if (model$model_specs$item_parameter_number == 1) {
    if (stringr::str_detect(pars, "slope")) stop('1pl models have no interesting slope parameter')
    beta_ran <- sym("r_item")
    beta_fix <- sym("b_Intercept")
    beta_names = c("r_item[item,]", "b_Intercept")
    beta_lang = sapply(beta_names, str2lang)

  } else if (model$model_specs$item_parameter_number == 2) {
    beta_ran <- sym("r_item__beta")
    beta_fix <- sym("b_skillintercept_Intercept")
    beta_names = c("r_item__beta[item,]", "b_skillintercept_Intercept")
    beta_lang = sapply(beta_names, str2lang)

  } else {
    stop('Only 1pl and 2pl-models implemented yet')
  }

  data_long <- model$data

  item_key <- data_long %>% select(item) %>% unique
  rownames(item_key) <- item_key$item
  item_key <- item_key %>% mutate(item = seq(nrow(item_key))) %>% rename(item_nr = item)

  if (pars %in% c('slope', 'alpha1', 'alpha')) {
    data <- model %>% tidybayes::spread_draws(r_item__logalpha[item,], b_logalpha_Intercept) %>%
      mutate(alpha1 = exp(r_item__logalpha+b_logalpha_Intercept))
  } else if (pars == 'adjusted slope') {
    stop('3pl models not supportet yet')
    # data <- model %>% spread_draws(alpha1[item_nr], gamma[item_nr]) %>%
    #   mutate(adjusted_alpha = alpha1*(1-gamma))
  } else if (pars %in% c('easyness', 'delta', 'beta', 'difficulty')) {
    data <- model %>% tidybayes::spread_draws(!!!beta_lang) %>%
      mutate(delta = ifelse(pars == 'difficulty', -1, 1)*({{beta_ran}}+{{beta_fix}}))
  } else if (pars == 'pseudoguess' | pars == 'gamma') {
    stop('3pl models not supportet yet')
    # data <- model %>% spread_draws(gamma[item_nr])
  } else {
    stop('Dieser Parameter wurde noch nicht implementiert.')
  }

  data <- data %>% mutate(item_nr = item_key[item,1])

  return(data)
}
