# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('itempair', '.draw', 'item1', 'item2', '.lower', '.upper', 'ppv', 'value',
                                                        'or_dif_median', 'z_or_dif_median_highlighted', 'z_or_dif_median',
                                                        'above_zero', 'beneath_zero', 'or_act',
                                                        '.width', '.interval', '.zero_correction', '.point', 'or_dif_ci',
                                                        'll_low', 'ul_low', 'sd_person__Intercept', 'itemname', 'item',
                                                        'sd_person__theta_Intercept', 'b_logalpha_Intercept', 'r_item__logalpha',
                                                        'alpha', 'll_up', 'ul_up', 'under_bonds', 'over_bonds', 'outof_bonds',
                                                        'in_bonds', 'or_act_ci', 'or_act_ci.lower', 'or_act_ci.upper', 'inside_or_act_ci'
                                                        ))

#' Odds ratio
#' Calculates the odds ratio for the posterior samples or the original responses
#' adjusted from Anna Scharl and Timo Gnambs (https://www.tqmp.org/RegularArticles/vol15-2/p075/p075.pdf)
#'
#' During Haldane zero-correction a value close to 0 is added to all cells
#' (https://www.oxfordreference.com/view/10.1093/acref/9780199976720.001.0001/acref-9780199976720-e-1977).
#' Most common is 0.5. This value eliminates the first order bias term.
#'
#' @param y_rep (pers) x (item) x (rep) array; replicated data (can handle response data as dataframe or lists of dataframes as well)
#' @param y (pers) x (item) dataframe; response data
#' @param zero_correction character; 'none', 'Haldane', or 'Bayes'
#' @param ci_method character; 'Woolf', 'unconditional', 'compromise', 'BayesEqTails' or 'BayesHDI'
#' @param nsim_ci integer; number of draws used to get a distribution of the odds ratio value to calculate CIs
#' @param nsim_median integer; number of draws used to get a distribution of the odds ratio value to calculate the median
#' @param ci_width double
#'
#' @return tibble
#' @export
#'
#' @examples
#' tibble::tribble(
#' ~x, ~y,
#' 0, 1,
#' 1, 0,
#' 1, 1,
#' 0, 0,
#' 1, 0
#' ) %>% calculate_odds_ratio() # equals 0.5
calculate_odds_ratio <- function(y_rep = NULL, y = NULL, zero_correction = 'none', ci_method = 'Woolf', ci_width = .89,
                                 nsim_ci = 10000000, nsim_median = 100000) {
  if(is.null(y_rep) & is.null(y)) stop('Missing data argument! Use either y_rep or y.')
  if(!is.null(y_rep) & !is.null(y)) stop('Too many data arguments! Use either y_rep or y.')
  if(!(zero_correction %in% c('none', 'Haldane', 'Bayes'))) stop('Invalid zero correction method.')
  if(!(ci_method %in% c('Woolf', 'unconditional', 'BayesEqTails', 'BayesHDI', 'compromise'))) stop('Invalid CI method.')

  if(!is.null(y)) y_rep <- y # function only uses y_rep
  if(!is.array(y_rep)) {
    if(!is.list(y_rep[[1]])) y_rep <- birtms::list2array(list(y_rep)) # make pseudo three dimensional array from dataframe
    else y_rep <- birtms::list2array(y_rep) # make three dimensional array from dataframe list
  }
  if(dim(y_rep)[3] == 1) {
    sum_fct <- function(x) sum(x)
  } else {
    sum_fct <- function(x) colSums(x)
  }

  .draw <- NULL
  if(dimnames(y_rep)[[2]][[1]] == '.draw') {
    .draw <- y_rep[1,1,]
    y_rep <- y_rep[,-1,, drop = FALSE]
  }

  rep <- dim(y_rep)[3] # number of (post) samples
  J <- dim(y_rep)[2] # number of items
  n <- matrix(0, 4, rep)
  n_pairs <- (J^2 - J)/2
  or <- matrix(data = NA, nrow = rep, ncol = n_pairs) %>% as.data.frame()
  count <- 1
  or_ci <- NULL
  if (rep == 1) {
    or_ci <- rbind(or, or)
    rownames(or_ci) <- c('.lower', '.upper')
  }
  percent <- 0.1

  for (j in seq_len(J)) {
    i <- 1

    while (i<j) {
      n[1,] <- sum_fct(y_rep[, i, ] == 1 & y_rep[, j, ] == 1)
      n[2,] <- sum_fct(y_rep[, i, ] == 0 & y_rep[, j, ] == 0)
      n[3,] <- sum_fct(y_rep[, i, ] == 1 & y_rep[, j, ] == 0)
      n[4,] <- sum_fct(y_rep[, i, ] == 0 & y_rep[, j, ] == 1)

      if (zero_correction == 'Haldane') {
        corr <- 0.5
        n <- n + corr
      } else corr <- 0

      or[, count] <- (n[1,]*n[2,])/(n[3,]*n[4,])

      if (zero_correction == 'Bayes') {
        for(col_index in seq_len(ncol(n))) {
          if (min(n[,col_index]) == 0) {
            or[col_index, count] <- stats::median(get_or_distribution(n[,col_index], nsim = nsim_median))
          }
        }
      }

      if(rep == 1) {
        if (ci_method == 'Woolf') or_ci[, count] <- or_ci_woolf(or[, count], n, corr, ci_width) %>% unlist()
        else if (ci_method == 'unconditional') or_ci[, count] <- or_ci_uncond(n, ci_width)
        else if (ci_method == 'BayesEqTails') or_ci[, count] <- or_ci_bayes(n, ci_width, nsim = nsim_ci, hdi = FALSE)
        else if (ci_method == 'BayesHDI') or_ci[, count] <- or_ci_bayes(n, ci_width, nsim = nsim_ci, hdi = TRUE)
        else if (ci_method == 'compromise') {
          if (min(n) == 0) or_ci[, count] <- or_ci_bayes(n, ci_width, nsim = nsim_ci, hdi = FALSE)
          else or_ci[, count] <- or_ci_uncond(n, ci_width)
        }

        colnames(or_ci)[count] <- paste0('ItemPair', i, '_', j)
      }

      colnames(or)[count] <- paste0('ItemPair', i, '_', j)
      count <- count + 1
      i <- i + 1

      if (ci_method %in% c('BayesEqTails', 'BayesHDI') | zero_correction == 'Bayes') {
        if (count/n_pairs > percent) {
          print(paste(100*percent, '% finished'))
          percent <- percent + 0.1
        }
      }
    }
  }

  if(!is.null(.draw)) {
    or <- cbind(.draw, or)
  }

  return(list(or = or, ci = or_ci))
}

#' Summarises Odds Ratio statistic
#' Returns odds ratio values for actual dataset and posterior predictions.
#' Summarises the median and HDI of their difference and returns the ppp-value.
#'
#' @param model birtmsfit
#' @param n_samples int - Number of posterior samples to use
#' @param zero_correction character; 'none', 'Haldane', 'compromise' or 'Bayes'
#' @param ci_method character; 'Woolf', 'unconditional', 'compromise', 'BayesEqTails' or 'BayesHDI'
#' @param ci_width double
#' @param nsim_ci integer; number of draws used to get a distribution of the odds ratio value to calculate CIs
#' @param nsim_median integer; number of draws used to get a distribution of the odds ratio value to calculate the median
#'
#' @return birtmsdata; tibble with additinal attributes
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' get_or(fit, n_samples = 500)
#' }
get_or <- function(model, n_samples = NULL, ci_width = .89, zero_correction = 'none', ci_method = 'Woolf', nsim_ci = 10000000, nsim_median = 100000) {
  seperate_itempairs <- function(x) {
    x <- x %>% mutate(itempair = stringr::str_remove(itempair, 'ItemPair')) %>% tidyr::separate(itempair, into = c('item1', 'item2'), convert = TRUE)

    return(x)
  }

  gather_or <- function(x, name) {
    x <- x %>% tidyr::pivot_longer(names_to = 'itempair', values_to = {{name}}, cols = -.draw) %>%
      seperate_itempairs()

    return(x)
  }

  comp <- FALSE
  if(zero_correction == 'compromise') comp <- TRUE

  item <- model$var_specs$item
  person <- model$var_specs$person

  y <- make_responsedata_wider(model) %>% select(-dplyr::any_of(unlist(model$var_specs)))
  itemnames <- colnames(y)

  yrep <- posterior_predict_long(model, n_samples) %>%
    select({{person}}, {{item}}, .draw, yrep) %>%
    tidyr::pivot_wider(names_from = {{item}}, values_from = 'yrep') %>%
    select(-{{person}})%>% mutate(.draw = as.numeric(.draw)) %>%
    dplyr::group_by(.draw) %>% dplyr::group_split(.keep = TRUE) %>%
    list2array()

  message('Calculating posterior odds ratio')
  if(comp) zero_correction <- 'Haldane'
  or_rep <- calculate_odds_ratio(yrep, zero_correction = zero_correction, nsim_median = nsim_median, nsim_ci = nsim_ci)$or # calculates odds ratio for posterior samples

  message('Calculating actual odds ratio')
  if(comp) zero_correction <- 'Bayes'
  or_list <- calculate_odds_ratio(y, zero_correction = zero_correction, ci_method = ci_method, nsim_median = nsim_median, nsim_ci = nsim_ci) # calculates odds ratio for actual sample/data
  or_act <- or_list$or %>% mutate(.draw = 0, .before = 1)

  or_act_ci <- or_list$ci %>% t() %>% tibble::as_tibble(rownames = "itempair") %>% seperate_itempairs() %>%
    dplyr::mutate(.width = ci_width, .zero_correction = zero_correction, .interval = ci_method) %>%
    dplyr::group_by(item1, item2) %>% tidyr::nest(or_act_ci = c(.lower, .upper, .width, .zero_correction, .interval))

  or_act_dat <- rep_dataframe(or_act, nrow(or_rep))
  or_dif <- or_rep - or_act_dat

  or <- or_dif %>% gather_or('or_dif') %>%
    dplyr::group_by(item1, item2) %>% tidyr::nest(or_dif_samples = c(.draw, or_dif)) %>%
    dplyr::mutate(itemname1 = itemnames[[item1]], itemname2 = itemnames[[item2]], .before = 1)

  or_act <- or_act %>% gather_or('or_act') %>% select(-.draw)
  or_rep <- or_rep %>% gather_or('or_rep') %>% tidyr::nest(or_rep_samples = c(.draw, or_rep))
  or_ppp <- colMeans(or_dif %>% select(-.draw) > 0) %>% tibble::as_tibble(rownames = "itempair") %>% seperate_itempairs() %>%
    dplyr::rename(or_ppp = value)

  or_dif_hdi <- or %>% tidyr::unnest() %>% dplyr::filter(is.finite(or_dif)) %>%
    tidybayes::median_hdi(or_dif, .width = ci_width) %>%
    dplyr::rename(or_dif_median = or_dif) %>%
    tidyr::nest(or_dif_ci = c(.lower, .upper, .width, .point, .interval))

  or <- or %>% left_join(or_act, by = c('item1', 'item2')) %>%
    left_join(or_act_ci, by = c('item1', 'item2')) %>%
    left_join(or_rep, by = c('item1', 'item2')) %>%
    left_join(or_dif_hdi, by = c('item1', 'item2')) %>%
    left_join(or_ppp, by = c('item1', 'item2')) %>%
    dplyr::relocate(dplyr::any_of(c('or_act', 'or_act_ci', 'or_rep_samples')), .after = item2) %>%
    dplyr::ungroup()

  message('-------')
  return(or)
}

#' PPMC Odds ratio heatmap
#' Prints a heatmap of odds ratio differences used for post predictive model checking.
#'
#' @param or_data dataframe generated by birtms::get_or()
#' @param alternative_color boolean; color highlight only the problematic items
#'
#' @return ggplot object
#' @export
#' @importFrom ggplot2 aes
#'
#' @examples
#' \dontrun{
#' or_data <- get_or(fit, n_samples = 500)
#' plot_ppmc_or_heatmap(or_data)
#' }
plot_ppmc_or_heatmap <- function(or_data, alternative_color = FALSE) {
  # unite boolean columns that check if HDI includes zero for multimodal distributions
  or_data <- or_data %>% tidyr::unnest(or_dif_ci, keep_empty = TRUE) %>% tidyr::unnest(or_act_ci, names_sep = "") %>%
    dplyr::mutate(above_zero = .lower > 0, beneath_zero = .upper < 0,
                  inside_or_act_ci = .lower > (or_act_ci.lower- or_act) &  .upper < (or_act_ci.upper - or_act)) %>%
    dplyr::select(item1, item2, or_dif_median, above_zero, beneath_zero, inside_or_act_ci) %>%
    dplyr::group_by(item1, item2) %>% dplyr::summarise_all(~mean(.x, na.rm = FALSE)) %>%
    dplyr::mutate_at(c('above_zero', 'beneath_zero'), ~ifelse(. != 0, TRUE, FALSE)) %>%
    dplyr::mutate_at(c('inside_or_act_ci'), ~ifelse(. == 1, TRUE, FALSE)) %>%
    dplyr::ungroup()


  above <- or_data$above_zero
  beneath <- or_data$beneath_zero
  cap <- '**Interpretation:** The grey fields represent items for which the HDI does not contain a odds ratio difference of 0.<br>'
  cap2 <- '**Interpretation:** The colored fields represent items for which the HDI does not contain a odds ratio difference 0.<br>'

  cap <- paste0(cap, 'Fields with an *L* represent items where predicted odds ratio is lower than actual observed odds ratio.<br>
    Fields with an *H* represent items where predicted odds ratio is higher than actual observed odds ratio.')
  cap2 <- paste0(cap2, '*Blue* fields represent items where predicted odds ratio is lower than actual observed odds ratio.<br>
    *Red* fields represent items where predicted odds ratio is higher than actual observed odds ratio.')

  or_data <- or_data %>% dplyr::mutate(z_or_dif_median = scale(or_dif_median),
                                       z_or_dif_median_highlighted = ifelse(above | beneath, NA, z_or_dif_median))

  g <- ggplot2::ggplot(or_data, aes(item1, item2, fill = z_or_dif_median_highlighted, label = z_or_dif_median_highlighted, height = 1, width = 1)) +
    ggplot2::scale_x_continuous("Item A", expand=c(0,0), position = "top", breaks = seq(min(or_data$item1),max(or_data$item1),1)) +
    ggplot2::scale_y_continuous("Item B", expand=c(0,0), breaks = seq(min(or_data$item2),max(or_data$item2),1)) +
    ggplot2::ggtitle('z-standardised Odds Ratio difference') +
    ggplot2::geom_tile(color="black") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.border= ggplot2::element_rect(size=2, color="black", fill=NA),
                   axis.ticks = ggplot2::element_blank(), plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0)) +
    ggplot2::coord_fixed()

  if(alternative_color) {
    g <- g + ggplot2::scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "#f7f7f7",
                                           midpoint = 0, limit = c(-1, 1), name = "z(\u0394OR)",
                                           oob = scales::squish, na.value = '#00ff00') +
      ggplot2::labs(caption = cap)

    if(sum(above, na.rm = TRUE) > 0) { # otherwise the empty subset throws an error
      g <- g + ggplot2::geom_tile(data = subset(or_data, above), fill = 'gray50', color="black") +
        ggplot2::geom_text(data = subset(or_data, above), aes(label = 'H'), size = 5)
    }
    if(sum(beneath, na.rm = TRUE) > 0) {
      g <- g + ggplot2::geom_tile(data = subset(or_data, beneath), fill = 'gray50', color="black") +
        ggplot2::geom_text(data = subset(or_data, beneath), aes(label = 'L'), size = 5)
    }
  } else {
    or_data <- or_data %>% dplyr::mutate(z_or_dif_median = scale(or_dif_median), z_or_dif_median_highlighted = ifelse(above | beneath, NA, abs(z_or_dif_median)))

    g <- g + ggplot2::scale_fill_gradient(low = "white", high = "grey50", limit = c(0, 1), oob = scales::squish,
                                          na.value = '#00ff00', name = "\u007C z(\u0394OR) \u007C") +
      ggplot2::labs(caption = cap2)

    fillings <- NULL

    if(sum(above, na.rm = TRUE) > 0) { # otherwise the empty subset throws an error
      g <- g + ggplot2::geom_tile(data = subset(or_data, above), aes(alpha = 'over bonds'), fill = '#ca0020', color="black")
      fillings <- c("#ca0020", fillings)
    }
    if(sum(beneath, na.rm = TRUE) > 0) {
      g <- g + ggplot2::geom_tile(data = subset(or_data, beneath), aes(alpha = 'under bonds'), fill = '#0571b0', color="black")
      fillings <- c("#0571b0", fillings)
    }
    if(sum(or_data$inside_or_act_ci, na.rm = TRUE) > 0) {
      g <- g + ggplot2::geom_tile(data = subset(or_data, or_data$inside_or_act_ci), aes(alpha = 'in bonds'), fill = '#1a9641', color="black")
      fillings <- c("#1a9641", fillings)
    }

    g <- g + ggplot2::scale_alpha_manual("Color flags", values=c(1, 1, 1),
                                         guide = ggplot2::guide_legend(override.aes = list(fill=fillings)))
  }

  return(g)
}

#' Odds ratio heatmap
#' Odds ratio values should be greater 1 for items that are conditionally independent in a one dimensional
#' IRT model (Holland, Rosenbaum, 1986). Haberman (2007) shows that the lower bond can be estimated more
#' precisely (and will be even higher than 1) and also a upper bond can be calculated and gives formulae
#' for 1PL and 2PL models.
#'
#' @param or_data dataframe generated by birtms::get_or()
#' @param median_centered_colorscale boolean
#' @param model birtmsfit object
#' @param a double; mean slope parameter (if not passed model)
#' @param sigma double; standard deviation of person dimesnion (theta)
#' @param bayesian boolean; should the bayesian distribution for actual odds ratio should be used as reference instead of point estimate
#' @param nsim_ci integer; number of draws used to get a distribution of the odds ratio value to calculate CIs
#' @param ci_width double
#'
#' @return ggplot2 object
#' @export
#' @importFrom ggplot2 aes
#' @importFrom stats median
#'
#' @examples
#' \dontrun{
#' or_data <- get_or(fit, n_samples = 500)
#' plot_or_heatmap(or_data)
#' }
plot_or_heatmap <- function(or_data, model = NULL, a = 1, sigma = 1, median_centered_colorscale = TRUE, bayesian = FALSE, nsim_ci = 1000000, ci_width = .89) {
  # check if odds are out of bonds
  or_data <- or_data %>% mutate(ll_low = NA_real_, ll_up = NA_real_, ul_low = NA_real_, ul_up = NA_real_, or_act_scaled = NA_real_) %>%
    mutate(or_act = ifelse(is.infinite(or_act) | is.nan(or_act), NA, or_act))

  if(is.null(model)) {
    if (bayesian) stop('Bayesian method needs acompanying model.')

    lims <- or_limits_irt(ai = a, aj = a, sigma = sigma)
    ll <- lims[[1]]
    ul <- lims[[2]]

    or_data <- or_data %>% mutate(ll_low = or_act-ll, ll_up = ll_low, ul_low = ul - or_act, ul_up = ul_low)
  } else {
    if (model$model_specs$dimensionality_type != 'unidimensional') {
      warning('The limits used to color code the item pairs were derived only for unidimensional models.')
    }

    if (model$model_specs$item_parameter_number == 1) {
      sigma <- model %>% tidybayes::spread_draws(sd_person__Intercept) %>% dplyr::pull(sd_person__Intercept)
    } else if (model$model_specs$item_parameter_number == 2) {
      key <- tibble::tibble(itemname = c(or_data$itemname1, or_data$itemname2), item = c(or_data$item1, or_data$item2)) %>%
        dplyr::group_by(itemname) %>% dplyr::summarise(item = stats::median(item))

      sigma <- model %>% tidybayes::spread_draws(sd_person__theta_Intercept) %>% dplyr::pull(sd_person__theta_Intercept)
      alphas <- model %>% tidybayes::spread_draws(b_logalpha_Intercept, r_item__logalpha[itemname,])
      alphas <- alphas %>% mutate(alpha = exp(b_logalpha_Intercept + r_item__logalpha)) %>% dplyr::group_by(itemname) %>%
        select(alpha, itemname) %>% tidyr::nest() %>% left_join(key, by = c('itemname')) %>% dplyr::arrange(item)
    } else{
      warning('The limits used to color code the item pairs were derived only for 1 and 2 parametric models.')
    }

    # for 1PL models bonds are the same for all itempairs
    lims <- or_limits_irt(sigma = sigma)
    ll <- lims[[1]]
    ul <- lims[[2]]

    if (bayesian) {
      y <- make_responsedata_wider(model) %>% select(-dplyr::any_of(unlist(model$var_specs)))
      percent <- 0.1
      n_pairs <- nrow(or_data)
      message('Calculating odds ratio distributions.')
    }

    for (i in seq_along(or_data$or_act)) {
      # for 2PL models bonds are different for all itempairs
      if (model$model_specs$item_parameter_number == 2) {
        lims <- or_limits_irt(ai = alphas$data[[or_data$item1[[i]]]], aj = alphas$data[[or_data$item2[[i]]]], sigma = sigma)
        ll <- lims[[1]][[1]]
        ul <- lims[[2]][[1]]
      }

      if (bayesian) {
        counts <- count_for_itempair_or(y[or_data$itemname1[[i]]], y[or_data$itemname2[[i]]])
        v <- contingency2successratio(counts)
        reference <- get_or_distribution(counts, k = .5, nsim = nsim_ci)

        ll <- sample(ll, nsim_ci, replace = TRUE)
        ul <- sample(ul, nsim_ci, replace = TRUE)

        if (i/n_pairs > percent) {
          print(paste(100*percent, '% finished'))
          percent <- percent + 0.1
        }
      } else {
        reference <- or_data$or_act[[i]]
      }

      ll_vec <- (reference - ll) %>% ggdist::hdi(.width = ci_width)
      ul_vec <- (ul - reference) %>% ggdist::hdi(.width = ci_width)
      # if there are multiple HDI areas the lowest value will be the left and the highest the right limit
      ll_vec <- matrix(c(min(ll_vec[,1]), max(ll_vec[,2])), ncol = 2)
      ul_vec <- matrix(c(min(ul_vec[,1]), max(ul_vec[,2])), ncol = 2)

      or_data[i, c('ll_low', 'll_up')] <- ll_vec
      or_data[i, c('ul_low', 'ul_up')] <- ul_vec

      if (bayesian) {
        or_data[i, 'or_act_scaled'] <- (sum(reference > median(ll)) - sum(reference >median(ul)))/nsim_ci
      } else {
        or_data[i, 'or_act_scaled'] <- (median(reference)-median(ll))/(median(ul)-median(ll))
      }
    }
  }

  or_data <- or_data %>% mutate(under_bonds = ifelse(ll_up < 0, TRUE, FALSE),
                                over_bonds = ifelse(ul_up < 0, TRUE, FALSE),
                                in_bonds = ifelse(ll_low > 0 & ul_low > 0, TRUE, FALSE),
                                outof_bonds = under_bonds|over_bonds)

  # setting color sheme
  if(median_centered_colorscale) {
    limits <- c(0, HDInterval::hdi(or_data$or_act, credMass = .89)[[2]])
    cap <- 'Color scale midpoint is set to median(OR).'
    mid <- stats::median(or_data$or_act, na.rm = TRUE)
    colorscale <- ggplot2::scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "#f7f7f7",
                                                midpoint = mid, limit = c(limits[[1]], limits[[2]]), name = 'OR',
                                                oob = scales::squish, na.value = '#00ff00')
    data_col <- sym('or_act')
  } else{
    if (bayesian & !is.null(model)) {
      cap = 'Color scale shows the ratio of CI falling between bonds. <br>
      1 := whole CI inbetween bonds. 0 := whole CI is outside of bonds.<br>
      Dark green: Whole CI falls between bonds. Red: Whole CI is outsite of bonds.'

      colorscale <- ggplot2::scale_fill_gradient(low = "grey50", high = "white", limit = c(0, 1), oob = scales::squish,
                                   na.value = '#00ff00', name = "% of OR distribution in bonds")
    } else {
      cap = paste0('Dark green: Odds ratio point estimate falls between bonds in ', ci_width, ' % of the cases.<br>
      Red: Odds ratio point estimate is outside of bonds in ', ci_width, ' % of the cases.')

      if((length(a) == 1 & length(sigma) == 1 & is.null(model))) {
        colorscale <- ggplot2::scale_fill_continuous(na.value = '#00ffff')
      }
      else {
        colorscale <- ggplot2::scale_fill_gradient2(low = "#fdae61", high = "#fdae61", mid = "#a6d96a",
                                                    midpoint = 0.5, limit = c(0, 1), name = 'scaled OR',
                                                    oob = scales::squish, na.value = '#00ffff')

        cap <- paste0('Color scale midpoint is set to mean of upper and lower bonds.<br>
      1 := odds ratio value of the upper bond. 0 := odds ratio value of the lower bond.<br>', cap)
      }
    }

    cap <- paste0('**Interpretation:** ', cap, '<br>Fields with an *L* represent items where odds ratio is lower than bonds.<br>
    Fields with an *H* represent items where odds ratio is higher than bonds.')
    data_col <- sym('or_act_scaled')
  }

  # plot heatmap
  g <- or_data %>%
    ggplot2::ggplot(aes(item1, item2, fill = {{data_col}}, label = {{data_col}}, height = 1, width = 1)) +
    ggplot2::scale_x_continuous("Item A", expand=c(0,0), position = "top", breaks = seq(min(or_data$item1),max(or_data$item1),1)) +
    ggplot2::scale_y_continuous("Item B", expand=c(0,0), breaks = seq(min(or_data$item2),max(or_data$item2),1)) +
    ggplot2::ggtitle('Odds Ratio values', subtitle = 'actual dataset') +
    ggplot2::geom_tile(color="black", show.legend = TRUE) +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.border= ggplot2::element_rect(size=2, color="black", fill=NA),
                   axis.ticks = ggplot2::element_blank(), plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0)) +
    ggplot2::coord_fixed() +
    colorscale +
    ggplot2::labs(caption = cap)

  fillings <- NULL
  if(!median_centered_colorscale) {
    if(sum(or_data$outof_bonds, na.rm = TRUE) > 0) { # otherwise the empty subset throws an error
      g <- g + ggplot2::geom_tile(data = subset(or_data, outof_bonds), aes(alpha = 'out of bonds'), fill = '#d7191c', color="black")
      fillings <- c("#d7191c", fillings)

      if(sum(or_data$over_bonds, na.rm = TRUE) > 0) {
        g <- g + ggplot2::geom_text(data = subset(or_data, over_bonds), label = 'H', size = 5)
      }
      if(sum(or_data$under_bonds, na.rm = TRUE) > 0) {
        g <- g + ggplot2::geom_text(data = subset(or_data, under_bonds), label = 'L', size = 5)
      }
    }
    if(sum(or_data$in_bonds, na.rm = TRUE) > 0) {
      g <- g + ggplot2::geom_tile(data = subset(or_data, in_bonds), aes(alpha = 'in bonds'), fill = '#1a9641', color="black")
      fillings <- c("#1a9641", fillings)
    }

    # creates second fill legend; alpha will get replaced by fill
    g <- g + ggplot2::scale_alpha_manual("Color flags", values=c(1, 1),
                                         guide = ggplot2::guide_legend(override.aes = list(fill=fillings)))

  }

  return(g)
}



#' Calculates CIs for Odds ratio with Woolfs method
#'
#' @param or double
#' @param counts double of length 4
#' @param corr double; Haldane zero correction term
#' @param ci_width double
#'
#' @return double of length 2
or_ci_woolf <- function(or, counts, corr = 0.5, ci_width = .89) {
  counts <- c(counts)
  z <- stats::qnorm(ci_width + (1 - ci_width)/2)
  se <- sqrt(1/(counts[1]+corr)+1/(counts[2]+corr)+1/(counts[3]+corr)+1/(counts[4]+corr))
  upper <- exp(log(or)+z*se)
  lower <- exp(log(or)-z*se)

  return(list(lower = lower, upper = upper))
}

#' Wrapper to get bayesian CI for OR
#'
#' @param counts double of length 4
#' @param ci double
#'
#' @return double of length 2
or_ci_uncond <- function(counts, ci = .89) {
  v <- contingency2successratio(counts)
  ci <- PropCIs::orscoreci(v[[1]], v[[2]], v[[3]], v[[4]], ci)

  return(ci)
}

#' Calculates CI for Odds Ratio bayesian way
#' Extracted from PropCIs::orci.bayes()
#' Can return HDI instead of equal tailed CI interval as well.
#'
#' @param counts double of length 4
#' @param conf.level double
#' @param k double; prior for beta distribution
#' @param nsim integer
#' @param hdi boolean; should HDI be used instead of equaly tailes CI?
#'
#' @return double of length 2
or_ci_bayes <- function(counts, conf.level = 0.89, k = .5, nsim = 10000000, hdi = FALSE) {
  # Bayes tail interval with beta priors
  fct.F<- function(x,t,a1,b1,a2,b2){
    c <- (b2/a2)/(b1/a1)
    stats::df(x,2*a2,2*b2)*stats::pf(x*t/c,2*a1,2*b1)
  }

  or.F <- function(t,a1,b1,a2,b2)
  {
    return(stats::integrate(fct.F,0,Inf,t=t,a1=a1,b1=b1,a2=a2,b2=b2)$value)
  }

  or.fct <- function(ab,a1,b1,c1,d1,conf.level)
  {
    abs(or.F(ab[2],a1,b1,c1,d1) - (1 - (1-conf.level)/2))+
      abs(or.F(ab[1],a1,b1,c1,d1) - (1-conf.level)/2)
  }

  v <- contingency2successratio(counts)
  temp <- or_distribution_bayes(v[[1]], v[[2]], v[[3]], v[[4]], k, k, k, k, nsim)
  z <- sort(temp[[1]])

  if(hdi) {
    if (v[[3]] != v[[4]]) {
      ci <- z %>% HDInterval::hdi(credMass = conf.level) %>% c()
    } else {
      ci <- 1/z %>% HDInterval::hdi(credMass = conf.level) %>% c()
      warning('HDI is not invariant under transformation 1/z! Check if using equally tailed CI is more appropriate.')
    }
    return(ci)
  } else {
    a1 <- temp[[2]]
    b1 <- temp[[3]]
    c1 <- temp[[4]]
    d1 <- temp[[5]]

    lq <- nsim * (1-conf.level)/2
    uq <- nsim * (1 - (1-conf.level)/2)
    ci <- array(0,2)
    ci[1] <- z[lq]
    ci[2] <- z[uq]
    start <- ci

    if (v[[3]] != v[[4]]) {
      tailci <- stats::optim(start,or.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                      conf.level=conf.level,control=list(maxit=20000))$par
      if(tailci[1] < 0) tailci[1]  <- 0
    } else {
      tailci1 <- stats::optim(start,or.fct,a1=a1,b1=b1,c1=c1,d1=d1,
                       conf.level=conf.level,control=list(maxit=20000))$par
      if(tailci1[1] < 0) tailci1[1]  <- ci[1]
      tailci <- array(0,2)
      tailci[1] <- 1/ tailci1[2]
      tailci[2] <- 1/ tailci1[1]
    }

    return(tailci)
  }
}

#' Simulate Odds Ratio distribution
#' Extracted from PropCIs::orci.bayes()
#'
#' @param a1 a parameter for first beta distribution
#' @param b1 b parameter for first beta distribution
#' @param c1 a parameter for second beta distribution
#' @param d1 b parameter for second beta distribution
#' @param nsim integer
#'
#' @return vector of doubles, distribution od odds ratios
or.sim <- function(a1,b1,c1,d1,nsim = 10000000)
{
  z1 <- stats::rf(nsim, 2*a1,2*b1)
  z2 <- stats::rf(nsim, 2*c1,2*d1)
  a <- (d1/c1)/(b1/a1)
  z <- a*z1/z2
  return(z)
}

#' Calculate successes per sample and sample sizes from contingency table
#'
#' @param mat vector of length 4 with counts c(n11, n00, n10, n01) from contingency table
#'
#' @return vector of length 4: c(y1, n1, y2, n2)
contingency2successratio <- function(mat) {
  y1 <-  mat[1] #n11
  y2 <- mat[4] #n01
  n1 <- mat[1] + mat[3] #n11 + n10
  n2 <- mat[4] + mat[2] #n01 + n00

  return(c(y1, n1, y2, n2))
}

#' Get the odds ratio distribution
#' combines prior with actual data to posterior beta distributions and initiates sampling
#' Extracted from PropCIs::orci.bayes()
#'
#' @param x1 number of successes in sample 1
#' @param n1 sample size in sample 1
#' @param x2 number of successes in sample 2
#' @param n2 sample size in sample 2
#' @param a beta prior for x1
#' @param b beta prior for x2
#' @param c beta prior for n1
#' @param d beta prior for n2
#' @param nsim integer
#'
#' @return vector of doubles, distribution od odds ratios
or_distribution_bayes <- function(x1,n1,x2,n2,a,b,c,d, nsim = 10000000)
{
  if(x2!=n2) {
    a1 <- a + x1
    b1 <- b + n1 - x1
    c1 <- c + x2
    d1 <- d + n2 - x2
  } else {
    a1 <- a + n1 - x1
    b1 <- b +  x1
    c1 <- c + n2 - x2
    d1 <- d + x2
  }

  z <- or.sim(a1,b1,c1,d1, nsim)

  return(list(z, a1, b1, c1, d1))
}

#' Get odds ratio distribution
#'
#' @param counts (4x1) matrix with counts n11, n00, n10, n01 from contingency table
#' @param k double; concentration of beta priors: 0.5 for Jeffreys prior, 1 for uniform priors
#' @param nsim interger
#'
#' @return double, odds ratio distribution
#' @export
#'
#' @examples
#' c <- c(5,2,10,7) # n11, n00, n10, n01
#' or_dist <- get_or_distribution(c, nsim = 10)
get_or_distribution <- function(counts, k = 0.5, nsim = 10000000) {
    v <- contingency2successratio(counts)
    z <- or_distribution_bayes(v[[1]], v[[2]], v[[3]], v[[4]], k, k, k, k, nsim)[[1]]

    if (v[[3]] == v[[4]]) {
      return(1/z)
    } else {
      return(z)
    }
}

#' Calculate Odds Ratio Limits for IRT models
#' Calculates limits for odds ratio values for itempairs to check if they show
#' misfit assuming that data is generated by a one-dimensional 1PL or 2PL model.
#' LIMITS ON LOG ODDS RATIOS FOR UNIDIMENSIONAL ITEM RESPONSE THEORY MODELS (Haberman, 2007)
#'
#' @param ai double; slope of item 1
#' @param aj double; slope of item 2
#' @param sigma double; standard deviation
#' @param beta double
#'
#' @return list(lower_limit, upper_limit) of doubles
#' @export
#'
#' @examples
or_limits_irt <- function(ai = 1, aj = 1, sigma = 1, beta = 0) {
  numerator <- ai*aj*sigma^2
  lower_limit <- exp(numerator/(1+sigma^2*(beta+(ai^2+aj^2)/4)))
  upper_limit <- exp(numerator)

  return(list(lower_limit = lower_limit, upper_limit = upper_limit))
}

#' Creates contingency table counts for item pair based odds ratio
#'
#' @param x double vector, answers to item 1
#' @param y double vector, answers to item 2
#'
#' @return double vector of length 4: c(n11, n00, n10, n01)
#' @export
#'
#' @examples
count_for_itempair_or <- function(x, y) {
  n <- rep(NA, 4)

  n[1] <- sum(x == 1 & y == 1)
  n[2] <- sum(x == 0 & y == 0)
  n[3] <- sum(x == 1 & y == 0)
  n[4] <- sum(x == 0 & y == 1)

  return(n)
}
