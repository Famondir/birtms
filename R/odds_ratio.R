# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('itempair', '.draw', 'item1', 'item2', '.lower', '.upper', 'ppv', 'value',
                                                        'or_dif_mode', 'or_dif_mode', 'z_or_dif_mode_highlighted', 'z_or_dif_mode'))

#' Odds ratio
#' Calculates the odds ratio for the posterior samples or the original responses
#' adjusted from Anna Scharl and Timo Gnambs (https://www.tqmp.org/RegularArticles/vol15-2/p075/p075.pdf)
#'
#' @param y_rep (pers) x (item) x (rep) array; replicated data (can handle response data as dataframe or lists of dataframes as well)
#' @param y (pers) x (item) dataframe; response data
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
calculate_odds_ratio <- function(y_rep = NULL, y = NULL) {
  if(is.null(y_rep) & is.null(y)) stop('Missing data argument! Use either y_rep or y.')
  if(!is.null(y_rep) & !is.null(y)) stop('Too many data arguments! Use either y_rep or y.')

  if(!is.null(y)) y_rep <- y # function only uses y_rep
  if(!is.array(y_rep)) {
    if(!is.list(y_rep[[1]])) y_rep <- list2array(list(y_rep)) # make pseudo three dimensional array from dataframe
    else y_rep <- list2array(y_rep) # make three dimensional array from dataframe list
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
  or <- matrix(data = NA, nrow = rep, ncol = (J^2 - J)/2)
  count <- 1

  for (j in seq_len(J)) {
    i <- 1
    while (i<j) {
      n[1,] <- sum_fct(y_rep[, i, ] == 1 & y_rep[, j, ] == 1)
      n[2,] <- sum_fct(y_rep[, i, ] == 0 & y_rep[, j, ] == 0)
      n[3,] <- sum_fct(y_rep[, i, ] == 1 & y_rep[, j, ] == 0)
      n[4,] <- sum_fct(y_rep[, i, ] == 0 & y_rep[, j, ] == 1)
      or[, count] <- (n[1,]*n[2,])/(n[3,]*n[4,])
      or <- as.data.frame(or)
      colnames(or)[count] <- paste0('ItemPair', i, '_', j)
      count <- count + 1
      i <- i + 1
    }
  }

  if(!is.null(.draw)) {
    or <- cbind(.draw, or)
  }

  return(or)
}

#' Summarises Odds Ratio statistic
#' Returns odds ratio values for actual dataset and posterior predictions.
#' Summarises the mode and hdi of their difference and returns the ppp-value.
#'
#' @param model birtmsfit
#' @param n_samples int - Number of posterior smaples to use
#' @param hdi_width double
#'
#' @return tibble
#' @export
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#'
#' @examples
#' \dontrun{
#' get_or(fit, n_samples = 500)
#' }
get_or <- function(model, n_samples = NULL, hdi_width = .89) {
  seperate_itempairs <- function(x) {
    x <- x %>% mutate(itempair = stringr::str_remove(itempair, 'ItemPair')) %>% tidyr::separate(itempair, into = c('item1', 'item2'), convert = TRUE)

    return(x)
  }

  gather_or <- function(x, name) {
    x <- x %>% tidyr::pivot_longer(names_to = 'itempair', values_to = {{name}}, cols = -.draw) %>%
      seperate_itempairs()

    return(x)
  }

  item <- model$var_specs$item
  person <- model$var_specs$person

  yrep <- posterior_predict_long(model, n_samples) %>%
    select({{person}}, {{item}}, .draw, yrep) %>%
    tidyr::pivot_wider(names_from = {{item}}, values_from = 'yrep') %>%
    select(-{{person}})%>% mutate(.draw = as.numeric(.draw)) %>%
    dplyr::group_by(.draw) %>% dplyr::group_split(.keep = TRUE) %>%
    list2array()

  message('Calculating posterior odds ratio')
  or_rep <- calculate_odds_ratio(yrep) # calculates odds ratio for posterior samples

  y <- make_responsedata_wider(model) %>% select(-dplyr::any_of(unlist(model$var_specs)))
  or_act <- calculate_odds_ratio(y) %>% # calculates odds ratio for actual sample/data
    mutate(.draw = 0, .before = 1)

  or_act_dat <- rep_dataframe(or_act, nrow(or_rep))
  or_dif <- or_rep - or_act_dat

  or <- or_dif %>% gather_or('or_dif') %>%
    dplyr::group_by(item1, item2) %>% tidyr::nest(or_dif_samples = c(.draw, or_dif))

  or_act <- or_act %>% gather_or('or_act') %>% select(-.draw)
  or_rep <- or_rep %>% gather_or('or_rep') %>% tidyr::nest(or_rep_samples = c(.draw, or_rep))
  or_ppp <- colMeans(or_dif %>% select(-.draw) > 0) %>% tibble::as_tibble(rownames = "itempair") %>% seperate_itempairs() %>%
    dplyr::rename(or_ppp = value)

  rope <- stats::sd(or_act$or_act)/10
  message(paste0('ROPE is set to sd(or_act)/10: ', round(rope,2)))
  or_dif_hdi <- or %>% tidyr::unnest() %>% tidybayes::mode_hdi(or_dif, .width = hdi_width) %>% dplyr::rename(or_dif_mode = or_dif) %>%
    mutate(above_zero = .lower > 0, beneath_zero = .upper < 0,
           above_rope = .lower > 0 + rope, beneath_rope = .upper < 0 - rope)

  or <- or %>% left_join(or_act, by = c('item1', 'item2')) %>%
    left_join(or_rep, by = c('item1', 'item2')) %>%
    left_join(or_dif_hdi, by = c('item1', 'item2')) %>%
    left_join(or_ppp, by = c('item1', 'item2')) %>%
    dplyr::relocate(dplyr::any_of(c('or_act', 'or_rep')), .after = item2) %>%
    dplyr::ungroup()

  attr(or, 'rope_width') <- rope

  return(or)
}

#' PPMC Odds ratio heatmap
#' Prints a heatmap of odds ratio differences used for post predictive model checking.
#' Be aware that setting use_rope to TRUE will highlight fewer items as probably problematic.
#'
#' @param or_data dataframe generated by birtms::get_or()
#' @param use_rope boolean; not only check if 0 is included in HDI but rather any value of a ROPE
#' @param alternative_color boolean; color highlight only the problematic items
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' \dontrun{
#' or_data <- get_or(fit, n_samples = 500)
#' plot_ppmc_or_heatmap(or_data)
#' }
plot_ppmc_or_heatmap <- function(or_data, use_rope = FALSE, alternative_color = FALSE) {
  if (use_rope) {
    above <- or_data$above_rope
    beneath <- or_data$beneath_rope
    cap <- '**Interpretation:** The grey fields represent items for which the HDI does not contain any value inside the ROPE around 0.<br>'
    cap2 <- '**Interpretation:** The colored fields represent items for which the HDI does not contain any value inside the ROPE around 0.<br>'
  } else {
    above <- or_data$above_zero
    beneath <- or_data$beneath_zero
    cap <- '**Interpretation:** The grey fields represent items for which the HDI does not contain a odds ratio difference of 0.<br>'
    cap2 <- '**Interpretation:** The colored fields represent items for which the HDI does not contain a odds ratio difference 0.<br>'
  }

  cap <- paste0(cap, 'Fields with an *L* represent items where predicted odds ratio is lower than actual observed odds ratio.<br>
    Fields with an *H* represent items where predicted odds ratio is higher than actual observed odds ratio.')
  cap2 <- paste0(cap2, '*Blue* fields represent items where predicted odds ratio is lower than actual observed odds ratio.<br>
    *Red* fields represent items where predicted odds ratio is higher than actual observed odds ratio.')

  or_data <- or_data %>% dplyr::mutate(z_or_dif_mode = scale(or_dif_mode), z_or_dif_mode_highlighted = ifelse(above | beneath, NA, z_or_dif_mode))

  g <- ggplot2::ggplot(or_data, ggplot2::aes(item1, item2, fill = z_or_dif_mode_highlighted, label = z_or_dif_mode_highlighted, height = 1, width = 1)) +
    ggplot2::scale_x_continuous("Item A", expand=c(0,0), position = "top", breaks = seq(min(or_data$item1),max(or_data$item1),1)) +
    ggplot2::scale_y_continuous("Item B", expand=c(0,0), breaks = seq(min(or_data$item2),max(or_data$item2),1)) +
    ggplot2::ggtitle('z-standardised Odds Ratio difference') +
    ggplot2::geom_tile(color="black") +
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.border= ggplot2::element_rect(size=2, color="black", fill=NA),
                   axis.ticks = ggplot2::element_blank(),plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0)) +
    ggplot2::coord_fixed()

  if(alternative_color) {
    or_data <- or_data %>% dplyr::mutate(z_or_dif_mode = scale(or_dif_mode), z_or_dif_mode_highlighted = ifelse(above | beneath, NA, abs(z_or_dif_mode)))

    g <- g + ggplot2::scale_fill_gradient(low = "white", high = "grey50", limit = c(0, 1), oob = scales::squish,
                                          na.value = '#ff0000', name = "\u007C z(\u0394OR) \u007C") +
      ggplot2::geom_tile(data = subset(or_data, above), fill = '#ca0020', color="black") +
      ggplot2::geom_tile(data = subset(or_data, beneath), fill = '#0571b0', color="black") +
      ggplot2::labs(caption = cap2)
  } else {
    g <- g + ggplot2::scale_fill_gradient2(low = "#0571b0", high = "#ca0020", mid = "#f7f7f7",
                                  midpoint = 0, limit = c(-1, 1), name = "z(\u0394OR)",
                                  oob = scales::squish, na.value = 'gray50') +
      ggplot2::geom_text(data = subset(or_data, above), ggplot2::aes(label = 'H'), size = 4) +
      ggplot2::geom_text(data = subset(or_data, beneath), ggplot2::aes(label = 'L'), size = 4) +
      ggplot2::labs(caption = cap)
  }

  return(g)
}
