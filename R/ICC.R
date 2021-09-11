# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "item.id", "r_item", "b_Intercept", "b_skillintercept_Intercept",
                                                        'person.id', 'response', 'group_id', 'score', 'item_score',
                                                        'sd_y', 'n', 'sd_x', 'mean_y', 'mean_x', 'person_sd', 'person_mean',
                                                        'se', 'group_sd_x', 'group_mean_x', 'group_mean_y', 'group_sd_y',
                                                        'item_nr', 'xerr', 'yerr', 'frame', 'r_item__beta', 'se_x'
                                                        ))


#' Plots ICC for an item
#'
#' @param model birtmsfit object
#' @param item_id int; item to plot ICC for
#' @param num_groups int; number of groups of persons
#' @param verbose boolean; should a explanation be added as a caption
#' @param post_responses list of data.frames from birtms::get_postdata
#' @param ellipse_type string; 'axisparallel' or 'norm' or 't' or 'none'
#' @param plot_post_person_estimated boolean
#'
#' @return ggplot object
#' @importFrom dplyr rename
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom stats sd
#' @export
#'
#' @examples
ICC_check <- function(model, item_id = 1, num_groups = NULL, verbose = FALSE, post_responses = NULL,
                      ellipse_type = "t", plot_post_person_estimated = TRUE) {
  stopifnot(model$model_specs$response_type == 'dichotom')
  stopifnot(model$model_specs$add_common_dimension == FALSE)
  stopifnot(model$model_specs$dimensinality_type == 'unidimensional')

  data_long <- model$data %>% mutate(item.id = get.item.id(.))
  person <- model$var_specs$person
  symperson <- sym({{person}})

  if (is.null(num_groups)) num_groups <- round(length(unique(data_long[[{{person}}]]))^(1/3))

  if (is.null(post_responses)) post_responses <- get_postdata(model)
  yrep_item <- getYrep(model, data_long = data_long, yrep = post_responses$yrep, item_id = item_id)

  table_person_values <- get.table_person_values(model = model, num_groups = num_groups)

  item_key <- data_long %>% select(item.id, item) %>% unique
  rownames(item_key) <- item_key$item

  if (model$model_specs$item_parameter_number == 1) {
    table_marg_pars <- model %>%
      tidybayes::spread_draws(r_item[item,], b_Intercept) %>% mutate(delta = b_Intercept+r_item, item_nr = item_key[item,"item.id"],
                                                                     alpha = 1, gamma = 0, psi = 0)

    r_person <- sym(paste0('r_',{{person}}))

  } else if (model$model_specs$item_parameter_number == 2) {
    table_marg_pars <- model %>%
      tidybayes::spread_draws(r_item__beta[item,], b_skillintercept_Intercept,
                              r_item__logalpha[item,], b_logalpha_Intercept) %>%
      mutate(delta = b_skillintercept_Intercept+r_item__beta,
             item_nr = item_key[item,"item.id"],
             alpha = exp(b_logalpha_Intercept+r_item__logalpha),
             gamma = 0, psi = 0)

    r_person <- sym(paste0('r_',{{person}},'__theta'))

  } else stop('Currently only 1pl and 2pl models are supported.')

  r_person_vec <- str2lang(paste0({{r_person}},'[',{{person}},',]'))

  message('datawrangling for ICCs')

  key <- get.scoregroup(model, num_groups = num_groups, table_person_values = table_person_values)
  yrep_item <- yrep_item %>% mutate(group_id = key[person.id,1], .before = 1) #%>% dplyr::arrange(group_id, person.id, item, sample) # ist das arrange notwendig?

  theta_post <- model %>% tidybayes::spread_draws(!!r_person_vec) %>% mutate({{symperson}} := as.character({{symperson}}),
                                                                             group_id = key[{{symperson}},1], .before = 1) %>%
    rename(theta = !!r_person) #%>% dplyr::arrange(group_id, {{person}})

  temp <- data_long %>% filter(item.id == item_id) %>% select({{person}}, response)

  data_gg <- table_person_values %>% select(group_id, {{person}}, theta, score) %>% #dplyr::arrange(ID) %>%
    left_join(temp, by = {{person}}) %>% rename(item_score = response) %>%
    # mutate(item_score = ifelse(ID %in% temp$ID, temp$response, NA)) %>%
    group_by(group_id)

  data_gg_summary <- data_gg %>% summarise(mean_y = mean(item_score, na.rm = T), sd_y = sd(item_score, na.rm = T), n = sum(!is.na(item_score)), se_y = sd_y/sqrt(n),
                                           mean_x = mean(theta, na.rm = T), sd_x = sd(theta, na.rm = T), se_x = sd_x/sqrt(n), .groups = 'drop') %>% rename(y = mean_y, yerr = sd_y, x = mean_x, xerr = sd_x)

  data_gg_post <- yrep_item %>% group_by(group_id, person.id) %>%
    summarise(person_mean = mean(response, na.rm = T), person_sd = sd(response, na.rm = T), n = sum(!is.na(response)), se = person_sd/sqrt(n), .groups = 'drop') %>%
    rename(y = person_mean, yerr = person_sd, se_x = se, n_x = n) %>% ungroup() #%>% dplyr::arrange(group_id, person.id)

  temp2 <- theta_post %>% group_by(group_id, {{symperson}}) %>% #filter({{symperson}} %in% unique(yrep_item$person.id)) %>% # sp\u00E4ter filtern viel schneller!
    summarise(person_mean = mean(theta, na.rm = T), person_sd = sd(theta, na.rm = T), n = sum(!is.na(theta)), se = person_sd/sqrt(n), .groups = 'drop') %>%
    rename(x = person_mean, xerr = person_sd, se_y = se, n_y = n) %>% ungroup %>% #dplyr::arrange(group_id, {{person}}) %>%
    rename(person.id = {{person}}) %>% filter(person.id %in% unique(yrep_item$person.id))

  data_gg_post <- data_gg_post %>% left_join(temp2, by = c('person.id', 'group_id'))

  data_gg_post_summary <- data_gg_post %>% group_by(group_id) %>% summarise(
    group_mean_x = mean(x), group_sd_x = sd(x), group_mean_y = mean(y), group_sd_y = sd(y),
    n = dplyr::n(), se_x = group_sd_x/sqrt(n), se_y = group_sd_x/sqrt(n), .groups = 'drop') %>%
    rename(x = group_mean_x, y = group_mean_y, xerr = group_sd_x, yerr = group_sd_y)

  # Plot
  message('plotting ICCs')

  item_params <- table_marg_pars %>% filter(item_nr == item_id)

  g <- plot_ICC_cont(item_params[["delta"]], item_params[["alpha"]], item_params[["gamma"]], item_params[["psi"]], print = FALSE)

  g <- g + ggnewscale::new_scale_fill() +
    ggplot2::scale_fill_discrete(guide = ggplot2::guide_legend(title = "vorhergesagte Antwortquoten\nder Personen", nrow = 1,
                                             title.position = "top", label.position = "bottom", label.hjust = 0.5)) +
    ggplot2::scale_colour_discrete(guide = ggplot2::guide_legend(title = "vorhergesagte Antwortquoten\nder Personen", nrow = 1,
                                               title.position = "top", label.position = "bottom", label.hjust = 0.5))

  ellipse_cred_mass <- c(.51, .66, .81, .96)

  if (ellipse_type == "axisparallel") {
    # Forces ellipses axes to be parallel to the coordinate axes
    for (i in stats::qnorm(0.5+0.5*ellipse_cred_mass)) {
      g <- g + ggplot2::geom_polygon(data = error_ellipses(mutate(data_gg_post_summary, xerr = i*xerr, yerr = i*yerr)),
                            aes(x = x, y = y, group = frame, fill = frame, colour = frame), alpha = .15, lty = 'dotted')
    }
  } else if ( ellipse_type %in% c("norm", "t")) {
    for (i in ellipse_cred_mass) {
      g <- g + ggplot2::stat_ellipse(data = data_gg_post, geom = "polygon", mapping = ggplot2::aes(x = x, y = y, fill = factor(group_id), colour = factor(group_id)),
                            alpha = .15, lty = 'dotted', level = i, type = "norm") # assumes data to be normal rather than t distributed
    }
  } else if (ellipse_type == "none") {

  } else stop('This axis type is not valid.')

  if(plot_post_person_estimated) {
    g <- g + ggplot2::geom_point(data = data_gg_post, ggplot2::aes(x = x, y = y, colour = factor(group_id)), pch=3)
  }
  # g <- g + geom_point(data = data_gg, aes(x = theta, y = item_score, colour = factor(group_id)))
  # g <- g + geom_point(data = data_gg, aes(x = theta, y = inv_logit_scaled(theta), colour = factor(group_id)))

  g <- g +
    # geom_point(data = data_gg, aes(x = theta, y = item_score, colour = factor(group_id))) +
    # geom_pointinterval(data = data_gg_summary, aes(x = x, y = y, colour = factor(group_id), xmin = .lower, xmax = .upper, size = se_x)) +
    # ggnewscale::new_scale_colour() + ggnewscale::new_scale_fill() +
    ggplot2::geom_point(data = data_gg_post_summary, ggplot2::aes(x = x, y = y), pch=3, show.legend = FALSE, size = 5, stroke = 4) +
    ggplot2::geom_point(data = data_gg_post_summary, ggplot2::aes(x = x, y = y, colour = factor(group_id)), pch=3, show.legend = FALSE, size = 6.4, stroke = 2) +
    ggplot2::guides(fill=ggplot2::guide_legend(title = "gemittelte Antwortquote\nder Rohsummengruppen", nrow = 1, title.position = "top", label.position = "bottom",
                             label.hjust = 0.5)) +
    ggplot2::geom_line(data = data_gg_summary, ggplot2::aes(x = x, y = y), size = 1) +
    ggplot2::geom_point(data = data_gg_summary, ggplot2::aes(x = x, y = y, fill = factor(group_id), size = se_x), pch=21, stroke = 1.5) +
    ggplot2::scale_size(range = c(4, 10)) +
    ggplot2::guides(size=ggplot2::guide_legend(title = "Standardfehler der gemittelten\nF\u00E4higkeit der Rohsummengruppen", nrow = 1, title.position = "top", label.position = "bottom",
                             label.hjust = 0.5)) +
    ggplot2::ggtitle(paste0('Item ', unique(data_long %>% filter(item.id == item_id) %>% select(item))),
            subtitle = paste0('Itemnr. ', item_id, ' (', sum(!is.na(data_gg$item_score)), ' Beobachtungen)')) +
    ggplot2::xlab("F\u00E4higkeit [logit]") + ggplot2::ylab("beobachtete Antwortquote / vorhergesagte Antwortwahrscheinlichkeit") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5), plot.subtitle = ggplot2::element_text(hjust = 0.5),
          plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0.5))

  if (verbose) {
    g <- g +
      ggplot2::labs(caption="**Erkl\u00E4rung:** Die Kreise markieren die \u00FCber die Rohsummengruppen gemittelten Antwortquoten und F\u00E4higkeiten der Beobachtung.<br>
         Die gr\u00F6\u00DFe der Punkte entspricht dem Standardfehler der F\u00E4higkeitsmittelwerte und ist damit proportional zu deren Streuung innerhalb der Gruppen.<br>
         Die kleinen Punkte repr\u00E4sentieren die \u00FCber alle Posteriosamples gemittelten vorhergesagten Antwortquoten und F\u00E4higkeiten der Einzelpersonen.<br>
         Die Ellipsen repr\u00E4sentieren die entsprechenden Wahrscheinlichkeitsdichten der Mittelwertverteilung der \u00FCber die Rohsummengruppen aggregierten<br>
         Antwortquoten und F\u00E4higkeiten. Deren Zentren sind jeweils mit einem Kreuz markiert.<br>
         Sie bilden somit das auf den Posteriorsamples basierende Pendant zu den auf den tats\u00E4chlichen Beobachtungen basierenden Kreisen.")
  }

  return(g)
}

get.item.id <- function(data_long) {
  key <- seq_along(unique(data_long$item))
  # print(key)
  names(key) <- unique(data_long$item)
  data_long %>% mutate(item.id = key[item], .after = item) %>% dplyr::pull(item.id) %>%
    return()
}

#' Generates a list of posterior predictions and probabilities
#'
#' @param model birtmsfit object
#' @param subset integer vector; which posterior samples should be used?
#'
#' @return list of data.frames
#' @export
#'
#' @examples
get_postdata <- function(model = NULL, subset = NULL) {

  message('loading post responses')
  yrep <- brms::posterior_predict(model, subset = subset)
  message('loaded')

  message('loading post probs')
  ppe <- brms::posterior_epred(model, subset = subset)
  message('loaded')

  return(list('yrep' = yrep, 'ppe' = ppe, 'subset' = subset))
}

getYrep <- function(model, data_long = NULL, yrep, item_id = 1) {
  message('generating yrep')

  person <- model$var_specs$person
  # symperson <- sym(person)

  n <- nrow(yrep)
  long.rep <- yrep %>% t() %>% as.data.frame() %>% mutate({{person}} := data_long[[{{person}}]], item = data_long$item, item_id = data_long$item.id, .before = 1)
  names(long.rep) <- c("person.id", "item", "item.id", c(1:n))

  yrep <- long.rep %>% dplyr::filter(item.id == item_id) %>%
    tidyr::pivot_longer(cols = '1':colnames(.)[ncol(.)], names_to = 'sample', values_to = 'response')

  message('finished')

  return(yrep)
}

get.table_person_values <- function (model, num_groups = 5) {
  person <- model$var_specs$person
  symperson <- sym(person)

  table_person_values <- model$data %>% group_by({{symperson}}) %>% summarise(score = sum(response), .groups = 'drop') %>%
    mutate(theta = brms::ranef(model)[[{{person}}]][,1,1]) %>% dplyr::arrange(score, {{person}}) %>%
    mutate(order = 1:nrow(.), .before = 1)

  key <- get.scoregroup(model, num_groups = num_groups, table_person_values = table_person_values)
  table_person_values <- table_person_values %>% #dplyr::arrange({{symperson}}) %>%
    mutate(group_id = key$group_id, .before = 1) #%>% dplyr::arrange(group_id, {{symperson}})

  return(table_person_values)
}

get.scoregroup <- function(model, num_groups = 5, table_person_values = NULL) {
  person <- model$var_specs$person
  symperson <- sym(person)

  num_persons <- nrow(table_person_values)

  breaks <- round(seq(1, num_persons, by = num_persons/num_groups)) %>% as.data.frame() %>% t() %>% as.data.frame() %>%
    mutate(num_persons+1) %>% stats::setNames(NULL) %>% t()

  group_vec <- c(rep(NA, num_persons))
  for (i in 2:length(breaks)) {
    group_vec[breaks[i-1]:(breaks[i]-1)] <- c(rep(i-1, length(breaks[i-1]:(breaks[i]-1))))
  }

  key <- table_person_values %>% dplyr::arrange(order) %>% mutate(group_id = group_vec, .before = 1) %>% select(group_id, {{symperson}}) %>%
    #dplyr::arrange({{symperson}}) %>%
    as.data.frame()
  rownames(key) <- unique(table_person_values[[{{person}}]])

  return(key)
}

plot_ICC_cont <- function (delta, alpha = NULL, gamma = NULL, psi = NULL, p_interval = 'median_qi', step_size = 0.1, print = TRUE, width = seq(0,0.96,0.12)) {
  if (is.null(delta)) {
    stop("Bitte geben sie f\u00FCr ein 1PL-Modell mindestens einen Vektor f\u00FCr die Itemleichtigkeit (delta) ein.")
  }

  data <- tibble::tibble(x = seq(from = -4.5, to = 4.5, by = step_size)) %>%  dplyr::group_by_all()

  data <- data %>% dplyr::do(tibble::tibble(y = icc_function(.$x, delta = delta, alpha = alpha, gamma = gamma, psi = psi)))

  g <- data  %>%  ggplot2::ggplot(ggplot2::aes(x = x, y = y)) +
    tidybayes::stat_lineribbon(aes(fill = ggplot2::after_stat(.width)), .width = width, point_interval = p_interval, colour = 'white', size = .75) +
    ggplot2::scale_fill_binned(low = "#1d1d1d", high = "#E1E1E1", limit = c(0, 0.96), show.limits = TRUE, breaks = seq(0.12, 0.84, by = 0.12),
                      guide = ggplot2::guide_coloursteps(title = 'Glaubw\u00FCrdigkeitsintervall in %', nrow = 1, title.position = "top", label.position = "bottom",
                                                barwidth = 10, frame.colour = NULL), labels = scale100) +
    ggplot2::theme(legend.position="bottom", legend.box = "horizontal") +
    ggplot2::coord_cartesian(xlim = c(-4,4), ylim = c(0,1), expand = TRUE, default = FALSE, clip = "on")

  if (print) {
    print(g)
  }

  return(g)
}

icc_function <- function(x, delta = NULL, alpha = NULL, gamma = NULL, psi = NULL) {
  if (is.null(delta)) {
    delta = 0;
  }
  if (is.null(alpha)) {
    alpha = (0*delta)+1;
  }
  if (is.null(gamma)) {
    gamma = 0;
  }
  if (is.null(psi)) {
    psi = 0;
  }

  gamma + (1 - gamma - psi) * brms::inv_logit_scaled(alpha %*% t(x) + delta) %>%
    return()
}

scale100 <- function (x) {
  scales::number_format(accuracy = 1,
                        scale = 100,
                        suffix = "",
                        big.mark = "")(x)
}

ellipses <- function(center = c(0, 0), axes = c(1, 1), npoints = 101){
  tt <- seq(0,2*pi, length.out = npoints)
  xx <- center[1] + axes[1] * cos(tt)
  yy <- center[2] + axes[2] * sin(tt)
  return(data.frame(x = xx, y = yy))
}

error_ellipses <- function(dd) {
  dd$frame <- factor(seq(1:nrow(dd)))

  ddEll <- data.frame()
  for(k in levels(dd$frame)){
    ddEll <- rbind(ddEll, cbind(as.data.frame(with(dd[dd$frame == k,],
                                                   ellipses(center = c(x, y), axes = c(xerr, yerr), npoints = 101))),frame = k))
  }

  ddEll %>% group_by(frame) %>%
    return()
}
