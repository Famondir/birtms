ICC_check <- function(item_id = 1, model, num_groups = NULL, post_responses = NULL) {
  data_long <- model$data %>% mutate(item.id = get.item.id(.))
  person <- model$var_specs$person

  if (is.null(num_groups)) num_groups <- round(length(unique(data_long[[{{person}}]]))^(1/3))

  if (is.null(post_responses)) {
    stop('Die vom Modell vorhergesagten Beobachtungen (post_responses) fehlen.')
  } else {
    yrep_item <- getYrep(model, data_long = data_long, yrep = post_responses$yrep, item_id = item_id)
  }

  table_person_values <- get.table_person_values(model = model, num_groups = num_groups)

  beta = FALSE
  delta = FALSE
  alpha = FALSE
  gamma = FALSE
  logitgamma = FALSE
  testlet = FALSE

  pars <- model[["fit"]]@model_pars

  if (any(str_detect(pars, 'beta'))) {
    beta = TRUE
  } else {
    stop('No item difficulties specified as beta.')
  }
  if (any(str_detect(pars, 'delt'))) {
    delta = TRUE
  } else {
    stop('No derived item difficulties specified as delta found.')
  }
  if (any(str_detect(pars, 'alpha'))) {
    alpha = TRUE
  }
  if (any(str_detect(pars, 'logitgamma'))) {
    logitgamma = TRUE
  }
  if (any(pars == 'gamma')) {
    gamma = TRUE
  }
  if (any(str_detect(pars, 'testlet'))) {
    testlet = TRUE
  }

  print('extracting itempars')

  if (testlet) {
    if (gamma) {
      table_marg_pars_3PL_Testlets_tidy <- model %>%
        spread_draws(marginalized_alpha[item_nr], marginalized_delta[item_nr], gamma[item_nr]) %>% mutate(item = unique(data_long$item)[item_nr])
    } else if (logitgamma) {
      warning('Modelle ohne abgeleiteten Parameter gamma werden nicht weiter unterstützt')
      gamma_table <- model %>% spread_draws(r_item__logitgamma[item], b_logitgamma_Intercept) %>%
        mutate(logitgamma = r_item__logitgamma + b_logitgamma_Intercept, gamma = inv_logit_scaled(logitgamma))
      table_marg_pars_3PL_Testlets_tidy <- model %>%
        spread_draws(marginalized_alpha[item_nr], marginalized_delta[item_nr]) %>% mutate(item = unique(data_long$item)[item_nr]) %>%
        ungroup %>% mutate(gamma = gamma_table$gamma)
    } else if (alpha) {
      table_marg_pars_2PL_Testlets_tidy <- model %>%
        spread_draws(marginalized_alpha[item_nr], marginalized_delta[item_nr]) %>% mutate(item = unique(data_long$item)[item_nr])
    } else {
      table_marg_pars_1PL_Testlets_tidy <- model %>%
        spread_draws(marginalized_delta[item_nr]) %>% mutate(item = levels(data_long$item)[item_nr])
    }
  } else {
    # stop('Modelle ohne Testlet noch nicht implementiert')
    if (gamma) {
      table_marg_pars_3PL_tidy <- model %>%
        spread_draws(delt[item_nr], alpha1[item_nr], gamma[item_nr]) %>% mutate(item = levels(data_long$item)[item_nr])
    } else if (alpha) {
      table_marg_pars_2PL_tidy <- model %>%
        spread_draws(delt[item_nr], alpha1[item_nr]) %>% mutate(item = levels(data_long$item)[item_nr])
    } else {
      table_marg_pars_1PL_tidy <- model %>%
        spread_draws(delt[item_nr]) %>% mutate(item = levels(data_long$item)[item_nr])
    }
  }

  print('datawrangling for ICCs')

  key <- get.scoregroup(num_groups = num_groups, table_person_values = table_person_values)
  yrep_item <- yrep_item %>% mutate(group_id = key[person.id,1], .before = 1) %>% arrange(group_id, person.id, item, sample)
  # table_person_values <- table_person_values %>% arrange(ID) %>% mutate(group_id = key$group_id, .before = 1) %>% arrange(group_id, ID) # schon in der Helperfunktion erledigt

  theta_post <- model %>% spread_draws(r_ID__theta[ID,]) %>% mutate(group_id = key[ID,1], .before = 1) %>% rename(theta = r_ID__theta) %>% arrange(group_id, ID)

  # if (testlet) {
  #   theta_testlet_post <- model %>% spread_draws(r_ID__testlet[ID,testlet])
  #
  #   testlet_nr <- get.testlet.id(data_long)[[item_id]]
  #   testlet_name <- unique(theta_testlet_post$testlet)[[testlet_nr]]
  #   theta_testlet_post2 <- theta_testlet_post %>% filter(testlet == testlet_name) %>% ungroup()
  #   theta_post2 <- theta_post %>% ungroup() %>% left_join(theta_testlet_post2) %>% mutate(theta_adj = theta + r_ID__testlet)
  #   theta_post <- theta_post %>% ungroup() %>% mutate(theta = theta_post2$theta_adj)
  # }

  temp <- data_long %>% filter(item.id == item_id) %>% select(ID, response)

  data_gg <- table_person_values %>% select(group_id, ID, theta, score) %>% arrange(ID) %>% left_join(temp, by = 'ID') %>% rename(item_score = response) %>%
    # mutate(item_score = ifelse(ID %in% temp$ID, temp$response, NA)) %>%
    group_by(group_id)

  data_gg_summary <- data_gg %>% summarise(mean_y = mean(item_score, na.rm = T), sd_y = sd(item_score, na.rm = T), n = sum(!is.na(item_score)), se_y = sd_y/sqrt(n),
                                           mean_x = mean(theta, na.rm = T), sd_x = sd(theta, na.rm = T), se_x = sd_x/sqrt(n), .groups = 'drop') %>% rename(y = mean_y, yerr = sd_y, x = mean_x, xerr = sd_x)

  data_gg_post <- yrep_item %>% group_by(group_id, person.id) %>%
    summarise(person_mean = mean(response, na.rm = T), person_sd = sd(response, na.rm = T), n = sum(!is.na(response)), se = person_sd/sqrt(n), .groups = 'drop') %>%
    rename(y = person_mean, yerr = person_sd, se_x = se, n_x = n) %>% ungroup() %>% arrange(group_id, person.id)

  temp2 <- theta_post %>% group_by(group_id, ID) %>% filter(ID %in% unique(yrep_item$person.id)) %>%
    summarise(person_mean = mean(theta, na.rm = T), person_sd = sd(theta, na.rm = T), n = sum(!is.na(theta)), se = person_sd/sqrt(n), .groups = 'drop') %>%
    rename(x = person_mean, xerr = person_sd, se_y = se, n_y = n) %>% ungroup %>% arrange(group_id, ID) %>% rename(person.id = ID)

  data_gg_post <- data_gg_post %>% left_join(temp2, by = c('person.id', 'group_id'))

  data_gg_post_summary <- data_gg_post %>% group_by(group_id) %>% summarise(
    group_mean_x = mean(x), group_sd_x = sd(x), group_mean_y = mean(y), group_sd_y = sd(y),
    n = n(), se_x = group_sd_x/sqrt(n), se_y = group_sd_x/sqrt(n), .groups = 'drop') %>%
    rename(x = group_mean_x, y = group_mean_y, xerr = group_sd_x, yerr = group_sd_y)

  # Plot
  print('plotting ICCs')

  if (testlet) {
    if (logitgamma) {
      gamma.median <- median(pull(filter(table_marg_pars_3PL_Testlets_tidy, item_nr == item_id), gamma))
      delta.median <- median(pull(filter(table_marg_pars_3PL_Testlets_tidy, item_nr == item_id), marginalized_delta))
      alpha.median <- median(pull(filter(table_marg_pars_3PL_Testlets_tidy, item_nr == item_id), marginalized_alpha))
      alpha.median.adjust <- alpha.median*(1-gamma.median)
      theta.5 <- (logit_scaled((0.5 - gamma.median)/(1 - gamma.median))-delta.median)/alpha.median

      # gamma.median <- 0
      # delta.median <- 1
      # alpha.median <- 0.5
      # alpha.median.adjust <- alpha.median*(1-gamma.median)


      g <- plot_ICC_cont(delta = pull(filter(table_marg_pars_3PL_Testlets_tidy, item_nr == item_id), marginalized_delta),
                         alpha = pull(filter(table_marg_pars_3PL_Testlets_tidy, item_nr == item_id), marginalized_alpha),
                         gamma = pull(filter(table_marg_pars_3PL_Testlets_tidy, item_nr == item_id), gamma), print = FALSE)

      # g <- plot_ICC_cont(delta = delta.median,
      #                    alpha = alpha.median,
      #                    gamma = gamma.median, print = FALSE)

      g <- g + annotate("label", x = -4, y = .9, hjust = 0,
                        label = c(paste0('Schwierigkeit: ', round(-delta.median, 3),
                                         '\nTrennschärfe: ', round(alpha.median.adjust, 3),
                                         '\nPseuderatew.: ', round(gamma.median, 3),
                                         '\nTheta(p = 0.5): ', round(theta.5, 3),
                                         '\nTheta(p = ', round((1+gamma.median)/2,3), '): ', round(-delta.median/alpha.median, 3))))

      # g <- g + geom_hline(yintercept = gamma.median, linetype="dotted", alpha = .5) +
      #   geom_hline(yintercept = (1+gamma.median)/2, linetype="dotted", alpha = .5) +
      #   geom_vline(xintercept = -delta.median/alpha.median, linetype="dotted", alpha = .5)

    } else if (alpha) {
      delta.median <- median(pull(filter(table_marg_pars_2PL_Testlets_tidy, item_nr == item_id), marginalized_delta))
      alpha.median <- median(pull(filter(table_marg_pars_2PL_Testlets_tidy, item_nr == item_id), marginalized_alpha))

      g <- plot_ICC_cont(delta = pull(filter(table_marg_pars_2PL_Testlets_tidy, item_nr == item_id), marginalized_delta),
                         alpha = pull(filter(table_marg_pars_2PL_Testlets_tidy, item_nr == item_id), marginalized_alpha),
                         print = FALSE) # 2PL

      # g <- g + geom_hline(yintercept = 1/2, linetype="dotted", alpha = .5) +
      #   geom_vline(xintercept = -delta.median, linetype="dotted", alpha = .5)
    } else {
      g <- plot_ICC_cont(delta = pull(filter(table_marg_pars_1PL_Testlets_tidy, item_nr == item_id), marginalized_delta),
                         print = FALSE) # 1PL
    }
  } else {
    if (gamma) {
      g <- plot_ICC_cont(delta = pull(filter(table_marg_pars_3PL_tidy, item_nr == item_id), delt),
                         alpha = pull(filter(table_marg_pars_3PL_tidy, item_nr == item_id), alpha1),
                         gamma = pull(filter(table_marg_pars_3PL_tidy, item_nr == item_id), gamma), print = FALSE) +
        geom_hline(yintercept = median(pull(filter(table_marg_pars_3PL_tidy, item_nr == item_id), gamma)), linetype="dotted", alpha = .5)
    } else if (alpha) {
      g <- plot_ICC_cont(delta = pull(filter(table_marg_pars_2PL_tidy, item_nr == item_id), delt),
                         alpha = pull(filter(table_marg_pars_2PL_tidy, item_nr == item_id), alpha1), print = FALSE) # 2PL
    } else {
      g <- plot_ICC_cont(delta = pull(filter(table_marg_pars_1PL_tidy, item_nr == item_id), delt),
                         print = FALSE) # 1PL
    }
  }

  g <- g + ggnewscale::new_scale_fill() +
    scale_fill_discrete(guide = guide_legend(title = "vorhergesagte Antwortquoten\nder Personen", nrow = 1,
                                             title.position = "top", label.position = "bottom", label.hjust = 0.5)) +
    scale_colour_discrete(guide = guide_legend(title = "vorhergesagte Antwortquoten\nder Personen", nrow = 1,
                                               title.position = "top", label.position = "bottom", label.hjust = 0.5))

  # for (i in c(0.7, 0.95, 1.3, 2.05)) {
  #   g <- g + geom_polygon(data = error_ellipses(mutate(data_gg_post_summary, xerr = i*xerr, yerr = i*yerr)),
  #                         aes(x = x, y = y, group = frame, fill = frame, colour = frame), alpha = .15, lty = 'dotted')
  # }

  for (i in c(.51, .66, .81, .96)) {
    g <- g + stat_ellipse(data = data_gg_post, geom = "polygon", mapping = aes(x = x, y = y, fill = factor(group_id), colour = factor(group_id)),
                          alpha = .15, lty = 'dotted', level = i)
  }

  g <- g + geom_point(data = data_gg_post, aes(x = x, y = y, colour = factor(group_id)), pch=3)
  # g <- g + geom_point(data = data_gg, aes(x = theta, y = item_score, colour = factor(group_id)))
  # g <- g + geom_point(data = data_gg, aes(x = theta, y = inv_logit_scaled(theta), colour = factor(group_id)))

  g <- g +
    # geom_point(data = data_gg, aes(x = theta, y = item_score, colour = factor(group_id))) +
    # geom_pointinterval(data = data_gg_summary, aes(x = x, y = y, colour = factor(group_id), xmin = .lower, xmax = .upper, size = se_x)) +
    # ggnewscale::new_scale_colour() + ggnewscale::new_scale_fill() +
    geom_point(data = data_gg_post_summary, aes(x = x, y = y), pch=3, show.legend = FALSE, size = 5, stroke = 4) +
    geom_point(data = data_gg_post_summary, aes(x = x, y = y, colour = factor(group_id)), pch=3, show.legend = FALSE, size = 6.4, stroke = 2) +
    guides(fill=guide_legend(title = "gemittelte Antwortquote\nder Rohsummengruppen", nrow = 1, title.position = "top", label.position = "bottom",
                             label.hjust = 0.5)) +
    geom_line(data = data_gg_summary, aes(x = x, y = y), size = 1) +
    geom_point(data = data_gg_summary, aes(x = x, y = y, fill = factor(group_id), size = se_x), pch=21, stroke = 1.5) +
    scale_size(range = c(4, 10)) +
    guides(size=guide_legend(title = "Standardfehler der gemittelten\nFähigkeit der Rohsummengruppen", nrow = 1, title.position = "top", label.position = "bottom",
                             label.hjust = 0.5)) +
    ggtitle(paste0('Item ', unique(data_long %>% filter(item.id == item_id) %>% select(item))),
            subtitle = paste0('Itemnr. ', item_id, ' (', sum(!is.na(data_gg$item_score)), ' Beobachtungen)')) +
    xlab("Fähigkeit [logit]") + ylab("beobachtete Antwortquote / vorhergesagte Antwortwahrscheinlichkeit") +
    theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5),
          plot.caption = ggtext::element_markdown(lineheight = 1.5, hjust = 0.5)) +
    labs(caption="**Erklärung:** Die Kreise markieren die über die Rohsummengruppen gemittelten Antwortquoten und Fähigkeiten der Beobachtung.<br>
         Die größe der Punkte entspricht dem Standardfehler der Fähigkeitsmittelwerte und ist damit proportional zu deren Streuung innerhalb der Gruppen.<br>
         Die kleinen Punkte repräsentieren die über alle Posteriosamples gemittelten vorhergesagten Antwortquoten und Fähigkeiten der Einzelpersonen.<br>
         Die Ellipsen repräsentieren die entsprechenden Wahrscheinlichkeitsdichten der Mittelwertverteilung der über die Rohsummengruppen aggregierten<br>
         Antwortquoten und Fähigkeiten. Deren Zentren sind jeweils mit einem Kreuz markiert.<br>
         Sie bilden somit das auf den Posteriorsamples basierende Pendant zu den auf den tatsächlichen Beobachtungen basierenden Kreisen.")


  return(g)
}

get.item.id <- function(data_long) {
  key <- seq_along(unique(data_long$item))
  # print(key)
  names(key) <- unique(data_long$item)
  data_long %>% mutate(item.id = key[item], .after = item) %>% pull(item.id) %>%
    return()
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

  gamma + (1 - gamma - psi) * inv_logit_scaled(alpha %*% t(x) + delta) %>%
    return()
}

get_postdata <- function(model = NULL, subset = NULL) {

  print('loading post responses')
  yrep <- posterior_predict(model, subset = subset)
  print('loaded')

  print('loading post probs')
  ppe <- posterior_epred(model, subset = subset)
  print('loaded')

  return(list('yrep' = yrep, 'ppe' = ppe, 'subset' = subset))
}

getYrep <- function(model, data_long = NULL, yrep, item_id = 1) {
  print('generating yrep')

  person <- model$var_specs$person

  n <- nrow(yrep)
  long.rep <- yrep %>% t() %>% as.data.frame() %>% mutate(ID = data_long[{{person}}], item = data_long$item, item_id = data_long$item.id, .before = 1)
  names(long.rep) <- c("person.id", "item", "item.id", c(1:n))

  yrep <- long.rep %>% filter(item.id == item_id) %>% pivot_longer(cols = '1':colnames(.)[ncol(.)], names_to = 'sample', values_to = 'response')

  print('finished')

  return(yrep)
}

get.table_person_values <- function (model, num_groups = 5) {
  symperson <- sym(model$var_specs$person)
  person <- model$var_specs$person

  table_person_values <- model$data %>% group_by({{symperson}}) %>% summarise(score = sum(response), .groups = 'drop') %>% mutate(theta = ranef(model)[[{{person}}]][,1,1]) %>% arrange(score, {{person}}) %>%
    mutate(order = 1:nrow(.), .before = 1)

  key <- get.scoregroup(model, num_groups = num_groups, table_person_values = table_person_values)
  table_person_values <- table_person_values %>% arrange({{symperson}}) %>% mutate(group_id = key$group_id, .before = 1) %>% arrange(group_id, {{symperson}})

  return(table_person_values)
}

get.scoregroup <- function(model, num_groups = 5, table_person_values = NULL) {
  symperson <- sym(model$var_specs$person)
  person <- model$var_specs$person

  num_persons <- nrow(table_person_values)

  breaks <- round(seq(1, num_persons, by = num_persons/num_groups)) %>% as.data.frame() %>% t() %>% as.data.frame() %>%
    mutate(num_persons+1) %>% setNames(NULL) %>% t()

  group_vec <- c(rep(NA, num_persons))
  for (i in 2:length(breaks)) {
    group_vec[breaks[i-1]:(breaks[i]-1)] <- c(rep(i-1, length(breaks[i-1]:(breaks[i]-1))))
  }

  key <- table_person_values %>% mutate(group_id = group_vec, .before = 1) %>% select(group_id, {{symperson}}) %>%
    arrange({{symperson}}) %>% as.data.frame()
  rownames(key) <- unique(table_person_values[[{{person}}]])

  return(key)
}
