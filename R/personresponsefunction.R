plot_personresponsefunction <- function(model, personresponsedata, id, persResFunStyle = 1) {
  scatter = ifelse(1 %in% persResFunStyle, TRUE, FALSE)
  pp = ifelse(2 %in% persResFunStyle, TRUE, FALSE)
  pp_scatter = ifelse(3 %in% persResFunStyle, TRUE, FALSE)
  force_se = ifelse(4 %in% persResFunStyle, TRUE, FALSE)

  person <- model$var_specs$person
  personsym <- sym(person)

  personnames <- unique(model$data[[person]])
  person_key <- seq(length(personnames))
  names(person_key) <- unique(model$data[[person]])

  data <- personresponsedata %>% filter({{personsym}} %in% names(person_key)[id]) %>%
    mutate({{person}} := paste0({{personsym}}, " (", person_key[{{personsym}}], ")"))
  # id <- unique(data[[person]])

  if (length(id) > 2) {
    multi = TRUE
  } else {
    multi = FALSE
  }

  if (force_se) {
    multi = FALSE
  }

  g <- data %>% group_by({{personsym}}) %>% ggplot() +
    geom_smooth(aes(x = diff, y = response, color = factor({{personsym}}), fill = factor({{personsym}})), se = !multi) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = 'Personresponsefunction', x = "relative item difficulty",
         y = "response probability", color = "Person (ID)", fill = "Person (ID)")

  if (scatter) {
    g <- g + geom_point(aes(x = diff, y = response, color = factor({{personsym}})), alpha = .2)
  }

  if (pp) {
    g <- g + geom_smooth(aes(x = diff, y = ppe, color = factor({{personsym}})), linetype = "dashed")
  }

  if (pp_scatter) {
    g <- g + geom_point(aes(x = diff, y = ppe, color = factor({{personsym}})), alpha = .2)
  }

  return(g)
}

calc.personresponsefunction <- function(model, post_responses, id = NULL) {
  person <- model$var_specs$person
  personsym <- sym(person)

  personnames <- unique(model$data[[person]])
  person_key <- seq(length(personnames))
  names(person_key) <- unique(model$data[[person]])

  if (is.null(id)) id <- seq(length(personnames))

  eprd <- make_post_longer(model = model, postdata = post_responses, 'ppe') %>%
    mutate(person.id = person_key[{{personsym}}])

  eprd_person <- eprd %>% filter(person.id %in% id) %>% group_by(item, {{personsym}}) %>%
    summarise(ppe = median(ppe)) %>% ungroup()

  answers <- model$data %>% filter({{personsym}} %in% names(person_key)[id])
  theta <- model %>% get.table_person_values() %>% select({{person}}, theta) %>%
    filter({{personsym}} %in% names(person_key)[id])

  data <- get.delta_for_wrigthmap(model) %>% rename(item = rowname) %>%
    left_join(eprd_person, by = 'item') %>% left_join(answers, by = c('item', {{person}})) %>%
    left_join(theta, by = {{person}}) %>% mutate(diff = delta - theta) %>% filter(!is.na(diff))

  return(data)
}

make_post_longer <- function(model, postdata, name) {
  message('Some datawrangling')

  postdata <- postdata[[name]] %>% t() %>%
    as.data.frame() %>%
    cbind(model$data) %>%
    gather("draw", !!name, starts_with("V"))

  return(postdata)
}
