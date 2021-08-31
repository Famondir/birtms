# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c('ppe', 'rowname', 'delta'))

#' PLots person response functions
#'
#' @param model birtmsfit object
#' @param personresponsedata data.frame from birtms::calc.personresponsedata
#' @param id integer vector; persons to plot PRF for
#' @param persResFunStyle iteger vector; 1 = scatter, 2 = posterior predict based PRF, 3 = pp_scatter, 4 = force_se
#'
#' @return ggplot object
#' @export
#'
#' @examples
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

  g <- data %>% group_by({{personsym}}) %>% ggplot2::ggplot() +
    ggplot2::geom_smooth(aes(x = diff, y = response, color = factor({{personsym}}), fill = factor({{personsym}})), se = !multi) +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(title = 'Personresponsefunction', x = "relative item difficulty",
         y = "response probability", color = "Person (ID)", fill = "Person (ID)")

  if (scatter) {
    g <- g + ggplot2::geom_point(aes(x = diff, y = response, color = factor({{personsym}})), alpha = .2)
  }

  if (pp) {
    g <- g + ggplot2::geom_smooth(aes(x = diff, y = ppe, color = factor({{personsym}})), linetype = "dashed")
  }

  if (pp_scatter) {
    g <- g + ggplot2::geom_point(aes(x = diff, y = ppe, color = factor({{personsym}})), alpha = .2)
  }

  return(g)
}

#' Calculate person response data
#'
#' @param model birtmsfit object
#' @param post_responses data.frame from birtms::
#' @param id integer vector; persons to generate data for
#'
#' @return data.frame to use in birtms::plot_personresponsefunction
#' @export
#'
#' @examples
calc_personresponsedata <- function(model, post_responses, id = NULL) {
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
