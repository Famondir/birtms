# Sets the expressions used to build the formula as global variables to inform R
# CMD check that they are intended to have no definition at time of package
# building
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "item.id", "r_item", "b_Intercept", "b_skillintercept_Intercept",
                                                        'person.id', 'response', 'group_id', 'score', 'item_score',
                                                        'sd_y', 'n', 'sd_x', 'mean_y', 'mean_x', 'person_sd', 'person_mean',
                                                        'se', 'group_sd_x', 'group_mean_x', 'group_mean_y', 'group_sd_y',
                                                        'item_nr', 'xerr', 'yerr', 'frame', 'r_item__beta', 'se_x', 'testlet',
                                                        'unit'
))

#' Plots a wrightmap
#'
#' @param model birtmsfit object
#' @param items integer vector; which items should be mapped?
#' @param palette color palette
#' @param lims double vector of length 2
#' @param namefun function to shorten names
#' @param groupfun function to group items based on names
#' @param labsize double
#' @param bins integer
#' @param classic boolean
#'
#' @return ggplot object
#' @export
#'
#' @examples
plot_wrightmap <- function(model, items = c(1:5), palette = NULL, lims = NULL,
                           namefun = identity, groupfun = identity,
                           labsize = 4, bins = 20, classic = TRUE) {
  stopifnot(model$model_specs$response_type == 'dichotom')
  stopifnot(model$model_specs$add_common_dimension == FALSE)
  stopifnot(model$model_specs$dimensinality_type == 'unidimensional')

  data_long <- model$data
  theta <- get.theta_for_wrightmap(model)
  delta <- get.delta_for_wrigthmap(model)

  map <- ggWrightMap.custom(theta = theta, b = delta[["delta"]], bins = bins, size = 15,
                            color = "skyblue", rel.width = c(1,3),
                            group = groupfun(delta[["rowname"]]),
                            item.names = namefun(delta[["rowname"]]),
                            palette, lims, labsize = labsize,
                            classic = classic)

  return(map)
}

get.theta_for_wrightmap <- function(model) {
  theta <- model %>% get.table_person_values()

  return(theta[["theta"]])
}

get.delta_for_wrigthmap <- function(model) {
  item <- model$var_specs$item
  if (model$model_specs$item_parameter_number > 1) {
    betaname <- "beta_Intercept"
    interceptname <- "skillintercept_Intercept"
  } else {
    betaname <- "Intercept"
    interceptname <- "Intercept"
  }

  betas <- brms::ranef(model)[[item]][,"Estimate",betaname] %>% as.data.frame() %>%
    tibble::rownames_to_column() %>% rename(beta = ".")
  intercept <- brms::fixef(model)[interceptname,"Estimate"]
  deltas <- betas %>% mutate(delta = -(beta + intercept))

  return(deltas)
}

ggWrightMap.custom <- function(theta, b, bins = 10, color = "blue", size = 15, item.names = NULL,
                               rel.width = 1, group = NULL, palette = NULL, lims = NULL, labsize = 6,
                               classic = FALSE) # ursprünglich aus ShinyItemAnalysis
{
  if (missing(theta)) {
    stop("'theta' needs to be specified", call. = FALSE)
  }
  if (missing(b)) {
    stop("item locations need to be specified", call. = FALSE)
  }
  if (is.null(item.names)) {
    ITEM.NAMES <- 1:length(b)
  } else {
    ITEM.NAMES <- item.names
  }

  df.theta <- data.frame(theta = theta)

  binwidth <- -(min(c(theta, b))-max(c(theta, b)))/bins
  theta.cut.points <- seq(min(c(theta, b)) - binwidth / 2, max(c(theta, b)) + binwidth / 2, binwidth)
  b.cut.points <- cut(b, theta.cut.points, include.lowest = T)
  levels(b.cut.points) <- theta.cut.points[-length(theta.cut.points)] + diff(theta.cut.points) / 2
  b.cut.points <- as.numeric(paste(b.cut.points))

  df.b <- data.frame(item = as.character(ITEM.NAMES), b = b, y = b.cut.points)
  df.b$x <- 0
  for (i in unique(df.b$y)) {
    n <- nrow(df.b[df.b$y == i, ])
    df.b[df.b$y == i, "x"] <- 1:n
  }

  df.b$item <- as.character(df.b$item)
  maxn <- max(nchar(df.b$item))

  if (is.null(item.names)) {
    while (any(nchar(df.b$item) < maxn)) {
      df.b$item <- ifelse(nchar(df.b$item) < maxn, paste0("0", df.b$item), df.b$item)
    }
  } else {
    df.b$item <- as.character(df.b$item)
    while (any(nchar(df.b$item) < maxn)) {
      df.b$item <- ifelse(nchar(df.b$item) < maxn, paste0(" ", df.b$item), df.b$item)
    }
  }

  if (any(df.b$x > 1)) {
    for (k in which(df.b$x > 1)) {
      df.b[nrow(df.b) + 1, ] <- df.b[k, ]
      df.b[nrow(df.b), "item"] <- "|"
      df.b[nrow(df.b), "x"] <- df.b[nrow(df.b), "x"] - 0.5
    }
  }

  if (is.null(lims)) {
    lim.x.min <- min(c(theta, b)) - binwidth
    lim.x.max <- max(c(theta, b)) + binwidth
  } else {
    lim.x.min <- min(lims) - binwidth
    lim.x.max <- max(lims) + binwidth
  }


  if (is.null(group)) {
    group <- paste0("i", seq_along(ITEM.NAMES))
  }

  vec <- c(group, rep(NA, nrow(df.b)-length(ITEM.NAMES)))
  df.b <- df.b %>% mutate(testlet = vec)

  if (is.null(palette)) {
    colourCount = length(unique(vec))
    palette <- randomcoloR::distinctColorPalette(colourCount)
  }

  g1 <- ggplot2::ggplot(df.theta, ggplot2::aes_string(x = "theta")) +
    ggplot2::geom_density(fill = color, colour = color, na.rm = TRUE) +
    # xlim(lim.x.min, lim.x.max) +
    ggplot2::scale_x_continuous(limits = c(lim.x.min, lim.x.max), breaks = seq(-3,3,1)) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse() +
    ggplot2::xlab("Student ability") +
    theme.wm(base_size = size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    )

  g2 <- ggplot2::ggplot(df.b[1:length(group),], ggplot2::aes_string(x = 'x', y = 'y'))

  if (classic) {
    g2 <- g2 + ggplot2::geom_text(label = "X", hjust = 0.5, vjust = 0.5, na.rm = TRUE, size = labsize) +
      ggplot2::geom_text(data = df.b[length(group)+1:nrow(df.b),], label = "|", hjust = 0.5, vjust = 0.5, na.rm = TRUE, size = labsize)
  } else {
    g2 <- g2 + ggplot2::geom_label(aes(label = item, fill = testlet), hjust = 0.5, vjust = 0.5, na.rm = TRUE,
                          size = labsize, label.padding = unit(0.25, "lines"))
  }

  g2 <- g2 + ggplot2::scale_y_continuous(position = "right", breaks = seq(-3,3,1)) +
    # scale_y_continuous(position = "right", limits = c(lim.x.min, lim.x.max), breaks = seq(-3,3,1)) +
    # scale_x_continuous(limits = c(min(df.b$x) - 0.5, max(df.b$x) + 0.5)) +
    ggplot2::coord_cartesian(ylim = c(lim.x.min, lim.x.max), xlim = c(min(df.b$x) - 0.5, max(df.b$x) + 0.5)) +
    # geom_rect(aes(xmin = x-.6, xmax = x+.6, ymin = y-.06, ymax = y+0.06)) +
    # geom_rect_pattern(aes(xmin = x-.6, xmax = x+.6, ymin = y-.06, ymax = y+0.06), pattern = 'magick') +
    # geom_text(hjust = 0.5, vjust = 0.5, na.rm = TRUE, size = 5) +
    ggplot2::ylab("Item difficulty") +
    theme.wm(base_size = size) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    # guides (colour = 'legend') +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::guides(fill="none")

  g <- cowplot::plot_grid(g1, g2, rel_widths = rel.width)
  return(g)
}

theme.wm <- function (base_size = 15, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(legend.key = ggplot2::element_rect(fill = "white", colour = NA),
          legend.title = ggplot2::element_blank(), legend.position = "none",
          axis.line = ggplot2::element_line(colour = "black"), panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(face = "bold"))
}
