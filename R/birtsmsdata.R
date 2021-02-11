new_birtmsdata <- function(x, attribute_list) {
  stopifnot(is.data.frame(x))
  stopifnot(is.list(attribute_list) & !is.null(attr(attribute_list, 'name')))

  x <- tibble::as_tibble(x)
  class(x) <- c('birtmsdata', class(x))
  attributes(x) <- c(attributes(x), attribute_list)

  invisible(x)
}

is.birtmsdata <- function(x) inherits(x, "birtmsdata")

#' Print birtms data
#' Prints data as a tibble but prints additional attributes as well
#'
#' @param x birtmsdata
#' @param ... see base::print()
#'
#' @export
#'
#' @examples
#' \dontrun{
#' or_data <- get_or(fit, n_samples = 500)
#' or_data
#' }
print.birtmsdata <- function(x, ...) {
  dots <- rlang::enquos(...)
  NextMethod(x, dots)

  attr_list <- attributes(x)
  attr_list['names'] <- NULL
  attr_list['row.names'] <- NULL
  attr_list['class'] <- NULL

  cat('\nClass:\n')
  print(class(x))
  cat('\nAdditional attributes:\n')
  print(attr_list)
  invisible(x)
}

#' Unnest for birtmsdata
#' Preserves additional attributes of birtmsdata objects.
#'
#' @param data data frame
#' @param ... see tidyr::unnest()
#'
#' @return birtmsdata
#' @export
#' @importFrom tidyr unnest
#'
#' @examples
#' \dontrun{
#' #' or_data <- get_or(fit, n_samples = 500)
#' or_data %>% unnest(or_dif_samples)
#' }
unnest.birtmsdata <- function(data, ...) {
  dots <- rlang::enquos(...)

  attr_old <- attributes(data)
  attr_old['names'] <- NULL
  attr_old['row.names'] <- NULL

  data <- NextMethod(data, dots)

  attributes(data) <- c(attributes(data), attr_old)

  return(data)

  # NextMethod(data, dots)
}
