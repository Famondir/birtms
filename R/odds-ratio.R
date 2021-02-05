#' Odds ratio
#' Calculates the odds ratio for the posterior samples or the original responses
#' adjusted from Anna Scharl and Timo Gnambs (https://www.tqmp.org/RegularArticles/vol15-2/p075/p075.pdf)
#'
#' @param y_rep (pers) x (item) x (rep) array; replicated data (can handle response data as well)
#' @param y (pers) x (item) dataframe; response data
#'
#' @return
#' @export
#'
#' @examples
calculate_odds_ratio <- function(y_rep = NULL, y = NULL) {
  if(is.null(y_rep) & is.null(y)) stop('Missing data argument! Use either y_rep or y.')
  if(!is.null(y_rep) & !is.null(y)) stop('Too many data arguments! Use either y_rep or y.')

  if(!is.null(y)) y_rep <- y # function only uses y_rep
  if(is.na(dim(y_rep)[3])) {
    y_rep <- list2array(list(y_rep)) # make pseudo array of dataframes from dataframe
    sum_fct <- function(x) sum(x)
  } else {
    sum_fct <- function(x) colSums(x)
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
  return(or)
}
