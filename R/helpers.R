#' Function to convert a list of dataframes to a 3D array
#' All objects in the list will be dataframes with identical column headings.
#' From Hannah Buckley (https://www.researchgate.net/publication/277670582_R_function_to_convert_a_list_of_dataframes_into_a_3-D_array)
#'
#' @param input.list List of dataframes
#'
#' @return three dimensional array (rows) x (cols) x (dataframenumber in list)
#' @export
#'
#' @examples
#' dat.ls <- vector("list",10) # empty list
#' for(i in 1:10) {
#' dat.ls[[i]] <- data.frame(xx=rnorm(25,3,2),yy=rnorm(25,2,1),zz=rnorm(25,2,1))
#' }  # fill list
#' dat.ary <- list2array(dat.ls)  # convert to ary
list2array = function(input.list){  #input a list of dataframes
  rows.cols <- dim(input.list[[1]])
  sheets <- length(input.list)

  output.array <- array(unlist(input.list), dim = c(rows.cols, sheets))

  colnames(output.array) <- colnames(input.list[[1]])
  row.names(output.array) <- row.names(input.list[[1]])

  return(output.array)    # output as a 3-D array
}
