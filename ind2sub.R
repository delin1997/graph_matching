#' Convert vector-index to matrix-index.
#'
#' \code{ind2sub} converts vector-index to matrix-index.
#'
#' @param row_n #rows of the matrix.
#' @param ind vector-index to be converted.
#' @return matrix-index, matrix of \code{length(ind)}*2,
#'  1st \code{i} for row number, 2nd \code{j} for column number.
#' @examples
#' ind2sub(3, c(6, 8))
ind2sub <- function(row_n, ind){
  i <- ((ind-1) %% row_n) + 1
  j <- ((ind-1) %/% row_n) + 1
  return(cbind(i, j))
}
