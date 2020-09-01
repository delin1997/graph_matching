#' Convert matrix-index to vector-index.
#'
#' \code{sub2ind} converts matrix-index to vector-index.
#'
#' @param row_n #rows of the matrix.
#' @param ind_i row indices vector.
#' @param ind_j column indices vector(same length as \code{ind_i}).
#' @return vector-index, vector of length \code{length(ind_i)}.
#' @examples
#' sub2ind(3, 1:2, 3:4)
sub2ind <- function(row_n, ind_i, ind_j){
  (ind_j-1)*row_n + ind_i
}
