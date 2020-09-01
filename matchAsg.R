#' Find the match between two assignments and compute the accuracy.
#'
#' \code{matchAsg} finds the match between two assignments and compute the accuracy.
#'
#' @param X original assignment matrix, n1*n2 matrix.
#' @param asgT ground-truth assignment (can be \code{NULL}).
#' @return \code{acc}: accuracy of the matching.
matchAsg <- function(X, asgT){
  # ground-truth
  XT <- asgT$X
  if(is.null(XT)){
    acc <- 0
    return(acc)
  }
  
  # non-zero correspondence
  idx <- which(XT!=0)
  
  # #correct correspondences found
  co <- sum(XT[idx] == X[idx])
  
  # percentage
  acc <- co / length(idx)
  
  return(acc)
}
