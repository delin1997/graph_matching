source("ind2sub.R")
#' Post-processing by a greedy algorithm.
#'
#' \code{gmPosDGre} post-processes the continuous assignment to obtain a discrete 
#' one by a greedy algorithm.
#' 
#' @references M. Leordeanu and M. Hebert, "A Spectral Technique
#'  for Correspondence Problems Using Pairwise Constraints", in ICCV, 2005
#'
#' @param X0 continuous correspondence, n1*n2 matrix.
#' @return \code{X}: discrete correspondence, n1*n2 matrix.
gmPosDGre <- function(X0, Ct){
  # Post-processing the continuous assignment to obtain a discrete 
  # one by a greedy algorithm.
  #
  # Reference
  #   M. Leordeanu and M. Hebert, "A Spectral Technique
  #   for Correspondence Problems Using Pairwise Constraints", in ICCV, 2005
  #
  # Input
  #   X0      -  continuous correspondence, n1 x n2
  #
  # Output
  #   X       -  discrete correspondence, n1 x n2
  #
  # History
  #   create  -  Feng Zhou (zhfe99@gmail.com), 01-25-2009
  #   modify  -  Feng Zhou (zhfe99@gmail.com), 10-22-2011
  
  # dimension
  n1 <- nrow(X0)
  n2 <- ncol(X0)
  X0[Ct == 0] <- -1
  
  # greedy discretization
  X <- matrix(0, n1, n2)
  idx <- which.max(X0)
  val <- X0[idx]
  n <- 0
  while(val > 0){
    X[idx] <- 1
    n <- n + 1
    
    # index
    i_j <- ind2sub(n1, idx)
    
    # remove
    X0[, i_j[2]] <- 0
    X0[i_j[1], ] <- 0
    
    # next position
    idx <- which.max(X0)
    val <- X0[idx]
  }

  return(X)
}
