source("compute_matching_from_W.R")
source("computeXorthonormal.R")
#' Compute the assingment matrix by the algorithm of spectral matching with constraint.
#'
#' \code{gmIniSmac} returns the global affinity matrix for graph matching.
#'
#' This algorithm is to obtain the optimal x for the following problem
#'     max_x   x' %*% K %*% x
#'     s.t.    A %*% x <= 1
#' nn = n1*n2
#' @references Timothee Cour, Praveen Srinivasan, Jianbo Shi, 
#'  "Balanced Graph Matching", in NIPS, 2008
#'
#' @param K nn*nn sparse affinity matrix(class dgCMatrix). 
#' @param Ct constraint, matrix of n1*n2.
#'   Ct_ij = 1: i and j can be matched
#'   Ct_ij = 0: i and j cannot be matched
#' @param ns #nodes, vector of length 2.
#' @param par parameter
#' @return \code{X}: permutation matrix of n1*n2.
gmIniSmac <- function(K, Ct, par){
  # dimension
  n1 <- nrow(Ct)
  n2 <- ncol(Ct)
  
  ns <- c(n1, n2)
  nn <- ns[1] * ns[2]
  
  # initial
  init <- compute_matching_from_W(K, Ct, 'both')
  X0 <- init$X
  
  # orthonormalize X
  X <- computeXorthonormal(X0, Ct)
  
  return(X)
}
