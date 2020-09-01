source("sub2ind.R")
#' Obtain the adjacency matrix from edge index matrix.
#'
#' \code{gphEg2Adj} obtains the adjacency matrix from edge index matrix.
#'
#' @param Eg graph edge, 2*2m matrix.
#' @param n #nodes.
#' @return List of 2,
#'  A: node-node adjacency, n*n matrix;
#'  idx: index of each edge, length 2m vector.
gphEg2Adj <- function(Eg, n){
  # dimension
  Eg <- as.matrix(Eg)
  m <- round(ncol(Eg) / 2)
  
  idx <- sub2ind(n, Eg[1, ], Eg[2, ])
  A <- matrix(0, n, n)
  A[idx] <- 1
  
  return(list(A = A, idx = idx))
}
