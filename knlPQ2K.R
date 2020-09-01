library(Matrix)
source("sub2ind.R")
knlPQ2K <- function(KP, KQ, Eg1, Eg2, n1, n2, m1, m2, nn){
  # Convert the node and edge affinity matrix to the global sparse affinity matrix.
  #
  # Input
  #   KP      -  node-node affinity, n1 x n2
  #   KQ      -  edge-edge affinity, m1 x m2
  #   gphs    -  graphs, 1 x 2 (cell)
  #
  # Output
  #   K       -  global affinity, nn x nn (sparse)
  #
  # History
  #   create  -  Feng Zhou (zhfe99@gmail.com), 08-09-2011
  #   modify  -  Feng Zhou (zhfe99@gmail.com), 05-06-2013
  
  # edge affinity (off-diagonal of K)
  I11 <- rep(Eg1[1, ], m2)
  I12 <- rep(Eg1[2, ], m2)
  I21 <- c(matrix(rep(Eg2[1, ], m1), nrow = m1, byrow = T))
  I22 <- c(matrix(rep(Eg2[2, ], m1), nrow = m1, byrow = T))
  idx1 <- sub2ind(n1, I11, I21)
  idx2 <- sub2ind(n1, I12, I22)
  vals <- c(KQ)
  
  
  # node affinity (diagonal of K)
  idx1 <- c(idx1, 1:nn)
  idx2 <- c(idx2, 1:nn)
  vals <- c(vals, c(KP))
  
  # create the sparse matrix
  K <- sparseMatrix(i = idx1, j = idx2, x = vals, dims = c(nn, nn))
  
  return(K)
}
