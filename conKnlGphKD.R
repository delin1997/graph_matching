source("knlPQ2K.R")
#' Compute the global affinity matrix for graph matching.
#'
#' \code{conKnlGphKD} returns the global affinity matrix for graph matching.
#'
#' The edge is directed and the edge feature is asymmetric, nn=n1*n2.
#'
#' @param KP node-node affinity, matrix of n1*n2.
#' @param KQ edge-edge affinity, matrix of m1*m2.
#' @param gphs graphs, list of length 2.
#' @return \code{K}: global affinity, nn*nn sparse matrix(class dgCMatrix).
conKnlGphKD <- function(KP, KQ, gphs){
  # dimension
  KP <- as.matrix(KP)
  KQ <- as.matrix(KQ)
  n1 <- nrow(KP);n2 <- ncol(KP)
  m1 <- nrow(KQ);m2 <- ncol(KQ)
  nn <- n1 * n2
  cat('-conKnlGphKD: ', 'nn ', nn, ', m1 ', m1, ', m2 ', m2, "\n")
  
  # edge
  Eg1 <- gphs[[1]]$Eg
  Eg2 <- gphs[[2]]$Eg
  
  # global kernel for directed edges
  K <- knlPQ2K(KP, KQ, Eg1, Eg2, n1, n2, m1, m2, nn)
  
  return(K)
}
