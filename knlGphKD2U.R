source("knlPQ2K.R")
#' Compute the global affinity matrix for undirected graph matching.
#'
#' \code{knlGphKD2U} returns the global affinity matrix for undirected graph matching.
#'
#' nn = n1*n2.
#'
#' @param KP node-node affinity, matrix of n1*n2.
#' @param KQ edge-edge affinity, matrix of m1*m2.
#' @param gphs graphs, list of length 2.
#' @return K: global affinity, nn*nn sparse matrix(class dgCMatrix).
knlGphKD2U <- function(KP, KQ, gphUs){
  # dimension
  KP <- as.matrix(KP)
  KQ <- as.matrix(KQ)
  n1 <- nrow(KP);n2 <- ncol(KP)
  m1 <- nrow(KQ);m2 <- ncol(KQ)
  nn <- n1 * n2;
  
  m1a = round(m1 / 2);
  m2a = round(m2 / 2);
  
  # KQU = KQ(1 : m1a, 1 : m2a);
  # KQU2 = [KQU, KQU; KQU, KQU];
  
  KQU <- (KQ[1 : m1a, 1 : m2a] + KQ[1 : m1a, (m2a + 1) : m2]) / 2
  KQU2 <- cbind(KQU, KQU)
  KQU4 <- rbind(KQU2, KQU2)
  
  # edge
  Eg1 <- gphUs[[1]]$Eg
  Eg2 <- gphUs[[2]]$Eg
  
  # global kernel for asymmetric edges
  KU <- knlPQ2K(KP, KQU4, Eg1, Eg2, n1, n2, m1, m2, nn);
  
  return(list(KU = KU, KQU = KQU))
}