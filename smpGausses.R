source("smpGauss.R")
source("n2s.R")
#' Sampling from a set of Gaussian distribution.
#'
#' \code{smpGausses} samples from a set of Gaussian distribution.
#'
#' @param mes mean, list of k,length d vector.
#' @param Vars variance, list of k, d*d matrix.
#' @param ns #samples in classes, length k vector.
#' @return List of length 2, 
#'  X: sample matrix of d*n(= sum(ns));
#'  G: class indicator matrix of k*n.
smpGausses <- function(mes, Vars, ns){
  # dimension
  k <- length(mes)
  d <- length(mes[[1]])
  
  # class size
  if (length(ns) == 1){
    ns <- rep(0, k) + ns
  }
  
  # generate sample
  Xs <- lapply(as.list(1:k), function(i){
    smpGauss(mes[[i]], Vars[[i]], ns[i])
  })
  X <- matrix(unlist(Xs), nrow = d)
  
  # label
  G <- n2s(ns)
  
  return(list(X = X, G = G))
}
