#' Sampling from a uniform distribution.
#'
#' \code{smpUnif} samples from a uniform distribution.
#'
#' @param mi minimum value of each dimension, vector of length d.
#' @param ma maximum value of each dimension, vector of length d.
#' @param n #samples.
#' @param maMiD flag of maximizing the minimum of pairwise distance, 'y' or 'n'(default).
#' @param nRep  #repetitions (only used if maMiD = 'y'), 100 as default.
#' @return \code{X}:  sample matrix of d*n.
smpUnif <- function(mi, ma, n, maMiD = "y", nRep = 100){
  # dimension
  d <- length(mi)
  
  isMaMiD <- maMiD =="y"
  
  # repeat sampling until satisfying the constraint
  if(!isMaMiD){
    nRep <- 1
  }
  
  Xs <- lapply(as.list(1:nRep), function(i){
    matrix(runif(d*n)* rep(ma-mi, n) + rep(mi, n), d, n)
  })
  
  if(isMaMiD){
    source("pickMaMiD.R")
    X <- pickMaMiD(Xs)
  }else{
    X = Xs[[1]]
  }
  
  return(X)
}
