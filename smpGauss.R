#' Sampling from a Gaussian distribution.
#'
#' \code{smpGauss} samples from a Gaussian distribution.
#'
#' @param me mean, length d vector.
#' @param Var variance, d*d matrix.
#' @param n #samples.
#' @param maMiD flag of maximizing the minimum of pairwise distance, 'y' or 'n'(default).
#' @param nRep  #repetitions (only used if maMiD = 'y'), 100 as default.
#' @return \code{X}:  sample matrix of d*n.
smpGauss <- function(me, Var, n, maMiD = "n", nRep = 100){
  # function option
  isMaMiD <- maMiD =="y"
  
  # dimension
  d = length(me);
  
  # repeat sampling until satisfying the constraint
  if(!isMaMiD){
    nRep <- 1
  }
  
  
  Xs <- lapply(as.list(1:nRep), function(i){
    X0 <- matrix(runif(d*n), d, n)
    
    # transformation
    V_D <- eigen(Var)
    D2 <- sqrt(V_D$values)
    
    V_D$vectors %*% diag(D2) %*% X0 + matrix(rep(me, n), ncol = n)
    
  })
  
  if(isMaMiD){
    X <- pickMaMiD(Xs)
  }else{
    X <- Xs[[1]]
  }
  
  return(X)
}
