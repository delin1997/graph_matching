library(RcppHungarian)
source("sub2ind.R")
#' Post-processing by the Hungrian algorithm.
#'
#' \code{gmPosDGre} post-processes the continuous assignment to obtain a discrete 
#' one by the Hungrian algorithm.
#' 
#' @param X0 continuous correspondence, n1*n2 matrix.
#' @param Ct constraint matrix, n1*n2 or \code{NULL},
#'  Ct_ij = 1: i and j can be matched;
#'  Ct_ij = 0: i and j cannot be matched.
#' @param opt optimization operator, 'min' or 'max'(default).
#' @return \code{X}: discrete correspondence, n1*n2 matrix.
gmPosDHun <- function(X0, Ct, opt = "max"){
  if(opt=='max'){
    X0 <- max(X0) - X0
  }else if(opt=='min'){
    # do noting
  }else{
    stop('unknown operator: ', opt)
  }
  
  if(exists('Ct') && !is.null(Ct)){
    X0[Ct == 0] <- Inf
  }
  
  # RcppHungarian package
  ind <- HungarianSolver(X0)$pairs
  
  # dimension
  n1 <- nrow(X0)
  n2 <- ncol(X0)
  
  # index -> matrix
  if(n1 <= n2){
    idx <- sub2ind(n1, ind[, 1], ind[, 2])
  }else{
    ind <- ind[ind[, 2]!=0, ]
    idx <- sub2ind(n1, ind[, 1], ind[, 2])
  }
  
  X <- matrix(0, n1, n2)
  X[idx] <- 1

  #X = hungarian(X0);
  #equal('X', X, X2);

  return(X)
}
