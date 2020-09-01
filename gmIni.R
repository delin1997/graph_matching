#' Initialize the assingment matrix.
#'
#' \code{gmIni} returns the initialized the assingment matrix.
#'
#' nn = n1*n2.
#'
#' @param K nn*nn sparse affinity matrix(class dgCMatrix). 
#' @param Ct correspondence constraint, matrix of n1*n2.
#' @param par parameter,
#'  alg: method, 'unif' | 'sm' | 'smac';
#'   'unif' : uniform value;
#'   'sm'   : spectral matching (top eigen-vector of K);
#'   'smac' : spectral matching with constraint.
#' @return \code{X}: permutation matrix of n1*n2.
gmIni <- function(K, Ct, par){
  # function parameter
  alg <- par$alg
  
  
  if(alg=='unif'){
    # uniform
    X <- gmIniUnif(Ct, par)
  }else if(alg=='sm'){
    source("gmIniSm.R")
    # spectral matching    
    X <- gmIniSm(K, Ct, par)
  }else if(alg=='smac'){
    source("gmIniSmac.R")
    # spectral matching with affine constraint
    X <- gmIniSmac(K, Ct, par)
  }else{
    stop('unknown initialization method: ', alg)
  }
  
  return(X)
}
