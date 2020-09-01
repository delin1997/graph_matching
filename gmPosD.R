#' Compute a discrete assingment matrix by rounding from a continuous one.
#'
#' \code{gmPosD} returns the discrete assingment matrix by rounding from a continuous one.
#' 
#' @param K affinity matrix, \code{NULL} or nn*nn sparse matrix(class dgCMatrix).
#' @param Ct constraint, n1*n2 matrix.
#' @param X0 continuous correspondence, n1*n2 matrix.
#' @param par parameter,
#'  alg   -  method, 'gre', 'hun' or 'ipfp',
#'   'gre'  : greedy algorithm;
#'   'hun'  : hungraian algorithm;
#'   'ipfp' : integer fixed point algorithm(not available for now).
#' @return \code{X}: discrete correspondence, n1*n2 matrix.
gmPosD <- function(K, Ct, X0, par){
  # function parameter
  alg <- par$alg
  
  if(alg=='gre'){
    source("gmPosDGre.R")
    X <- gmPosDGre(X0, Ct)
  }else if(alg=='hun'){
    source("gmPosDHun.R")
    X <- gmPosDHun(X0, Ct)
  }else if(alg=='ipfp'){
    X <- gmPosDIpfp(K, Ct, X0, par)
  }else{
    stop('unknown method: ', alg)
  }
  
  return(X)
}
