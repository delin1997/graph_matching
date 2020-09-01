#' Compute a better continuous assingment matrix by refining an old one.
#'
#' \code{gmPosC} returns a better continuous assingment matrix by refining an old one.
#' 
#' nn = n1*n2.
#'
#' @param K affinity matrix, \code{NULL} or nn*nn sparse matrix(class dgCMatrix).
#' @param Ct constraint, n1*n2 matrix.
#' @param X0 initial assignment, n1*n2 matrix.
#' @param par parameter,
#'  alg: method, 'none', 'grad' or 'rrwm';
#'   'none' : do nothing;
#'   'grad' : graduate assignment;
#'   'rrwm' : reweighted random walk matching;
#'   'quad' : naive quadratic programming.
#'  only 'none' method is available now.
#' @return \code{X}: correspondence matrix of n1*n2.
gmPosC <- function(K, Ct, X0, par){
  # function parameter
  alg <- par$alg
  
  # skip
  if(alg=='none'){
    X <- X0
  }else if(alg=='grad'){
    # graduate assignment
    X <- gmPosCGrad(K, X0, par)
  }else if(alg=='rrwm'){
    # rrwm
    X <- gmPosCRrwm(K, X0, par)
  }else if(alg=='quad'){
    # quad
    X <- gmPosCQuad(-K, X0, par)
  }else{
    stop('unknown method: ', alg)
  }

  return(X)
}
