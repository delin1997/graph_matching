library(RSpectra)
#' Compute the assingment matrix by the algorithm of spectral matching.
#'
#' \code{gmIniSm} returns the assingment matrix by the algorithm of spectral matching.
#'
#' This algorithm is to obtain the optimal x for the following problem
#'     max_x   t(x) %*% K %*% x
#'     s.t.    t(x) %*% x = 1
#' nn = n1 x n2.
#' 
#' @references M. Leordeanu and M. Hebert, "A Spectral Technique
#'  for Correspondence Problems Using Pairwise Constraints", in ICCV, 2005.
#'
#' @param K nn*nn sparse affinity matrix(class dgCMatrix). 
#' @param Ct constraint, matrix of n1*n2.
#' @param par parameter,
#'  top: method of computing the top eigenvector, 'eigs'(default) or 'pow';
#'   'eigs' : the eigs function by RSpectra;
#'   'pow'  : power iteration.
#'  nIt: iteration number(30 as default), 
#'   only used if top == 'pow'.
#' @return \code{X}: permutation matrix of n1*n2.
gmIniSm <- function(K, Ct, par){
  # function parameter
  top <- ifelse(is.null(par$top), 'eigs', par$top)
  nIt <- ifelse(is.null(par$nIt), 30, par$nIt)
  
  # dimension
  Ct <- as.matrix(Ct)
  n1 <- nrow(Ct)
  n2 <- ncol(Ct)
  ns <- c(n1, n2)
  nn <- ns[1]*ns[2]
  
  # RSpectra package
  if(top=='eigs'){
    x <- eigs(K, 1)$vectors
    #x <- eigen(K)$vectors[, 1]
  }else if(top=='pow'){
    # power iteration
    x <- rep(1, nn)
    x <- x/norm(x, "2")
    invisible(apply(matrix(1:nIt), 1, function(iIt){
      x <- K%*%x
      x <<- x/norm(x, "2")
    }))
    x <- as.matrix(x)
  }else{
    stop('unknown method: ', top)
  }
  
  # make sure the eigenvector is positive
  # [~, idx] = max(abs(x));
  # if x(idx) <= 0
  #     x = -x;
  # end
  x <- abs(x)
  
  # vector -> matrix
  X <- matrix(x, nrow = ns[1], ncol = ns[2])
  
  return(X)
}
