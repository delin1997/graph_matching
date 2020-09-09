#' a function for tranform a matrix into doubly stochastic matrix
#' using Sinkhorn method, by normalizing the rows and columns through the
#' iterations
#' @param X the target matrix, the elements should be non-negative
#' @param eps the error we tolerate for the iteration
#' @param Maxit the maximum of the number of iteration 
bioNormalization <- function(X, eps = 1e-3, Maxit = 200){
  n1 <- nrow(X)
  n2 <- ncol(X)
  if (n1 > n2){
    slacks <- matrix(rep(1,(n1-n2) * n2), n1, n1-n2)
    X <- cbind(X,slacks)
  }
  for(it in 1:Maxit){
    rsum <- rowSums(X)
    Xrow <- X/rsum
    csum <- colSums(Xrow)
    Xnew <- t(t(Xrow)/csum)
    if(sum(abs(Xnew - X)) < eps){
      break
    }else{
      X <- Xnew
    }
  }
  Xnew <- Xnew[1:n1, 1:n2]
  
  return(Xnew)
}