#' a function for graduated assignment
#' @param W the affinity matrix n1n2*n1n2
#' @param M the initial assignment matrix, n1*n2
#' @param b0 the initial beta
#' @param bMax the maximum of beta
#' @param bstep the step-length of the increament for beta
#' @param Maxit the maximum iteration for updating Q
#' @param eps the tolarent error for iteration
#' Q_ai = \sum W_aibj M_bj

gaPosC <- function(W, M, n1, n2, b0 = 0.5, bMax = 10, bstep = 1.075, Maxit = 10, eps = 1e-3){
  #for each update, we should update Q when M become a permutation matrix
  M_Q <- M
  for(it1 in 1:Maxit){
    vecM <- as.vector(t(M_Q))
    vecQ <- W %*% vecM
    Q <- matrix(vecQ, n1, n2, byrow = T)
    b <- b0
    while(b <= bMax){
      expM <- exp(b*Q)
      Mnew <- bioNormalization(expM)
      if(sum(abs(Mnew - M)) < eps){
        break
      }else{
        b <- b * bstep
        M <- Mnew
      }
    }
    if(sum(abs(Mnew - M_Q)) < eps){
      break
    }else{
      M_Q <- Mnew
    }
  }
  
  return(Mnew)
}