source("compute_full_rank_constraint.R")
#' Compute eigenvectors under affine constraint. 
#'
#' \code{computeEigenvectorsAffineConstraint} computes eigenvectors under affine constraint. 
#'
#'  solves
#'   max    X ^ T W X / X ^ T X
#'   subject to  C X = b
#'               X^T X = 1
#'   C: k*n matrix
#'   Ceq = C[1:k-1, ] - (1/b[k]) * b[1:k-1]%*%C[k, ,drop = FALSE];
#'   K = In - t(Ceq)%*%solve(Ceq%*%t(Ceq))%*%Ceq;
#'   t(Ceq) %*% solve(Ceq %*% t(Ceq)) %*% Ceq = 
#'    t(B) %*% Ainv %*% B + L1 %*% t(L2)
#'
#' @param W sparse n*n (compatibility) matrix.
#' @param C constraint matrix of k*n.
#' @param b constraint vector of length k(b cannot be all 0).
#' @return List of 4 containing matching info,
#'   X: constrained eigenvectors of length n;
#'   lambda: eigenvalue;
#'   timing: computation time;
#'   constraintViolation: error in constraint violation.
computeEigenvectorsAffineConstraint <- function(W, C, b){
  C_b <- compute_full_rank_constraint(C, b)
  C <- C_b$C
  b <- C_b$b
  
  
  W <- as(W, "sparseMatrix")
  C <- as(C, "sparseMatrix")
  
  
  # make sure b(end) non zero
  ind <- which.max(abs(b))
  stopifnot(b[ind]!=0)
  l <- length(b)
  b[c(l, ind)] <- b[c(ind, l)]
  C[c(l, ind), ] <- C[c(ind, l), ]
  
  k <- nrow(C)
  n <- ncol(C)
  B <- C[1 : (k - 1), ]
  u <- -b[1 : k - 1] / b[k]
  v <- C[k, ]
              
  Ainv <- solve(B %*% t(B))
  T1 <- Ainv %*% cbind(u, B %*% v + c(t(v) %*% v) * u)
  T2 <- T1 %*% solve(diag(2) + t(cbind(B %*% v, u)) %*% T1)
  T3 <- -(t(B) %*% T2 + v %*% (t(u) %*% T2))         

  L1 <- cbind(t(B) %*% (Ainv %*% u), t(B) %*% (Ainv %*% (B %*% v)) - v)
  L2 <- cbind(v + T3[, 2], T3[, 1])
  
  n <- nrow(W)
  t <- Sys.time()
  K <- diag(n)-(t(B)%*%Ainv%*%B+L1%*%t(L2))
  eigs_mod <- eigs(K%*%W%*%K, 1)
  timing <- Sys.time()-t
  lambda <- eigs_mod$values
  X <- eigs_mod$vectors
  

  
  # scaling X to satisfy affine constraint
  X <- Re(X)
  temp <- as.matrix(t(b) %*% C %*% X / c(t(b) %*% b))
  n <- nrow(X)
  X <- X / c(temp)
  temp <- C %*% X - b
  constraintViolation <- sqrt(sum(temp ^ 2))
  
  return(list(X = X, lambda = lambda, timing = timing, 
              constraintViolation = constraintViolation))
}
