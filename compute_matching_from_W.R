library(Matrix)
library(RSpectra)
source("computeConstraintMatching.R")
source("computeEigenvectorsAffineConstraint.R")
#' Compute matching from W.
#'
#' \code{compute_matching_from_W} computes matching from W.
#'
#' @param W sparse n*n (compatibility) matrix.
#' @param E12 matching hypothesis (E12 is all-one matrix if full-matching).
#' @param constraintMode 'none', 'col', 'row' or 'both'.
#' @return List of 4 containing matching info,
#'   X: constrained eigenvectors of length n;
#'   lambda: eigenvalue;
#'   timing: computation time;
#'   constraintViolation: error in constraint violation. 
compute_matching_from_W <- function(W, E12, constraintMode){
  # E12 = matching hypothesis (E12 = ones(n1, n2) if full-matching)
  #
  # Input
  #   k = # eigenvectors
  #   constraintMode = 'none' | 'col' | 'row' | 'both'
  #   isAffine = 0 | 1
  
  
  #W <- tril(sparseMatrix(W))
  W <- as(W, "sparseMatrix")
  
  constraintViolation <- NULL
  n1 <- nrow(E12)
  n2 <- ncol(E12)
  n12 <- ncol(W)
  
  time_eigensolverTotal <- Sys.time()
  if(constraintMode=='none'){
    X0_lambda <- eigs(W, 1)
    timing <- Sys.time()
    X0 <- X0_lambda$vectors
    lambda <- X0_lambda$values
  }else{
    C_b <- computeConstraintMatching(E12, constraintMode)
    C <- C_b$C
    b <- C_b$b
  }
  
  if(constraintMode=='both'){
    n <- nrow(C)
    C <- C[1 : (n - 1), ] # otherwise overconstrained
    b <- b[1 : (n - 1)]
  }

  Eig_AC <- computeEigenvectorsAffineConstraint(W, C, b)
  X0 <- Eig_AC$X
  lambda <- Eig_AC$lambda
  timing <- Eig_AC$timing
  constraintViolation <- Eig_AC$constraintViolation
  
  
  # time
  time_eigensolverTotal <- Sys.time() - time_eigensolverTotal
  timing <- list(timing = timing, eigensolverTotal = time_eigensolverTotal)
  
  X <- matrix(0, n1*n2)
  X[E12 > 0, ] <- X0
  X <- matrix(X, nrow = n1)
  
  return(list(X = X, lambda = lambda, timing = timing, 
              constraintViolation = constraintViolation))
}
