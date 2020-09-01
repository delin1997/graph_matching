source("gmIni.R")
source("gmPosC.R")
source("gmPosD.R")
source("matchAsg.R")
#' Graph matching.
#'
#' \code{gm} can be used as the interface of the following algorithms:
#' Graudate Assignment (GA)
#' Spectral Matching (SM)
#' Spectral Matching with Affine Constraint (SMAC)
#' Integer Projected Fixed Point (IPFP)
#' Reweighted Random Walks Matching (RRWM)
#' (only SM and SMAC are available now)
#'
#'
#' This code is to solve the following problem:
#'     max_X   t(vec(X)) %*% K %*% vec(X)
#'     s.t.    X is a permutation matrix
#' nn = n1*n2
#' 
#' @param K affinity matrix, nn*nn sparse matrix(class dgCMatrix).
#' @param Ct correspondence constraint, matrix of n1*n2,
#'  Ct_ij = 1: i and j can be matched;
#'  Ct_ij = 0: i and j cannot be matched.
#' @param asgT ground-truth assignment (can be \code{NULL})
#' @param pars parameter for graph matching,
#'  parIni: parameter for initialization;
#'  parPosC: parameter for continuous-continuous post-processing;
#'  parPosD: parameter for continuous-discrete post-processing.
#' @return Assignment \code{asg}, list of length 5,
#'  alg: algorithm name;
#'  X: binary correspondence matrix of n1*n2;
#'  acc: accuracy (= 0 if asgT is \code{NULL});
#'  obj: objective value;
#'  tim: time cost.
gm <- function(K, Ct, asgT, pars){
  # function parameter
  parIni <- pars$parIni
  parPosC <- pars$parPosC
  parPosD <- pars$parPosD
  
  cat('-gm: ', 'ini ', parIni$alg, ', posC ', 
      parPosC$alg, ', posD ', parPosD$alg, "\n")
  t <- Sys.time()
  
  # initialization
  X0 <- gmIni(K, Ct, parIni)
  
  # continous -> continous
  XC <- gmPosC(K, Ct, X0, parPosC)
  
  # continous -> discrete
  X <- gmPosD(K, Ct, XC, parPosD)
  
  # compare with ground-truth
  acc <- matchAsg(X, asgT)
  
  # store
  asg <- list()
  asg$alg <- sprintf('gm+%s+%s+%s', parIni$alg, parPosC$alg, parPosD$alg)
  asg$X <- X
  asg$acc <- acc
  asg$obj <- as.vector(t(c(X)) %*% K %*% c(X))
  asg$tim <- Sys.time()-t

  return(asg)
}
