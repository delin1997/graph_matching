#' Compute the best orthogonal approximation of X.
#'
#' \code{computeXorthonormal} computes the best orthogonal approximation of X.
#' 
#' @param X correspondence matrix.
#' @param E12 matching hypothesis (E12 is all-one matrix if full-matching).
#' @return \code{Xorth}: the best orthogonal approximation of \code{X}.
computeXorthonormal <- function(X, E12){
  n1 <- nrow(X)
  n2 <- ncol(X)
  svd_mod <- svd(X)
  U <- svd_mod$u
  V <- svd_mod$v
  Xorth <- U %*% t(V)
  Xorth[E12 == 0] <- 0

  return(Xorth)
}

