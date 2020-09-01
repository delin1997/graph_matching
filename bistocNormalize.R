#' Convert original edge affinity matrix to bistochastic.
#' 
#' \code{bistocNormalize} converts original edge affinity matrix to bistochastic
#'   via Sinkhorn Normalization.
#' 
#' @param KQ0 original edge affinity, m1*m2 matrix.
#' @param nItMa #maximum iteration.
#' @param th: threshold.
#' @return \code{KQ1}: normalized edge affinity, m1*m2 matrix.
bistocNormalize <- function(KQ0, nItMa, th){
  nIt <- 0
  repeat{
    KQ1 <- KQ0/rowSums(KQ0)
    KQ1 <- t(t(KQ0)/colSums(KQ0))
    nIt <- nIt + 1
    delta <- sum((KQ1-KQ0)^2)
    if(nIt >= nItMa || delta <= th){break}
    KQ0 <- KQ1
  }
  return(KQ1)
}