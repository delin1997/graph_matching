#' Obtain segment information from the segment length.
#'
#' \code{n2s} returns segment information from the segment length.
#'
#' @param ns segment length, vector of length m.
#' @return List of length 2,
#'  s: starting position of each segment, vector of length m+1;
#'  H: frame-segment indicator matrix, matrix of m*n.
n2s <- function(ns){
  s <- cumsum(c(1, ns))
  m <- length(ns)
  n <- sum(ns)
  
  zeros <- rep(0, n)
  H <- t(apply(matrix(1:m), 1, function(i){
    zeros[s[i]:(s[i+1]-1)] <- 1
    return(zeros)
  }))
  
  return(list(s = s, H = H))
}
