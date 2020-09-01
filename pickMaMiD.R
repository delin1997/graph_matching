#' Pick the sample set whose minmum pairwise sample distance is maxmimum.
#'
#' \code{pickMaMiD} picks the sample set whose minimum pairwise sample distance is maximum.
#'
#' @param Xs sample set, list of length m, d*n matrix.
#' @return List of length 3,
#'  X: sample set, d*n matrix;
#'  i: index of the optimum set;
#'  ds: all distances, vector of length m.
pickMaMiD <- function(Xs){
  # Pick the sample set whose minmum pairwise sample distance is maxmimum.
  #
  # Input
  #   Xs      -  sample set, 1 x m (cell), d x n
  #
  # Output
  #   X       -  sample set, d x n
  #   i       -  index of the optimum set
  #   ds      -  all distances, 1 x m
  #
  # History
  #   create  -  Feng Zhou (zhfe99@gmail.com), 07-17-2009
  #   modify  -  Feng Zhou (zhfe99@gmail.com), 10-09-2011
  
  # dimension
  m <- length(Xs)
  n <- ncol(Xs[[1]])
  
  # distance
  ds <- apply(matrix(1:m), 1, function(i){
    X <- Xs[[i]]
    
    # pairwise sample distance
    D <- conDst(t(X), t(X))
    
    # the minimum distance
    D <- D + diag(rep(1, n) * Inf)
    min(min(D))
  })

  
  # the set with maximum distance
  i <- which.max(ds)
  X <- Xs[[i]]
  
  return(list(X = X, i = i, ds = ds))
}
