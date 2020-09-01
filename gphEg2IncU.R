#' Obtain incidence matrix for undirected graph.
#'
#' \code{gphEg2IncA} obtains incidence matrix for undirected graph.
#' 
#' @param Eg graph edge, 2*(2m) matrix.
#' @param n #nodes.
#' @return List of length 2,
#'  G: node-edge adjacency, n*m matrix;
#'  H: augumented node-edge adjacency, n*(m + n) matrix.
gphEg2IncU <- function(Eg, n){
  # dimension
  Eg <- as.matrix(Eg)
  m <- round(ncol(Eg) / 2)
  
  # incidence matrix
  G <- matrix(0, n, m)
  invisible(apply(matrix(1:m), 1, function(c){
    G[Eg[, c], c] <<- 1
  }))
  
  # augumented adjacency
  H <- cbind(G, diag(1, n))
  
  return(list(G = G, H = H))
}
