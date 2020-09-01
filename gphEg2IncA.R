#' Obtain incidence matrix for asymmetric edges.
#'
#' \code{gphEg2IncA} obtains incidence matrix for asymmetric edges.
#' 
#' @param Eg graph edge, 2*(2m) matrix.
#' @param n #nodes.
#' @return List of length 4,
#'  G: node-edge incidence (starting), n*(2m) matrix;
#'  H: node-edge incidence (ending), n*(2m) matrix;
#'  U: incidence component, n*m matrix;
#'  V: incidence component, n*m matrix.
gphEg2IncA <- function(Eg, n){
  # dimension
  m <- round(ncol(Eg) / 2)
  
  # incidence matrix
  U <- matrix(0, n, m)
  V <- matrix(0, n, m)
  
  invisible(apply(matrix(1:m), 1, function(c){
    U[Eg[1, c], c] <<- 1
    V[Eg[2, c], c] <<- 1
  }))
  
  # incidence
  G <- cbind(U, V)
  H <- cbind(V, U)
  
  return(list(G = G, H = H, U = U, V = V))
}
