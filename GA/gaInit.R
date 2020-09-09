#'a function for creating the initial assignment matrix 
#'@param n1 the number of points in the origin graph
#'@param n2 the number of points in the graph we want to match to the origin graph

gaInit <- function(n1,n2){
  size <- n1 * n2
  M <- matrix(rep(1,size), n1, n2) 
  eps <- matrix(runif(size,0,1), n1, n2)
  M <- M + eps
  MInit <- bioNormalization(M)
  
  return(MInit)
}