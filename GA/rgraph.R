#'the function to generalize a random graph
#'@param N the number of the points
#'@param pc the pencent of connectivity, the prob of an edge exists
#'@param weight the weight on each edge
rgraph <- function(N, pc, weight = 1){
  vecA <- rbinom(N * (N-1)/2, 1, pc) * weight
  A <- matrix(0,N,N)
  A[upper.tri(A)] <- vecA
  A <- t(A)
  A[upper.tri(A)] <- vecA
  
  A <- Matrix(A,sparse = TRUE)
  
  return(A)
}