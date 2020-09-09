#' a function for graph matching with GA algorithm
#' the graph should be a weighted graph only (for now)
#' @param n1 the number of points in the origin graph
#' @param n2 the number of points in the graph we want to match to the origin graph
#' @param W the affinity matrix, the size should be n1n2 * n1n2, and the arrangement should be like (1,1) (1,2)...
#' (1,n2);(2,1)...
#' @param originM the ground-truth mathing

ga <- function(W, n1, n2, originM){
  #initialize the assignment matrix
  M0 <- gaInit(n1, n2)
  #do graduated assignment method
  MPosC <- gaPosC(W,M0,n1,n2)
  #do discreticization
  MPosD <- gaPosD(MPosC)
  #error comparison
  acc <- matchAsg(MPosD, originM)
  return(acc)
}