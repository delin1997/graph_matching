#' the function is used to calculate the wrong assignment between the algorithm
#' and the ground-truth
#' @param M the assignment matrix we estimate
#' @param trueM the ground-truth M 
#' the result will be the accurency of the algorithm
matchAsg <- function(M, trueM){
  idx <- which(trueM == 1, arr.ind = T)
  idx2 <- which(M[idx] == 1)
  co <- length(idx2)
  acc <- co/nrow(idx)
  
  return(acc)
}