#' the function to discreticize the assignment matrix using greedy algorithm 
gaPosD <- function(M){
  n1 <- nrow(M)
  n2 <- ncol(M)
  Mnew <- matrix(rep(0,), n1, n2)
  val <- which.max(M)
  while(val > 0){
    idx <- which(M==M[which.max(M)], arr.ind = T)
    Mnew[idx] <- 1
    M[idx[1],] <- 0
    M[,idx[2]] <- 0
    val <- which.max(M)
  }
  
  return(Mnew)
}