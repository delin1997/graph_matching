ConstructImage <- function(A, N, option = 1){
  vecP <- sample(1:N, N)
  I <- diag(1,N,N)
  P <- I[vecP,]
  if(option == 1){
    A_new <- t(P) %*% A %*% P
  }
  A_new <- Matrix(data = A_new, sparse = T)
  return(list(A_new,P))
}