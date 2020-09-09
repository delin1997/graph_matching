compat <- function(A,A_new, n1, n2){
  Aind <- which(A != 0, arr.ind = T)
  A_newind <- which(A_new != 0, arr.ind = T)
  i <- c()
  j <- c()
  x <- c()
  for(a in 1:nrow(Aind)){
    for(k in 1:nrow(A_newind)){
      x <- append(x, 1 - 3*abs(A[Aind[a,1],Aind[a,2]] - A_new[A_newind[k,1],A_newind[k,2]]))
      i <- append(i, A_newind[k,1] + (Aind[a,1]-1) * n2)
      j <- append(j, A_newind[k,2] + (Aind[a,2]-1) * n2)
    }
  }
  W <- sparseMatrix(i=i, j=j, x=x)
  return(W)
}