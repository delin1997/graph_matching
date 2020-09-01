#' Compute matching constraint.
#'
#' \code{computeConstraintMatching} computes matching constraint.
#'
#' The edge is directed and the edge feature is asymmetric, nn=n1*n2.
#'
#' @param W12 matching hypothesis (W12 is all-one matrix if full-matching).
#' @param constraint constraint mode, 'col', 'row' or 'both'(default).
#' @return List of 2,
#'  C: constraint matrix.
#'  b: constraint vector.
computeConstraintMatching <- function(W12, constraint="both"){
  # constraint = 'col' | 'row' | {'both'}
  # Timothee Cour, 21-Apr-2008 17:31:23
  # This software is made publicly for research use only.
  # It may be modified and redistributed under the terms of the GNU General Public License.
  
  n1 <- nrow(W12)
  n2 <- ncol(W12)
  n12 <- nnzero(W12)
  indj <- 1:n12
  indi <- rep(0, n12)
  temp <- colSums(W12)
  starts <- cumsum(temp) - temp + 1
  ends <- cumsum(temp)
  
  invisible(apply(matrix(1:n2), 1, function(j){
    indi[starts[j]:ends[j]] <<- j
  }))
  
  # C1 = sparse(indi,indj,1,n2+n1,n12);
  C1 = sparseMatrix(indi, indj, x = 1, dims = c(n2, n12))
  
  indj <- 1 : n12
  indi <- rep(0, n12)
  invisible(apply(matrix(1:n2), 1, function(j){
    indi[starts[j]:ends[j]] <<- which(W12[, j]!=0)
  }))
  
  # C2 = sparse(indi+n2,indj,1,n2+n1,n12);
  C2 = sparseMatrix(indi, indj, x = 1, dims = c(n1,n12))
  
  # b1 = ones(n2, 1) / n2;
  # b2 = ones(n1, 1) / n1;
  b1 = rep(1, n2) / n2 * sqrt(n1 * n2)
  b2 = rep(1, n1) / n1 * sqrt(n1 * n2)
  
  if(constraint=="col"){
    C <- C1
    b <- b1
  }else if(constraint=="row"){
    C <- C2
    b <- b2
  }else{
    C <- rbind(C1, C2)
    b <- c(b1, b2)
  }
  
  return(list(C = C, b = b))
}
