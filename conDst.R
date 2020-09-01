#' Compute (squared) distance matrix.
#'
#' \code{conDst} computes (squared) distance matrix.
#'
#' Dij is the squared Euclidean distance between the i-th point in X1 and j-th point in X2.
#' i.e., D[i,j] = || X1[i, ] - X2[j, ] ||_2^2
#'
#' @param X1 1st sample matrix of dim*n1.
#' @param X2 2nd sample matrix of dim*n2.
#' @param dst distance type,
#'    'e': Euclidean distance(default);
#'    'b': binary distance.
#' @return \code{D}: squared distance matrix of n1*n2.
conDst <- function(X1, X2, dst = "e"){
  # Compute (squared) distance matrix.
  #
  # Remark
  #   Dij is the squared Euclidean distance between the i-th point in X1 and j-th point in X2.
  #   i.e., D(i,j) = || X1(:, i) - X2(:, j) ||_2^2
  #
  # Usage (1)
  #   input   -  X1 = rand(3, 5); X2 = rand(3, 6);
  #   call    -  D = conDst(X1, X2);
  #
  # Usage (2): saving the D in the global workspace
  #   input   -  X1 = rand(3, 5); X2 = rand(3, 6);
  #   call    -  conDst(X1, X2);
  #
  # Input
  #   X1      -  1st sample matrix, dim x n1
  #   X2      -  2nd sample matrix, dim x n2
  #   varargin
  #     dst   -  distance type, {'e'} | 'b'
  #              'e': Euclidean distance
  #              'b': binary distance
  #
  # Output
  #   D       -  squared distance matrix, n1 x n2
  #              If nargout == 0, then the distance matrix is saved in the global variable (DG).
  #
  # History
  #   create  -  Feng Zhou (zhfe99@gmail.com), 01-05-2009
  #   modify  -  Feng Zhou (zhfe99@gmail.com), 01-25-2012
  
  # dimension
  X1 <- as.matrix(X1)
  X2 <- as.matrix(X2)
  n1 = nrow(X1)
  n2 = nrow(X2)
  D <- matrix(0, n1, n2)
  invisible(apply(matrix(1:n1), 1, function(i){
    apply(matrix(1:n2), 1, function(j){
      D[i, j] <<- sum((X1[i, ]-X2[j, ])^2)
    })
  }))


  if(dst=='e'){
    # Euclidean distance
  }else if(dst=='b'){
    D <- Re(D > 1e-8)
  }else{
    stop('unknown distance type: ', dst)
  }

  return(D)
}
