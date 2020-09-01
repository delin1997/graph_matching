source("conDst.R")
#' Connecting points to obtain edge.
#'
#' \code{gphEg} connects points to obtain edge.
#'
#' @param Pt graph node, 2*n matrix.
#' @param parGph parameter,
#'  link: link type, 'full'(default), 'rand', 'del', 'del2', 'knei', 'enei' or 'non',
#'   'full': fully connect;
#'   'rand': randomly connect;
#'   'del' : delaunay triangulation(not available for now);
#'   'del2': delaunay triangulation with refinement(the same as above);
#'   'knei': k nearest neighbor;
#'   'enei': epsilon nearest neighbor;
#'   'non' : no connection (no edge);
#'  val: parameter value(default is 3),
#'   link == 'rand': edge density;
#'   link == 'knei': value of k (#neighbors);
#'   link == 'enei': value of epsilon (the threshold);
#'   link == 'del2': minimum #connected nodes for each point.
#' @return List of 3, 
#'  Eg: edge matrix of 2*2m;
#'  A: node-node adjacency (has to be symmetric), n*n matrix;
#'  vis: binary indicator of nodes that have been kept, length n vector
#'   or \code{NULL}, only used if link == 'del2'.
gphEg <- function(Pt, parGph){
  # function parameter
  link <- ifelse(is.null(parGph$link), "full", parGph$link)
  val <- ifelse(is.null(parGph$val), 3, parGph$val)
  
  # dimension
  Pt <- as.matrix(Pt)
  n <- ncol(Pt)
  
  if(link=='full'){
    # full-connected graph
    A = matrix(1, n, n)
  }else if(link=='rand'){
    # randomly connected graph
    A  <- matrix(runif(n^2), n, n) <= val
  }else if(link=='del'){
    # delaunay triangulation
    A <- gphAdjDel(Pt)
  }else if(link=='del2'){
    # delaunay triangulation with refinement
    A_vis <- gphAdjDelRef(Pt, val)
    A <- A_vis$A
    vis <- A_vis$vis
  }else if(link=='knei'){
    # k-nearest neighbor
    
    # distance
    D <- conDst(t(Pt), t(Pt))
    diag(D) <- Inf
    
    # neighbor
    Idx <- apply(D, 2, order)
    A  <- matrix(0, n, n)
    invisible(apply(matrix(1:n), 1, function(i){
      A[Idx[1:val, i], i] <<- 1
      A[i, Idx[1:val, i]] <<- 1
    }))
  }else if(link=='enei'){
    # eps-nearest neighbor
    D <- conDst(t(Pt), t(Pt))
    D <- Re(sqrt(D))
    diag(D) <- Inf
    A <- D <= val
  }else if(link=='non'){
    # non edge
    A <- matrix(0, n, n)
  }else{
    stop('unknown edge link type: ', link)
  }

  
  # make sure A is symmetric and with 0 on diagonal
  A[!upper.tri(A)] <- 0
  A <- A + t(A)
  
  if(!exists('vis')){
    vis <- NULL
  }

# graph edge
  idx <- unname(which(A!=0&upper.tri(A), arr.ind = T))
  if(nrow(idx)==0){
    Eg <- NULL
  }else{
    Eg <- t(rbind(idx, idx[, 2:1]))
  }
  
  # print information
  m <- nrow(idx)
  cat('-gphAdj: ', 'link ', link,', val ', val, 
          ', #points ', n, ', #edges ', m, "\n")
  
  return(list(Eg = Eg, A = A, vis = vis))
}
