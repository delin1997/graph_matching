source("newGphA.R")
#' Connect nodes to generate edges.
#'
#' \code{newGphAs} connects nodes to generate edges.
#'
#' The edge is directed and the edge feature is asymmetric, nn=n1*n2.
#'
#' @param Pts graph node, list of length mG, d*ni matrix.
#' @param parGph parameter for computing the adjacency matrix;
#'              see \code{\link{gphEg} for more details.
#' @return \code{gphs}: graph, list of mG.
newGphAs <- function(Pts, parGph){
  # dimension
  mG <- length(Pts)
  
  # per graph
  gphs <- lapply(as.list(1:mG), function(iG){
    newGphA(Pts[[iG]], parGph)
  })
  
  return(gphs)
}

