source("gphEg2IncU.R")
#' Convert a directed graph to an undirected one.
#'
#' \code{gphD2U} converts a directed graph to an undirected one.
#'
#' @param gphD directed graph,
#'  Pt: graph node, d*n matrix;
#'  Eg: graph edge, 2*m matrix;
#'  G: node-edge adjacency, n*m matrix;
#'  H: augumented node-edge adjacency, n*m matrix;
#'  PtD: edge feature, 2*m matrix;
#'  dsts: distance, length m vector;
#'  angs: angle, length m vector;
#' @return \code{gphU}: directed graph,
#'  Pt: graph node, d*n matrix;
#'  Eg: graph edge, 2*(2m) matrix;
#'  G: node-edge adjacency, n*m matrix;
#'  H: augumented node-edge adjacency, n*(m + n) matrix;
#'  PtD: edge feature, 2*(2m) matrix;
#'  dsts: distance, length (2m) vector;
#'  angs: angle, length (2m) vector;
gphD2U <- function(gphD){
  # dimension
  n <- ncol(as.matrix(gphD$Pt))
  
  # incidence matrix
  G_H <- gphEg2IncU(gphD$Eg, n)
  
  # store
  gphU <- gphD
  gphU$G <- G_H$G
  gphU$H <- G_H$H
  
  return(gphU)
}
