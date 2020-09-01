source("gphEg.R")
source("gphEg2IncA.R")
source("gphEg2Feat.R")
#' Generate a graph by connecting points.
#'
#' \code{newGphA} generates a graph by connecting points.
#'
#' The edge feature is asymmetric.
#'
#' @param Pt graph node, d*n matrix.
#' @param  parGph  -  parameter for computing the adjacency matrix
#'  see \code{\link{gphEg}} for more details.
#' @param gphs graphs, list of length 2.
#' @return \code{gph}: graph list,
#'  Pt: graph node, d*n matrix;
#'  Eg: graph edge, 2*m matrix;
#'  vis: binary indicator of nodes that have been kept, length n vector or \code{NULL};
#'  G: node-edge adjacency (for starting point), n*m matrix;
#'  H: node-edge adjacency (for ending point), n*m matrix;
#'  PtD: edge feature, 2*m matrix;
#'  dsts: distance, length m vector;
#'  angs: angle, length m vector.
newGphA <- function(Pt, parGph){
  # dimension
  n <- ncol(Pt)
  
  # edge
  EgInfo <- gphEg(Pt, parGph)
  Eg <- EgInfo$Eg
  vis <- EgInfo$vis
  
  # incidence for asymmetric edge
  Inc <- gphEg2IncA(Eg, n)
  G <- Inc$G
  H <- Inc$H
  
  # second-order feature
  Feat <- gphEg2Feat(Pt, Eg)
  PtD <- Feat$PtD
  dsts <- Feat$dsts
  angs <- Feat$angSs
  angAs <- Feat$angAs
  
  # store
  gph <- list()
  gph$Pt <- Pt
  gph$Eg <- Eg
  gph$vis <- vis
  gph$G <- G
  gph$H <- H
  gph$PtD <- PtD
  gph$dsts <- dsts
  gph$angs <- angs
  gph$angAs <- angAs
  
  return(gph)
}
