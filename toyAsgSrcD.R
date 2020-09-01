source("toyGphMod.R")
source("toyGph.R")
source("newGphAs.R")
source("gphEg2Adj.R")
#' Generate toy source for assignment problem.
#'
#' \code{toyAsgSrcD} returns toy source for assignment problem.
#'
#' The edge is directed and the edge feature is asymmetric.
#'
#' @param tag shape type, only 1 is allowed for now.
#' @param nIn #inlier nodes.
#' @param nOuts #outlier nodes, vector of length 2.
#' @param egDen density of edge connection, 0-1.
#' @param egDef deformation of edge feature, 0-1.
#' @return List of length 6 containing info of toy source.
#' asgT: ground truth assignment;
#' gphs:  graph node set, list of length mG(2);
#' ns: #nodes, vector of length 2;
#' mess: mean of Gaussian, list of length mG(2), 
#' list of length ni, vector of d;
#' Varss: variance of Gaussian, list of length mG(2), 
#' list of length ni, matrix of d*d;
#' ords: order, list of length mG(2), vector of length ni.
toyAsgSrcD <- function(tag, nIn, nOuts, egDen, egDef){
  # dimension & #nodes 
  d <- 2
  mG <- 2
  ns <- rep(0, 2) + nIn + nOuts
  
  # inlier nodes (distribution, not used for computing feature, only for visualization)
  mes_Vars <- toyGphMod(tag, nIn)
  meIns <- mes_Vars$mes
  VarIns <- mes_Vars$Vars
  PtIn <- toyGph(meIns, VarIns)
  
  # outlier nodes
  ords <- list()
  mess <- list()
  Varss <- list()
  Pts <- list()
  for(iG in 1 : mG){
    if(nOuts[iG]>0){
      # outlier node
      mes_Vars <- toyGphMod(1, nOuts[iG])
      meOuts <- mes_Vars$mes
      VarOuts <- mes_Vars$Vars
      PtOut <- toyGph(meOuts, VarOuts)
      
      # combine nodes
      mes <- c(meIns, meOuts)
      Vars <- c(VarIns, VarOuts)
      Pt <- cbind(PtIn, PtOut)
    }else{
      mes <- meIns
      Vars <- VarIns
      Pt <- PtIn
    }
    
    # randomly re-order
    ords[[iG]] <- sample(ns[iG])
    mess[[iG]] <- mes[ords[[iG]]]
    Varss[[iG]] <- Vars[ords[[iG]]]
    Pts[[iG]] <- Pt[, ords[[iG]]]
  }
  
  
  # generate graph
  parGph <- list(link = 'rand', val = egDen)
  gphs <- newGphAs(Pts, parGph)
  
  # edge feature for inliers (asymmetric)
  ZIn <- matrix(runif(nIn^2), nIn, nIn)
  diag(ZIn) <- 0
  
  # feature for each graph
  for(iG in 1 : mG){
    # edge feature (asymmetric)
    ni <- ns[iG]
    Z <- matrix(runif(ni^2), ni, ni)
    diag(Z) <- 0
    
    # only re-set edge features for outliers
    Z[1 : nIn, 1 : nIn] <- ZIn
    
    # add noise on the edge feature
    ZDef <- egDef * matrix(runif(ni^2), ni, ni)
    diag(ZDef) <- 0
    Z <- Z + ZDef
    
    # re-order the edge feature
    Z <- Z[ords[[iG]], ords[[iG]]]
    
    # only keep the feature for existed edges because the graph is sparse
    A_idx <- gphEg2Adj(gphs[[iG]]$Eg, ni)
    idx <- A_idx$idx
    gphs[[iG]]$XQ <- Z[idx]
    
    # node feature (not used in the experiment)
    gphs[[iG]]$XP = rep(0, ni)
  }

  
  # ground-truth assignment
  XT <- matrix(0, ns, ns)
  XT[cbind(1 : nIn, 1 : nIn)] <- 1
  asgT <- list()
  asgT$alg <- 'truth'
  asgT$X <- XT[ords[[1]], ords[[2]]]
  
  # store
  wsSrc <- list()
  wsSrc$mess <- mess
  wsSrc$Varss <- Varss
  wsSrc$ords <- ords
  wsSrc$gphs <- gphs
  wsSrc$asgT <- asgT
  wsSrc$ns <- ns

  return(wsSrc)
}