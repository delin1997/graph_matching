#' Compute graph edge feature.
#'
#' \code{gphEg2Feat} computes graph edge feature.
#'
#' @param Pt graph node, d*n matrix.
#' @param Eg graph edge, 2*m matrix or \code{NULL}.
#' @return List of length 4,
#'  PtD: edge vector, d*m matrix;
#'  dsts: distance, length m vector;
#'  angSs: angle (symmetric), length m vector;
#'  angAs: angle (asymmetric), length m vector.
gphEg2Feat <- function(Pt, Eg){
  if(is.null(Eg)){
    PtD <- NULL
    dsts <- NULL
    angs <- NULL
    angAs <- NULL
  }else{
    # edge vector
    Pt1 <- Pt[, Eg[1, ]]
    Pt2 <- Pt[, Eg[2, ]]
    PtD <- Pt1 - Pt2
    
    # distance
    dsts <- Re(sqrt(colSums(PtD ^ 2)))
    
    # angle (symmetric)
    eps <- 1e-16
    angSs <- atan(PtD[2, ] / (PtD[1, ] + eps))
    
    # angle (asymmetric)
    angAs <- atan2(PtD[2, ], PtD[1, ])
  }
  
  return(list(PtD = PtD, dsts = dsts, angSs = angSs, angAs = angAs))
}
