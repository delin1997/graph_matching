source("modGausses.R")
#' Generate toy graph models.
#'
#' \code{toyGphMod} generates toy graph models.
#'
#' The edge is directed and the edge feature is asymmetric, nn=n1*n2.
#'
#' @param shp graph shape, only 1 is allowed for now,
#'   1: random graph.
#' @param k #nodes.
#' @return List of length 2,
#'  mes: mean of Gaussian, list of length k, vector of length d;
#'  Vars:  variance of Gaussian, list of length k, matrix of d*d.
toyGphMod <- function(shp, k){
  # random graph
  if(shp == 1){
    # Gauss for each node
    mes_Vars = modGausses(k, dis = 'unif', maMiD = 'n', 
                             rBd = matrix(c(.007, .02),nrow=1))
  }else{
    stop('unknown shape: ', shp)
  }

  
  return(mes_Vars)
}

