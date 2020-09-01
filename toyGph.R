source("smpGausses.R")
#' Generate toy graph.
#'
#' \code{toyGph} generates toy graph.
#'
#' @param mes mean of Gaussian, list of length k, vector of length 2.
#' @param Vars variance of Gaussian, list of length k, matrix of 2*2.
#' @return \code{Pt}: graph node set, 2*k matrix.
toyGph <- function(mes, Vars){
  if(length(mes)==0){
    Pt <- matrix()
  }else{
    Pt <- smpGausses(mes, Vars, 1)$X
  }
  
  return(Pt)
}

