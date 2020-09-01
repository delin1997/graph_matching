#' Matrix division.
#'
#' \code{mdiv} conducts matrix division.
#'
#' @param ori orientation, 'vert' or 'horz'.
#' @param Ms original matrix of d*n.
#' @param ks #dimension, length m vector.
#' @return \code{Ms}: matrix set, list of length m, 
#'  each with d_i*n_i matrix.
mdiv <- function(ori, M, ks){
  
  cum <- 0
  
  if(ori=='horz'){
    Ms <- lapply(as.list(ks), function(x){
      Mi <- M[, (cum+1):(cum+x)]
      cum <<- cum + x
      return(Mi)
    })
  }else if(ori=='vert'){
    Ms <- lapply(as.list(ks), function(x){
      Mi <- M[(cum+1):(cum+x), ]
      cum <<- cum + x
      return(Mi)
    })
  }else{
    stop('unknown direction: ', ori)
  }
  
  return(Ms)
}
