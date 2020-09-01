source("gphD2U.R")
#' Convert a directed graph to an undirected one.
#'
#' \code{gphD2Us} converts a directed graph to an undirected one.
#'
#' @param gphDs directed graphs, list of length m(2).
#' @return gphUs: undirected graphs, list of length m(2).
gphD2Us <- function(gphDs){
  # dimensin
  m <- length(gphDs)
  
  gphUs <- lapply(as.list(1:m), function(i){
    gphD2U(gphDs[[i]])
  })
  
  return(gphUs)
}
