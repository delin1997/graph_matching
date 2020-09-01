source("bistocNormalize.R")
#' Normalize edge affinity.
#' 
#' \code{knlEgNor} normalizes edge affinity.
#' 
#' @references T. Cour, P. Srinivasan, and J. Shi, "Balanced graph matching", In NIPS, 2006.
#' 
#' @param KQ0 original edge affinity, m1*m2 matrix.
#' @param par parameter, 
#'   Nor: flag of whether to normalize edge affinity, \code{T} or \code{F}(default).
#'   nItMa: #maximum iteration, 10 as default.
#'   th: threshold, 1e-7 as default.
#' @return \code{KQ}: new edge affinity, m1*m2 matrix.
knlEgNor <- function(KQ0, par){
  # function parameter
  isNor <- ifelse(!is.null(par$Nor), par$Nor, FALSE)
  nItMa <- ifelse(!is.null(par$nItMa), par$nItMa, 10)
  th <- ifelse(!is.null(par$th), par$th, 1e-7)
  
  if(!isNor){
    KQ <- KQ0
    return(KQ)
  }
  
  KQ <- bistocNormalize(KQ0, nItMa, th)
  KQ <- KQ / max(KQ)
  
  return(KQ)
}
