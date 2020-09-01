#' Obtain the 2-D rotation matrix from the given angle.
#'
#' \code{a2rot} returns the 2-D rotation matrix from the given angle.
#'
#' @param a angle.
#' @return \code{Rot}: rotation matrix of 2*2.
a2rot <- function(a){
  Rot = matrix(c(cos(a), sin(a), -sin(a), cos(a)),nrow = 2, byrow = T)
  return(Rot)
}

