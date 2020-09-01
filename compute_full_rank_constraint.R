#' Compute full-rank constraint.
#'
#' \code{compute_full_rank_constraint} computes full-rank constraint.
#'
#' @param C (original) constraint matrix.
#' @param b (original) constraint vector.
#' @return List of 2,
#'  C: (full-rank) constraint matrix.
#'  b: (full-rank) constraint vector.
compute_full_rank_constraint <- function(C, b){
  # Timothee Cour, 21-Apr-2008 17:31:23
  # This software is made publicly for research use only.
  # It may be modified and redistributed under the terms of the GNU General Public License.
  
  eps <- 1e-16
  
  k <- nrow(C)
  n <- ncol(C)
  # assert(k<n);
  
  r <- rankMatrix(C)
  
  if(r < k){
    qr_mod <- qr(C)
    C <- qr.R(qr_mod)
    b <- qr.coef(qr_mod, b)
    temp <- rowSums(C ^ 2)
    ind <- which(temp > eps)
    C <- C[ind, ]
    b <- b[ind]
  }
  
  return(list(C = C, b = b))
}
