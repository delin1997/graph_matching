source("smpUnif.R")
source("mdiv.R")
source("a2rot.R")
#' Generate a set of Gaussian models.
#'
#' \code{modGausses} returns the global affinity matrix for graph matching.
#'
#' @param k #classes.
#' @param dis distribution of cluster mean, 'unif'(default) or 'gauss'.
#'        ## uniform distribution ##
#' @param meMi minimum of mean, vector of length d(\code{rep(0, 2)} as default).
#' @param meMa maximum of mean, vector of length d(\code{rep(10, 2)} as default).
#'        ## Gaussian distribution ##
#' @param me0, vector of length dim(\code{rep(0, 2) as default).
#' @param r0 radius, 1 as default.
#' 
#' @param rBd bound of radius, vector of length 2,
#'  \code{c(.05, .13)} as default for 'unif', 
#'  \code{c(.2, .5) * .8} as default for 'gauss'.
#' @param maMiD flag of maximizing the minimum of pairwise distance, 
#'  'y' or 'n'(default).
#' @return List of length 2,
#'  mes: mean of Gaussian, list of length k, vector of length d;
#'  Vars:  variance of Gaussian, list of length k, matrix of d*d.
modGausses <- function(k, dis = "unif", maMiD = "n", meMi = rep(0,2), 
                       meMa = rep(10,2), me0 = rep(0,2),  
                       rBd = switch(dis=="unif", c(.05, .13), 
                                    c(.2, .5) * .8), r0 = 1){
  # generate mean
  if(dis=='unif'){
    
    # hyper-parameter
    rMi <- (meMa - meMi) * rBd[1]
    rMa <- (meMa - meMi) * rBd[2]
    
    mes <- smpUnif(meMi, meMa, k, maMiD = maMiD)
  }else if(dis=='gauss'){
    source("smpGauss.R")
    
    # hyper-parameter
    Var0 <- diag(r0, nrow = 2)
    rMi <- matrix(rep(r0, 2)) * rBd[1]
    rMa <- matrix(rep(r0, 2)) * rBd[2]
    
    mes <- smpGauss(me0, Var0, k, maMiD = maMiD)
  }else{
    stop('unknown distribution: ', dis)
  }
  
  mes <- mdiv('horz', mes, rep(1, k))
  
  # generate variance
  rs <- smpUnif(rMi, rMa, k)$X
  as <- pi * 2 * runif(k)
  
  Vars <- lapply(as.list(1:k), function(i){
    U = a2rot(as[i])
    t(U) %*% diag(rs[, i]^2) %*% U
  })
  

return(list(mes = mes, Vars = Vars))
}

