#' Obtain parameters for graph matching algorithm.
#'
#' \code{gmPar} returns parameters for each graph matching algorithm.
#'
#' @param tag type of pair, can be 1 or 2.
#' 1 : ga, sm, smac, ipfp1, ipfp2, rrwm, fgmU;
#' 2 : ga, sm, smac, ipfp1, ipfp2, rrwm, fgmU, fgmD.
#' @return List of length 2.
#' pars: parameters for each algorithm, list of length nAlg, 
#' and list of length nPari;
#' algs: algorithm name, list of length nAlg.
#' @examples
#' gmPar(2)
gmPar <- function(tag){
  pars <- list()
  algs <- list()

  if(tag == 1){
    # graph matching with symmetric edge feature
    
    # GA
    parIni <- list('alg' = 'unif', 'nor' = 'none')
    parPosC <- list('alg' = 'grad', 'b0' = .5, 'bMax' = 10)
    # parPosC <- list('alg' = 'grad', 'b0' = max(ns), 'bMax' = 200) # better
    parPosD <- list('alg' = 'hun')
    pars$GA <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$GA <- 'GA'
    
    # PM
    pars$PM <- list()
    algs$PM <- 'PM'
    
    # SM
    parIni <- list('alg' = 'sm', 'top' = 'eigs')
    parPosC <- list('alg' = 'none')
    parPosD <- list('alg' = 'gre')
    pars$SM <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$SM <- 'SM'
    
    # SMAC
    parIni <- list('alg' = 'smac', 'top' = 'eigs')
    parPosC <- list('alg' = 'none')
    parPosD <- list('alg' = 'hun')
    pars$SMAC <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$SMAC <- 'SMAC'
    
    # IPFP-U
    nAlg <- nAlg + 1
    parIni <- list('alg', 'unif', 'nor', 'doub')
    parPosC <- list('alg', 'none')
    parPosD <- list('alg', 'ipfp', 'deb', 'n')
    pars$`IPFP-U` <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$`IPFP-U` <- 'IPFP-U'
    
    # IPFP-S
    parIni <- list('alg' = 'sm', 'top' = 'eigs')
    parPosC <- list('alg' = 'none')
    parPosD = list('alg' = 'ipfp', 'deb' = 'n')
    pars$`IPFP-S` <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$`IPFP-S` <- 'IPFP-S'
    
    # RRWM
    parIni <- list('alg' = 'unif', 'nor' = 'unit')
    parPosC <- list('alg' = 'rrwm')
    parPosD <- list('alg' = 'hun')
    pars$RRWM <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$RRWM <- 'RRWM'
    
    # FGM
    parFgmS <- list('nItMa' = 100, 'nAlp' = 101, 'thAlp' = 0, 'deb' = 'n')
    pars$FGM <- list(parFgmS = parFgmS)
    algs$FGM <- 'FGM'
  }else if(tag == 2){
    # graph matching with asymmetric edge feature
    
    
    # GA
    parIni <- list('alg' = 'unif', 'nor' = 'none')
    parPosC <- list('alg' = 'grad', 'b0' = .5, 'bMax' = 10)
    # parPosC <- list('alg' = 'grad', 'b0' = max(ns), 'bMax' = 200) # better
    parPosD <- list('alg' = 'hun')
    pars$GA <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$GA <- 'GA'
    
    # PM
    pars$PM <- list()
    algs$PM <- 'PM'
    
    # SM
    parIni <- list('alg' = 'sm', 'top' = 'eigs')
    parPosC <- list('alg' = 'none')
    parPosD <- list('alg' = 'hun')
    pars$SM <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$SM <- 'SM'
    
    # SMAC
    parIni <- list('alg' = 'smac', 'top' = 'eigs')
    parPosC <- list('alg' = 'none')
    parPosD <- list('alg' = 'hun')
    pars$SMAC <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$SMAC <- 'SMAC'
    
    # IPFP-U
    parIni <- list('alg' = 'unif', 'nor' = 'doub')
    parPosC <- list('alg' = 'none')
    parPosD <- list('alg' = 'ipfp', 'deb' = 'n')
    pars$`IPFP-U` <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$`IPFP-U` <- 'IPFP-U'
    
    # IPFP-S
    parIni <- list('alg' = 'sm', 'top' = 'eigs')
    parPosC <- list('alg' = 'none')
    parPosD <- list('alg' = 'ipfp', 'deb' = 'n')
    pars$`IPFP-S` <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$`IPFP-S` <- 'IPFP-S'
    
    # RRWM
    parIni <- list('alg' = 'unif', 'nor' = 'unit')
    parPosC <- list('alg' = 'rrwm')
    parPosD <- list('alg' = 'hun')
    pars$RRWM <- list(parIni = parIni, parPosC = parPosC, parPosD = parPosD)
    algs$RRWM <- 'RRWM'
    
    # FGM-U
    parFgmS <- list('nItMa' = 100, 'nAlp' = 101, 'thAlp' = 0, 
                    'deb' = 'n', 'ip' = 'n')
    pars$`FGM-U` <- list(parFgmS = parFgmS)
    algs$`FGM-U` <- 'FGM-U'
    
    # FGM-D
    parFgmA <- list('nItMa' = 100, 'nAlp' = 101, 'deb' = 'n', 
                    'ip' = 'n', 'lamQ' = .5)
    pars$'FGM-D' <- list(parFgmA = parFgmA)
    algs$'FGM-D' <- 'FGM-D'
    
  }else{
    stop('unknown tag: ', tag)
  }

  return(list(pars = pars, algs = algs))
}


