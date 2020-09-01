source("conDst.R")
source("knlEgNor.R")
#' Compute node and feature affinity matrix for graph matching.
#'
#' \code{conKnlGphPQD} returns node and feature affinity matrix for graph matching.
#'
#' The edge is directed and the edge feature is asymmetric.
#'
#' @param gphs List of length 2,
#'  G: node-edge incidence matrix, matrix of ni*mi;
#'  H: node-edge incidence matrix, matrix of ni*mi.
#' @param parKnl parameter,
#'  alg: method of computing affinity, 'toy'(default), 'pas' or 'ucf',
#'   'toy': toy data;
#'   'pas': Pascal data;
#'   'ucf': UCF shape data.
#'  beta: weight to add on the pairwise affinity to make it be positive,
#'  default is 0;
#'  lamQ: weight to trade-off between the unary cost and the pairwise cost,
#'  default is 1.
#' @return List of length 4,
#'  KP: node-node affinity, matrix of n1*n2;
#'  KQ: edge-edge affinity, matrix of m1*m2;
#'  beta: weight;
#'  lamQ: weight.
conKnlGphPQD <- function(gphs, parKnl){
  # function parameter
  alg <- ifelse(is.null(parKnl$alg), 'toy', parKnl$alg)
  beta <- ifelse(is.null(parKnl$beta), 0, parKnl$beta)
  lamQ <- ifelse(is.null(parKnl$lamQ), 1, parKnl$lamQ)
  
  # dimension
  gph1 <- gphs[[1]]
  gph2 <- gphs[[2]]
  n1 <- nrow(gph1$G)
  m1 <- ncol(gph1$G)
  n2 <- nrow(gph2$G)
  m2 <- ncol(gph2$G)
  cat('-conKnlGphPQD: ', 'alg ', alg, ', n1 ', n1, ', n2 ', n2, 
          ', m1 ', m1, ', m2 ', m2, "\n")
  
  # for toy data
  if(alg=='toy'){
  KP <- matrix(0, n1, n2)
  DQ <- conDst(gph1$XQ, gph2$XQ)
  KQ <- exp(-DQ / .15)
  
  # for Pascal data
  #elseif strcmp(alg, 'pas')
  #DP = conDst(gphs{1}.XP, gphs{2}.XP);
  #KP = exp(-real(sqrt(DP)));    
  
  # distance
  #Dst1 = repmat(gph1.dsts', 1, m2);
  #  Dst2 = repmat(gph2.dsts, m1, 1);
  #  Dst = abs(Dst1 - Dst2) ./ (min(Dst1, Dst2) + eps);

    # angle
  #  Ang1 = repmat(gph1.angAs', 1, m2);
  #Ang2 = repmat(gph2.angAs, m1, 1);
  #Ang = abs(Ang1 - Ang2);
  
  # combine distance and angle
  #KQ = exp(-(Dst + Ang) / 2);
  
  # for Pascal data
  #elseif strcmp(alg, 'pas2')
  #DP = conDst(gphs{1}.XP, gphs{2}.XP);
  #KP = exp(-real(sqrt(DP)));    
  
  # distance
  #Dst1 = repmat(gph1.dsts', 1, m2);
  #  Dst2 = repmat(gph2.dsts, m1, 1);
  #  Dst = abs(Dst1 - Dst2) ./ (min(Dst1, Dst2) + eps);

    # angle
  #  Ang1 = repmat(gph1.angAs', 1, m2);
  #Ang2 = repmat(gph2.angAs, m1, 1);
  #AngD = Ang1 - Ang2;
  
  #AngD2 = abs(AngD);
  
  #Vis = AngD < 0;
  #AngD(Vis) = -AngD(Vis);
  
  #Vis = AngD > pi;
  #AngD(Vis) = 2 * pi - AngD(Vis);
  
  # combine distance and angle
  #KQ = exp(-(Dst + AngD) / 2);
  
  # for UCF data
  #elseif strcmp(alg, 'ucf')
  #[P1, Q1] = stFld(gphs{1}, 'Pt', 'PtD');
  #[P2, Q2] = stFld(gphs{2}, 'Pt', 'PtD');
  
  # KP
  #KP = 2 * P1' * P2 - (P1 .* P1)' * ones(2, n2) - ones(n1, 2) * (P2 .* P2);
  #KP = KP * (1 - lamQ);
  
  # KQ
  #KQ = 2 * Q1' * Q2 - (Q1 .* Q1)' * ones(2, m2) - ones(m1, 2) * (Q2 .* Q2);
  #    KQ = exp(KQ);
  
  # make sure KQ is positive
  #kqMi = min(KQ(:));
  #beta = -kqMi + beta;
  #KQ = KQ + beta;
  #KQ = KQ * lamQ;
  #kqMi = min(KQ(:));
  #kqMa = max(KQ(:));
  #pr('kqMi #.2f, kqMa #.2f', kqMi, kqMa);
  
  # for computational photography Class
  #elseif strcmp(alg, 'cg')
  #XP1 = gphs{1}.XP;
  #XP2 = gphs{2}.XP;
  #DP = conDstChi(XP1, XP2);
  #KP = exp(-real(sqrt(DP)));    
  
  # distance
  #Dst1 = repmat(gph1.dsts', 1, m2);
  #  Dst2 = repmat(gph2.dsts, m1, 1);
  #  Dst = abs(Dst1 - Dst2) ./ (min(Dst1, Dst2) + eps);

    # angle
  #  Ang1 = repmat(gph1.angs', 1, m2);
  #Ang2 = repmat(gph2.angs, m1, 1);
  #Ang = abs(Ang1 - Ang2);
  
  # combine distance and angle
  #KQ = exp(-(Dst + Ang) / 2);
  
  }else{
    stop('unknown algorithm: ', alg)
  }
    
  
  # normalize
  KQ <- knlEgNor(KQ, parKnl)
  
  return(list(KP = KP, KQ = KQ, beta = beta, lamQ = lamQ))
}
