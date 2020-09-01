#set.seed(1)
## src parameter
tag <- 1 
nIn <- 10 # #inliers
nOuts <- c(0,0) # #outliers
egDen <- .5 # edge density
egDef <- 0 # edge deformation
parKnl <- list('alg'='toy', 'Nor'=T) # type of affinity: synthetic data

## algorithm parameter
source("gmPar.R")
pars_algs <- gmPar(2)
pars <- pars_algs$pars
algs <- pars_algs$algs

## src
source("toyAsgSrcD.R")
wsSrc <- toyAsgSrcD(tag, nIn, nOuts, egDen, egDef)
gphs <- wsSrc$gphs
asgT <- wsSrc$asgT

## affinity
source("conKnlGphPQD.R")
KP_KQ <- conKnlGphPQD(gphs, parKnl)# node and edge affinity
KP <- KP_KQ$KP
KQ <- KP_KQ$KQ 
source("conKnlGphKD.R")
K <- conKnlGphKD(KP, KQ, gphs) # global affinity
Ct <- matrix(1, nrow(KP), ncol(KP)) # mapping constraint (default to a constant matrix of one)

## directed graph -> undirected graph (for fgmU and PM)
source("gphD2Us.R")
gphUs <- gphD2Us(gphs)
source("knlGphKD2U.R")
KU_KQU <- knlGphKD2U(KP, KQ, gphUs)
KQU <- KU_KQU$KQU


## Truth
asgT$obj <- as.vector(t(c(asgT$X)) %*% K %*% c(asgT$X))
asgT$acc <- 1
source("gm.R")
## GA
#asgGa = gm(K, Ct, asgT, pars{1}{:});

## PM
#asgPm = pm(K, KQU, gphUs, asgT);

## SM
asgSm <- gm(K, Ct, asgT, pars[[3]])

## SMAC
asgSmac <- gm(K, Ct, asgT, pars[[4]])

## IPFP-U
#asgIpfpU = gm(K, Ct, asgT, pars{5}{:});

## IPFP-S
#asgIpfpS = gm(K, Ct, asgT, pars{6}{:});

## RRWM
#asgRrwm = gm(K, Ct, asgT, pars{7}{:});

## FGM-U
#asgFgmU = fgmU(KP, KQU, Ct, gphUs, asgT, pars{8}{:});
#X = asgFgmU.X;
#asgFgmU.obj = X(:)' * K * X(:);

## FGM-D
#asgFgmD = fgmD(KP, KQ, Ct, gphs, asgT, pars{9}{:});


cat('Truth : acc ', asgT$acc, ' obj ', round(asgT$obj, 2), "\n");
#cat('GA    : acc ', asgGa$acc, ' obj ', round(asgGa$obj, 2), "\n");
#cat('PM    : acc ', asgPm$acc, ' obj ', round(asgPm$obj, 2), "\n");
cat('SM    : acc ', asgSm$acc, ' obj ', round(asgSm$obj, 2), "\n");
cat('SMAC  : acc ', asgSmac$acc, ' obj ', round(asgSmac$obj, 2), "\n");
#cat('IPFP-U: acc ', asgIpfpU$acc, ' obj ', round(asgIpfpU$obj, 2), "\n");
#cat('IPFP-S: acc ', asgIpfpS$acc, ' obj ', round(asgIpfpS$obj, 2), "\n");
#cat('RRWM  : acc ', asgRrwm$acc, ' obj ', round(asgRrwm$obj, 2), "\n");
#cat('FGM-U : acc ', asgFgmU$acc, ' obj ', round(asgFgmU$obj, 2), "\n");
#cat('FGM-D : acc ', asgFgmD$acc, ' obj ', round(asgFgmD$obj, 2), "\n");