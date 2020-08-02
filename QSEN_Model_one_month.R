#Set work directory
setwd("E:/Jeff/columbia/Projects/Lall/Echo-State-RNN")

#Load libraries
library(devtools)
#install_github("andrewzm/STRbook")
library("ggplot2")
library("dplyr")
library("STRbook")
library("tidyr")

#Model choices
quadInd <- TRUE # include both quadratic and linear output terms
quadInd_1 <- FALSE# if FALSE, then include only linear terms

ensembleLen <- 200# number of ensemble members (i.e., QESN runs)

#Parameters (can be trained by cross-validation or out-of-sample validation)
wWidth <- .10 # W-weight matrix, uniform dist "width" param.
uWidth <- .10 # U-weight matrix, uniform dist "width" param.
piW <- .10 # sparseness parameter for W-weight matrix
piU <- .10 # sparseness parameter for U-weight matrix
curNh <- 300 # number of hidden units
curNu <- .35 # scaling parameter for W-weight matrix
curM <- 4 # number of embeddings
tauEMB <- 6 # embedding lag
curRV <- .01 # output ridge regression parameter

## standardize and create embedding matrices (Here I focus on one-month forecasting)
xTestIndex = xTestIndex_2
#create a data object containing responses and embedding matrix inputs 
DataObj <- createEmbedRNNData(curM, tauEMB, tau_2, yTrain_2, TS_2,
                              xTestIndex_2)
#create a parameter object that contains the
#parameters to be used in constructing the ESN
n=n_2
setParObj <- setParsEESN(curRV ,curNh, n, curM, quadInd)
#Creat arrays to save forecasts
fmatESNFin <- array(NA, c(ensembleLen, testLen_2, n)) #time point
fmatESNFinFull <- array(NA,c(ensembleLen, testLen_2, 1066)) #spatial location

# run the ensemble of QESN models to obtain forecasts
for(iEnsem in 1:ensembleLen) {
  ## Run the QESN model for a single ensemble
  QESNOutObj = genResR(nh = curNh,
                       wWidth = wWidth,
                       uWidth = uWidth,
                       piW = piW,
                       piU = piU,
                       nuESN = curNu,
                       quadInd = quadInd,
                       DataObj = DataObj,
                       setParObj = setParObj,
                       testLen = testLen_2)
  ## save forecasts for the reduced dimension output
  fmatESNFin[iEnsem, , ] <- t(QESNOutObj$unScaledForecasts)
  ## forecasts for the full spatial field
  fmatESNFinFull[iEnsem, , ] <- fmatESNFin[iEnsem, , ] %*% t(PHI_2)
}
