#Set work directory
setwd("E:/Jeff/columbia/Projects/Lall/Echo-State-RNN")

#Load libraries
library(devtools)
#install_github("andrewzm/STRbook")
library("ggplot2")
library("dplyr")
library("STRbook")
library("tidyr")

#Load Data
df = read.csv("df_monthly.csv")

#Set train set length(rows/time points)
trainlen <- 480*0.9

#Forecast lead time (month)
tau_1 <- 6 
tau_2 <- 1

#delete time index cols
df <- df[,3:1068] 
df <- as.matrix(df)
#train dataframe
df_train <- df[1:trainlen,] 

######
#spatial mean
######
spat_mean_1 <- apply(df_train, 1, mean)

#detrend
df_detrend <- df_train - outer(rep(1, trainlen), spat_mean_1)

#Normalize
df_norm <- 1/sqrt(trainlen - 1)*df_detrend 

#SVD
E_1 <- svd(df_norm)

# for PC time series
pc_1 <- prcomp(df_norm)
screeplot(pc_1)
summary(pc_1) #The first 5 EOFs explains 80% variannce

#EOFs (PCs) to retain
n_1 <- 10 
n_2 <- 5

PHI_1 <- E_1$v[, 1:n_1] 
PHI_2 <- E_1$v[, 1:n_2]

# project data onto basis functions
TS_1 <- df %*% PHI_1 
TS_2 <- df %*% PHI_2

## training set
xTrainIndex_1 <- 1:(trainlen - tau_1) # training period ind. for input
yTrainIndex_1 <- (tau_1+1):(trainlen) # shifted period ind. for output
xTrain_1 <- TS_1[xTrainIndex_1, ] # training input time series
yTrain_1 <- TS_1[yTrainIndex_1, ] # training output time series

xTrainIndex_2 <- 1:(trainlen - tau_2) # training period ind. for input
yTrainIndex_2 <- (tau_2+1):(trainlen) # shifted period ind. for output
xTrain_2 <- TS_2[xTrainIndex_2, ] # training input time series
yTrain_2 <- TS_2[yTrainIndex_2, ] # training output time series

# test set: For prediction, we consider forecasting at ten three-month intervals
# I chose three-month intervals to improve the visualization, but one can 
#forecast each month if desired
xTestIndex_1 <- seq(trainlen, by = 3, length.out = 10)
yTestIndex_1 <- xTestIndex_1+tau_1 # test output indices
xTest_1 <- TS_1[xTestIndex_1,] # test input data
yTest_1 <- TS_1[yTestIndex_1,] # test output data
testLen_1 <- nrow(xTest_1) # number of test cases

#Here, I forecast 30 one-month
xTestIndex_2 <- seq(trainlen, by = 1, length.out = 30)
yTestIndex_2 <- xTestIndex_2+tau_2 # test output indices
xTest_2 <- TS_2[xTestIndex_2,] # test input data
yTest_2 <- TS_2[yTestIndex_2,] # test output data
testLen_2 <- nrow(xTest_2) # number of test cases


