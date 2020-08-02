#Set work directory
setwd("E:/Jeff/columbia/Projects/Lall/Echo-State-RNN")

#Load libraries
library(devtools)
#install_github("andrewzm/STRbook")
library("ggplot2")
library("dplyr")
library("STRbook")
library("tidyr")

#To assess whether or not we have the correct coverage of the prediction intervals,
#we consider 95% (pointwise) prediction intervals.
alpha <- .05 # alpha-level of 1-alpha pred. intervals (P.I.s)
lwPI <- alpha/2

# calculate the mean and the lower/upper boundaries of the
#95% prediction interval( (across the whole ensemble of realizations
#from the predictive distribution)
TXAvgPreds <- TXLwPI <- TXUpPI <- rep(NA, testLen_2)

TXIndex <- c(1:1066)

for(i in 1:testLen_2){
  TXAvgPreds[i] <- fmatESNFinFull[,i,TXIndex] %>%
    mean()
  TXLwPI[i] <- fmatESNFinFull[, i, TXIndex] %>%
    rowMeans() %>%
    quantile(lwPI, na.rm = TRUE)
  TXUpPI[i] <- fmatESNFinFull[,i,TXIndex] %>%
    rowMeans() %>%
    quantile(1 - lwPI, na.rm = TRUE)
}

TX_results <- data.frame(AvgPres = TXAvgPreds,
                         LwPI = TXLwPI,
                         UpPI = TXUpPI)
df_t <- t(df)

#These predictive-distribution summaries can be compared to the 
#average SST at the prediction month, 
#which we calculate as follows:
TX_results$AvgObs <- df_t[TXIndex, yTestIndex_2] %>%
  colMeans()

#allocate the prediction-month labels to the data frame
TX_results$date <- seq(as.Date("2015-06-01"),
                       length.out = 30, by = "1 months")

###########Plot###########
p2 <- ggplot(TX_results) +
  geom_line(aes(x = date, y = AvgObs)) +
  geom_ribbon(aes(x = date, ymin = LwPI, ymax = UpPI),
              alpha = 0.1, fill = "black") +
  geom_line(aes(x = date, y = AvgPres), col = "red") +
  ylab(expression("TX Index")) +
  xlab("month") + theme_bw()


####MSE######
#Observation values
df_test <- t(df_t[TXIndex, yTestIndex_2])
#ensemble 1
df_1 <- fmatESNFinFull[1,,]
#ensemble 50
df_50 <- fmatESNFinFull[50,,]
#MSE for ensemble 1
(sum(as.vector(df_1-df_test)^2))/(1066*30)
(sum(as.vector(df_50-df_test)^2))/(1066*30)
mse = c()
for(i in 1:200){
  mse[i] <- (sum(as.vector(fmatESNFinFull[i,,]-df_test)^2))/(1066*30)
}
mean(mse) #1.741297(ensemble=100); 1.740(ensemble=200)

matrices <- matrix(0, nrow = 30, ncol = 1066)
for (i in 1:200) {
  matrices <- matrices + fmatESNFinFull[i,,]
}
mse_2 = sum(as.vector((matrices/200)-df_test)^2)/(1066*30) #1.737(ensemble=100,200)
