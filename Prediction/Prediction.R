# setwd("/Pysrc/INFO7390/Midterm/Prediction")
source("Prediction_preprocess.R")

# Load data for Q12005
dummydata <- preprocess("Q1", "2005")
summary(dummydata)

# do the same for Q22005
testdata <- preprocess("Q2", "2005")
str(testdata)
summary(testdata)

#######################################
### # Regresssion # ##############
#######################################
# Regression
## using all variables 
lm.full <- lm(int_rt~., data = dummydata)
summary(lm.full)
# evaluation
library(forecast)
pred.full <- predict(lm.full, testdata)
accuracy(pred.full, testdata$int_rt)
#                    ME         RMSE          MAE         MPE        MAPE
# Test set 0.1148881417 0.3209110446 0.2468488484 1.746435496 4.211781922

# Variable selection
## Exhaustive
library(leaps)
regfit.full <- regsubsets(int_rt~., data = dummydata, nvmax = 25)
reg.summary <- summary(regfit.full)
## plot
## Plotting and choosing the subset
par(mfrow=c(2,1)) 
plot(reg.summary$rss ,xlab="Number of Variables ",ylab="RSS", type="l") 
title("Exhaustive Selection")
plot(reg.summary$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
title("Exhaustive Selection")
coef(regfit.full ,10)

## Forward
regfit.fwd <- regsubsets(int_rt~., data = dummydata, nvmax = 25, method = "forward")
F <- summary(regfit.fwd)
par(mfrow=c(2, 1)) 
plot(F$rss ,xlab="Number of Variables ",ylab="RSS", type="l") 
title("Forward Selection")
plot(F$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
title("Forward Selection")
coef(regfit.fwd, 10)

## Backward
regfit.bwd <- regsubsets(int_rt~., data = dummydata, nvmax = 25, method = "backward")
B <- summary(regfit.bwd)
par(mfrow=c(2, 1)) 
plot(B$rss ,xlab="Number of Variables ",ylab="RSS", type="l") 
title("Backward Selection")
plot(B$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
title("Backward Selection")
coef(regfit.fwd, 10)

## Evaluation of 10 variables from variable selection
lm.ten <- lm(int_rt~fico+mi_pct+orig_upb+orig_loan_term+occpy_sts.O+occpy_sts.S+channel.C+channel.T+prop_type.MH+loan_purpose.P, data=dummydata)
summary(lm.ten)
pred.ten <- predict(lm.ten, testdata)
accuracy(pred.ten, testdata$int_rt)
lm.ten
#                    ME         RMSE          MAE        MPE        MAPE
# Test set 0.1146751027 0.3210624456 0.2468204591 1.74121124 4.210971521

## Stepwise
## This step takes a long time
lm.null <- lm(int_rt~1, data = dummydata)
summary(lm.null)
lm.stepwise <- step(lm.null, scope = list(upper=lm.full), data = dummydata, direction = "both")
summary(lm.stepwise)
pred.stepwise <- predict(lm.stepwise, testdata)
accuracy(pred.stepwise, testdata$int_rt)
# 23 variables except ltv
#                    ME         RMSE          MAE         MPE        MAPE
# Test set 0.1148855889 0.3209114124 0.2468460591 1.746389998 4.211726504

#######################################
### # Regresssion Tree # ##############
#######################################
# Regresssion Tree
library(tree)
tree.int_rt <- tree(int_rt~., dummydata)
summary(tree.int_rt)

# Regression tree result:
# Variables actually used in tree construction:
#   [1] "orig_loan_term" "orig_upb"       "mi_pct"        
# Number of terminal nodes:  4 
# Residual mean deviance:  0.092547 = 32552.02 / 351735 
# Distribution of residuals:
#   Min.     1st Qu.      Median        Mean     3rd Qu. 
# -2.36484200 -0.19700740  0.01015754  0.00000000  0.17799260 
# Max. 
# 4.51384100 

## plot
plot(tree.int_rt)
text(tree.int_rt, pretty=0)
## prune tree nodes
# cv.int_rt <- cv.tree(tree.int_rt)
# plot(cv.int_rt$size, cv.int_rt$dev, type='b')

## Evaluation of Decision Tree
y.int_rt <- predict(tree.int_rt, testdata)
accuracy(y.int_rt, testdata$int_rt)
#                    ME         RMSE          MAE         MPE        MAPE
# Test set 0.1079303849 0.3346851751 0.2544392757 1.593644953 4.339359447

#######################################
### # Neural Network # ##############
#######################################
# Neural Network
library(neuralnet)
net.traindata <- createDummyFeatures(dummydata, cols = c("flag_fthb", "cd_msa", "ppmt_pnlty"), method = "reference")
net.testdata <- createDummyFeatures(testdata, cols = c("flag_fthb", "cd_msa", "ppmt_pnlty"), method = "reference")

## all variables
n <- names(net.traindata)
f <- as.formula(paste("int_rt ~", paste(n[!n %in% "int_rt"], collapse = " + ")))
# net.full <- neuralnet(f, net.traindata, hidden=c(20,20), threshold = 0.01)
net.full <- neuralnet(f, net.traindata, hidden=2, threshold = 0.01)
plot(net.full)
net.pred <- compute(net.full, net.testdata[,-8])
net.result <- net.pred$net.result

## Evaluation of ANN
MAE(net.result, net.testdata[, 8])
RMSE(net.result, net.testdata[, 8])
MAPE(net.result, net.testdata[, 8])
# > MAE(net.result, net.testdata[, 8])
# [1] 0.2899193012
# > RMSE(net.result, net.testdata[, 8])
# [1] 0.3739457409
# > MAPE(net.result, net.testdata[, 8])
# [1] 4.936072433

#######################################
### # Random Forest # ##############
#######################################
# Random Forest
# install.packages("randomForest")
library(randomForest)

# sampling (1%)
smp_size <- floor(0.01 * nrow(dummydata))
set.seed(21)
index <- sample(1:nrow(dummydata), size = smp_size)
forest.dummydata <- dummydata[index, ]

random.int_rt <- randomForest(int_rt~., forest.dummydata)
random.int_rt
varImpPlot(random.int_rt)

## plot
plot(random.int_rt)

## Evaluation of Random Forest
library(forecast)
random.y.int_rt <- predict(random.int_rt, testdata)
accuracy(random.y.int_rt, testdata$int_rt)
#                 ME      RMSE       MAE      MPE     MAPE
# Test set 0.1121943 0.3223013 0.2476922 1.701566 4.228776

#######################################
### # What-if Analysis # ##############
#######################################
# Financial crisis
source("Prediction_preprocess.R")
source("Prediction_WhatIfAnalysis.R")
# test demo
Q12005_Q22005 <- what_if("Q1", "2005", "Q2", "2005")
Q12005_Q22005$lm.ten
Q12005_Q22005$result

Q12005 <- preprocess("Q1", "2005")
Q22005 <- preprocess("Q2", "2005")
Q12005_Q22005 <- what_if_analysis(Q12005, Q22005)
summary(Q12005_Q22005$lm.ten)
Q12005_Q22005$result

## Q12007, Q22007, Q22007, Q42007
Y2007_Q12008 <- predict_whole_year(2007)

## Q12009, Q22009, Q32009, Q42009
Y2009_Q12010 <- predict_whole_year(2009)

# Economic boom
## Q11999, Q21999, Q31999, Q41999
Y1999_Q12000 <- predict_whole_year(1999)

## Q12013, Q22013, Q32013, Q42013
Y2013_Q12014 <- predict_whole_year(2013)



