# setwd("/Pysrc/INFO7390/Midterm/Classification")
source("Classification_preprocess_equal.R")

# load data for Q12005
# 0.01 for sampling percentage
dummydata <- preprocess_classification("Q1", "2005", 0.01)
invisible(gc())
summary(dummydata)
str(dummydata)
names(dummydata)

# do the same to load data for Q22005
testdata <- preprocess_classification("Q2", "2005", 0.01)

#######################################
### # Logistic regression # ###########
#######################################
# Logistic regression
# Y: delq_sts
table(dummydata$delq_sts.Y)
## Using all variables
lr.full <- glm(delq_sts.Y ~ ., data = dummydata, family = binomial(link = "logit"))
summary(lr.full)
## signif: current_upb + loan_age + mths_remng + current_int_rt + non_int_brng_upb + flag_mod.Y

## Using significant variables
lr.signif <- glm(delq_sts.Y ~ current_upb + loan_age + mths_remng + current_int_rt + non_int_brng_upb + flag_mod.Y, data = dummydata, family = binomial(link = "logit"))
summary(lr.signif)

## Evaluatioin
## Confusion Matrix for training data
lr.train.probs <- predict(lr.signif, dummydata, type = 'response')
lr.train.pred <- rep(0, length(lr.train.probs))
### Set the cutoff value to 0.5
lr.train.pred[lr.train.probs >= 0.5] <- 1
library(caret)
confusionMatrix(lr.train.pred, dummydata$delq_sts.Y)
#           Reference
# Prediction      0      1
#          0 247952  10771
#          1      2    123 
# Accuracy : 0.958381

## Confusion Matrix for test data
lr.test.probs <- predict(lr.signif, testdata, type = 'response')
lr.test.pred <- rep(0, length(lr.test.probs))
### Set the cutoff value to 0.5
lr.test.pred[lr.test.probs >= 0.5] <- 1
library(caret)
confusionMatrix(lr.test.pred, testdata$delq_sts.Y)
#           Reference
# Prediction      0      1
#          0 277993  13481
#          1      9    128
# Accuracy : 0.9537397

## ROC for training data
library(ROCR)
predict.train <- ROCR::prediction(lr.train.probs, dummydata$delq_sts.Y)
performance.train <- performance(predict.train, measure = "tpr", x.measure = "fpr")
plot(performance.train, main="lr ROC curve for Training Data", xlab = "1-Specificity", ylab="Sensitivity")

## ROC for test data
predict.test <- ROCR::prediction(lr.test.probs, testdata$delq_sts.Y)
performance.test <- performance(predict.test, measure = "tpr", x.measure = "fpr")
plot(performance.test, main="lr ROC curve for Test Data", xlab = "1-Specificity", ylab="Sensitivity")

#######################################
### # Classification Tree # #################
#######################################
# Classification Tree
tree.traindata <- dummydata
tree.testdata <- testdata
tree.traindata$delq_sts.Y <- factor(tree.traindata$delq_sts.Y, levels=c(0, 1), labels = c("N", "Y"))
tree.testdata$delq_sts.Y <- factor(tree.testdata$delq_sts.Y, levels=c(0, 1), labels = c("N", "Y"))
table(tree.testdata$delq_sts.Y)

library(tree)
tree = tree(delq_sts.Y ~ ., tree.traindata)
summary(tree)
plot(tree)
text(tree, pretty = 0)

# Evaluation
## Confusion Matrix
tree.pred = predict(tree, tree.testdata, type = "class")
confusionMatrix(tree.pred, tree.testdata$delq_sts.Y)
#           Reference
# Prediction      N      Y
#          N 278002  13609
#          Y      0      0
# Accuracy : 0.9533317
# Really bad, right?

## ROC for test data
predict.tree.test <- ROCR::prediction(as.numeric(tree.pred), tree.testdata$delq_sts.Y)
performance.tree.test <- performance(predict.tree.test, measure = "tpr", x.measure = "fpr")
plot(performance.tree.test, main="tree ROC curve for Test Data", xlab = "1-Specificity", ylab="Sensitivity")

#######################################
### # Neural Network # ################
#######################################
# Neural Network
# This step will take a long time
library(neuralnet)
## all variables
n <- names(dummydata)
f <- as.formula(paste("delq_sts.Y ~", paste(n[!n %in% "delq_sts.Y"], collapse = " + ")))
neuralnet <- neuralnet(f, data = dummydata, hidden=3, err.fct="sse", linear.output = FALSE)
plot(neuralnet)

## Evaluation
net.pred <- compute(neuralnet, testdata[,-16])
net.result <- net.pred$net.result

## Confusion Matrix
net.test.pred <- rep(0, length(net.result))
### Set the cutoff value to 0.5
net.test.pred[net.result >= 0.5] <- 1
library(caret)
confusionMatrix(net.test.pred, testdata$delq_sts.Y)
#           Reference
# Prediction      0      1
#          0 278001  13411
#          1      1    198
# Accuracy : 0.9540072

## ROC
## Bad ROC curve
predict.net.test <- ROCR::prediction(net.result, testdata$delq_sts.Y)
performance.net.test <- performance(predict.net.test, measure = "tpr", x.measure = "fpr")
plot(performance.net.test, main="net ROC curve for Test Data", xlab = "1-Specificity", ylab="Sensitivity")


#######################################
### # Random Forest # ##############
#######################################
# Random Forest
# install.packages("randomForest")
library(randomForest)

# sampling (1%) for dummydata
smp_size <- floor(0.01 * nrow(dummydata))
set.seed(21)
index <- sample(1:nrow(dummydata), size = smp_size)
forest.dummydata <- dummydata[index, ]
# sampling (1%) for testdata
smp_size <- floor(0.01 * nrow(testdata))
set.seed(21)
index <- sample(1:nrow(testdata), size = smp_size)
forest.testdata <- testdata[index, ]

forest <- randomForest(as.factor(delq_sts.Y)~., forest.dummydata)
forest

varImpPlot(forest)
## plot
# plot(random.delq_sts.Y)

## Evaluation of Random Forest
library(caret)
## Confusion Matrix
forest.pred = predict(forest, forest.testdata, type = "class")
confusionMatrix(forest.pred, forest.testdata$delq_sts.Y)
#           Reference
# Prediction     0     1
#          0 19075   919
#          1     0     6
# Accuracy : 0.954

## ROC for test data
predict.forest.test <- ROCR::prediction(as.numeric(forest.pred), forest.testdata$delq_sts.Y)
performance.forest.test <- ROCR::performance(predict.forest.test, measure = "tpr",  x.measure = "fpr")
plot(performance.forest.test, main="tree ROC curve for Test Data", xlab = "1-Specificity", ylab="Sensitivity")





#######################################
### # Auto Classification # ##############
#######################################
# setwd("/Pysrc/INFO7390/Midterm/Classification")
source("Classification_preprocess_equal.R")
source("Classification_auto.R")
# test demo
Q12005_Q22005 <- auto_classification("Q1", "2005", "Q2", "2005", 0.01)
Q12005_Q22005

Q12005 <- preprocess_classification("Q1", "2005", 0.01)
Q22005 <- preprocess_classification("Q2", "2005", 0.01)
Q12005_Q22005_auto_from_data <- auto_classification_from_data(Q12005, Q22005, "Q2", "2006")
Q12005_Q22005_auto_from_data
## combine results
Q <- rbind(Q12005_Q22005, Q12005_Q22005_auto_from_data)
Q

# Run from Q11999 to Q22016
curr_q <- NULL
curr_y <- NULL
prev_q <- NULL
prev_y <- NULL
result <- NULL
for (y in c(1999:2016)) {
  for (q in c("Q1", "Q2", "Q3", "Q4")) {
    curr_q <- q
    curr_y <- y
    
    if (y == 2016 & q == "Q3") {
      break
    }
    
    if (!is.null(prev_q) & !is.null(prev_y)) {
      print(paste(q, y, sep = ""))
      curr_result <- auto_classification(prev_q, prev_y, curr_q, curr_y, 0.01)
      result <- rbind(result, curr_result)
    }
    
    prev_q <- curr_q
    prev_y <- curr_y
  }
}