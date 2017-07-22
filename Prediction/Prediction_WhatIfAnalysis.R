what_if <- function(q1, y1, q2, y2) {
  source("Prediction_preprocess.R")
  
  # Load data for prior Quarter
  dummydata <- preprocess(q1, y1)
  # summary(dummydata)
  
  # do the same for later Quarter
  testdata <- preprocess(q2, y2)
  # str(testdata)
  # summary(testdata)
  
  library(forecast)
  # Best model from Regression with 10 variables
  if ("channel.T" %in% names(dummydata)) {
    lm.ten <- lm(int_rt~fico+mi_pct+orig_upb+orig_loan_term+occpy_sts.O+occpy_sts.S+channel.C+channel.T+prop_type.MH+loan_purpose.P, data=dummydata)
  } else {
    lm.ten <- lm(int_rt~fico+mi_pct+orig_upb+orig_loan_term+occpy_sts.O+occpy_sts.S+channel.C+prop_type.MH+loan_purpose.P, data=dummydata)
  }
  
  # summary(lm.ten)
  pred.ten <- predict(lm.ten, testdata)
  result <- accuracy(pred.ten, testdata$int_rt)
  
  return(list(lm.ten=lm.ten, result=result))
}

what_if_analysis <- function(dummydata, testdata) {
  library(forecast)
  # Best model from Regression with 10 variables
  lm.ten <- lm(int_rt~fico+mi_pct+orig_upb+orig_loan_term+occpy_sts.O+occpy_sts.S+channel.C+channel.T+prop_type.MH+loan_purpose.P, data=dummydata)
  # summary(lm.ten)
  pred.ten <- predict(lm.ten, testdata)
  result <- accuracy(pred.ten, testdata$int_rt)
  
  return(list(lm.ten=lm.ten, result=result))
}

what_if_result <- function(q1, y1, q2, y2) {
  result <- what_if(q1, y1, q2, y2)
  result.table <- result$result
  row.names(result.table) <- paste(q2, y2, sep = "")
  return(result.table)
}

predict_whole_year <- function(year) {
  # setwd("/Pysrc/INFO7390/Midterm/Prediction")
  source("Prediction_preprocess.R")
  source("Prediction_WhatIfAnalysis.R")
  
  # Run from Q1year to Q1(year+1)
  curr_q <- NULL
  curr_y <- NULL
  prev_q <- NULL
  prev_y <- NULL
  result <- NULL
  year2 <- year + 1
  
  for (y in c(year:year2)) {
    for (q in c("Q1", "Q2", "Q3", "Q4")) {
      curr_q <- q
      curr_y <- y
      
      if (y == year2 & q == "Q2") {
        break
      }
      
      if (!is.null(prev_q) & !is.null(prev_y)) {
        print(paste(q, y, sep = ""))
        curr_result <- what_if_result(prev_q, prev_y, curr_q, curr_y)
        result <- rbind(result, curr_result)
      }
      
      prev_q <- curr_q
      prev_y <- curr_y
    }
  }
  
  return(result)
  # write.csv(result, file = "result_prediction.csv")
  
}
