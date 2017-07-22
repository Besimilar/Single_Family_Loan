auto_classification <- function(q1, y1, q2, y2, p) {
  # source("Classification_preprocess.R")
  
  # load data for prior Quarter
  # p for percentage sampling
  dummydata <- preprocess_classification(q1, y1, p)
  invisible(gc())
  
  # do the same to load data for later Quarter
  testdata <- preprocess_classification(q2, y2, p)
  
  ## Best model using logistic regression
  ## signif: current_upb + loan_age + mths_remng + current_int_rt + non_int_brng_upb + modcost + flag_mod.Y
  ## Using significant variables
  lr.signif <- glm(delq_sts.Y ~ current_upb + loan_age + mths_remng + current_int_rt + non_int_brng_upb + modcost + flag_mod.Y, data = dummydata, family = binomial(link = "logit"))
  
  ## Confusion Matrix for test data
  lr.test.probs <- predict(lr.signif, testdata, type = 'response')
  lr.test.pred <- rep(0, length(lr.test.probs))
  ### Set the cutoff value to 0.5
  lr.test.pred[lr.test.probs >= 0.5] <- 1
  
  library(caret)
  result <- confusionMatrix(lr.test.pred, testdata$delq_sts.Y)
  # return(result)
  result_table <- result$table
  
  # calculate matrix
  actual_delq <- result_table[1,2] + result_table[2,2]
  predict_delq <- result_table[2,1] + result_table[2,2]
  records <- sum(result_table)
  proper_delq <- result_table[2,2]
  improper_delq <- result_table[2,1]
  
  result_row <- list(actual_delq=actual_delq, predict_delq=predict_delq, records=records, proper_delq=proper_delq, improper_delq=improper_delq)
  result <- data.frame(result_row, row.names = paste(q2, y2, sep = ""))
  return(result)
}

auto_classification_from_data <- function(dummydata, testdata, q2, y2) {
  ## signif: current_upb + loan_age + mths_remng + current_int_rt + non_int_brng_upb + flag_mod.Y
  ## Using significant variables
  lr.signif <- glm(delq_sts.Y ~ current_upb + loan_age + mths_remng + current_int_rt + non_int_brng_upb + flag_mod.Y, data = dummydata, family = binomial(link = "logit"))
  
  ## Confusion Matrix for test data
  lr.test.probs <- predict(lr.signif, testdata, type = 'response')
  lr.test.pred <- rep(0, length(lr.test.probs))
  ### Set the cutoff value to 0.5
  lr.test.pred[lr.test.probs >= 0.5] <- 1
  
  library(caret)
  result <- confusionMatrix(lr.test.pred, testdata$delq_sts.Y)
  # return(result)
  result_table <- result$table
  
  # calculate matrix
  actual_delq <- result_table[1,2] + result_table[2,2]
  predict_delq <- result_table[2,1] + result_table[2,2]
  records <- sum(result_table)
  proper_delq <- result_table[2,2]
  improper_delq <- result_table[2,1]
  
  result_row <- list(actual_delq=actual_delq, predict_delq=predict_delq, records=records, proper_delq=proper_delq, improper_delq=improper_delq)
  result <- data.frame(result_row, row.names = paste(q2, y2, sep = ""))
  return(result)
}