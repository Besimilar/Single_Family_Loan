# setwd("/Pysrc/INFO7390/Midterm/Classification")
source("Classification_preprocess.R")
source("Classification_auto.R")

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
      curr_result <- auto_classification(prev_q, prev_y, curr_q, curr_y, 0.1)
      result <- rbind(result, curr_result)
    }
    
    prev_q <- curr_q
    prev_y <- curr_y
  }
}

write.csv(result, file = "result_99to16_2M.csv")
