# Functioin to Preprocess data
preprocess <- function(q, y) {
  # global configuration
  quarter <- q
  year <- y
  time <- paste(quarter, year, sep = "")
  # filename <- "data/historical_data1_Q12005/historical_data1_Q12005.txt"
  filename <- paste("data/historical_data1_", time, "/historical_data1_", time, ".txt", sep = "")
  filename
  
  # load original dataset
  origclass <- c('integer','integer','character', 'integer', 'character', 'numeric', 'integer', 'character','numeric','integer','integer','integer','numeric','character','character','character','character', 'character','character','character','character', 'integer', 'integer','character','character','character')
  # origfile_Qnyyyy <- read.table(filename, sep="|", header=FALSE, colClasses=origclass )
  library(data.table)
  rawdata <- fread(filename, sep="|", header=FALSE, colClasses=origclass)
  rawdata <- as.data.frame(rawdata)
  # origfile_Qnyyyy_factor <- read.table(filename, sep="|", header=FALSE)
  # names(origfile_Qnyyyy)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv' ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc')
  names(rawdata)=c('fico','dt_first_pi','flag_fthb','dt_matr','cd_msa',"mi_pct",'cnt_units','occpy_sts','cltv' ,'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st', 'prop_type','zipcode','id_loan','loan_purpose', 'orig_loan_term','cnt_borr','seller_name','servicer_name', 'flag_sc')
  # head(origfile_Qnyyyy)
  # str(origfile_Qnyyyy)
  
  # pre-processing
  # rawdata <- origfile_Qnyyyy
  # names(rawdata)
  # > names(rawdata)
  # [1] "fico"           "dt_first_pi"    "flag_fthb"      "dt_matr"        "cd_msa"
  # [6] "mi_pct"         "cnt_units"      "occpy_sts"      "cltv"           "dti"
  # [11] "orig_upb"       "ltv"            "int_rt"         "channel"        "ppmt_pnlty"
  # [16] "prod_type"      "st"             "prop_type"      "zipcode"        "id_loan"
  # [21] "loan_purpose"   "orig_loan_term" "cnt_borr"       "seller_name"    "servicer_name"
  # [26] "flag_sc"
  # dim(rawdata)
  # summary(rawdata)
  
  ## v1: Credit score
  ## 301 - 850
  ## replace all NA to 300(min)
  # names(rawdata)[1]
  # summary(rawdata[[1]])
  min <- min(na.omit(rawdata[[1]]))
  rawdata[[1]][is.na(rawdata[[1]])] <- min
  # summary(rawdata[[1]])
  
  ## v2: First Payment data
  ## remove this column
  # names(rawdata)[2]
  # summary(rawdata[[2]])
  remove.columns <- c(2)
  
  ## v3: First time homebuyer
  ## replace N to 0
  ## replace Y to 1
  ## replace Others to 1(here) *** also can be 0
  ## because 1st Time Homebuyer does not apply and will be diclosed as "Not Applicable"
  # data.backup <- rawdata
  # names(rawdata)[3]
  # summary(rawdata[[3]])
  # table(rawdata[[3]])
  rawdata[[3]][rawdata[[3]] != "N"] <- 1
  rawdata[[3]][rawdata[[3]] == "N"] <- 0
  # table(rawdata[[3]])
  rawdata[[3]] <- factor(rawdata[[3]],
                         levels=c(0,1),
                         labels=c("N","Y"))
  # str(rawdata[[3]])
  # table(rawdata[[3]])
  
  ## v4: Maturity date
  ## remove this column
  # names(rawdata)[4]
  # summary(rawdata[[4]])
  remove.columns <- c(remove.columns, 4)
  # remove.columns
  
  ## v5: Metropolitan statistical area
  # data.backup <- rawdata
  # names(rawdata)[5]
  # summary(rawdata[[5]])
  # sum(rawdata[[5]] == "")
  rawdata[[5]][rawdata[[5]] != ""] <- 1
  rawdata[[5]][rawdata[[5]] == ""] <- 0
  # table(rawdata[[5]])
  rawdata[[5]] <- factor(rawdata[[5]],
                         levels=c(0,1),
                         labels=c("N","Y"))
  # str(rawdata[[5]])
  # table(rawdata[[5]])
  
  ## v6 Mortgage insurance percentage
  ## 1 - 55%
  ## replace NA to 0
  # data.backup <- rawdata
  # names(rawdata)[6]
  # summary(rawdata[[6]])
  rawdata[[6]][is.na(rawdata[[6]])] <- 0
  # summary(rawdata[[6]])
  
  ## v7: number of units
  # replace NA to median
  # data.backup <- rawdata
  # names(rawdata)[7]
  # summary(rawdata[[7]])
  median <- median(na.omit(rawdata[[7]]))
  rawdata[[7]][is.na(rawdata[[7]])] <- median
  # summary(rawdata[[7]])
  
  ## v8 Occupancy Status
  # data.backup <- rawdata
  # names(rawdata)[8]
  # summary(rawdata[[8]])
  # table(rawdata[8])
  rawdata[[8]] <- factor(rawdata[[8]])
  # str(rawdata[[8]])
  # table(rawdata[[8]])
  
  ## v9: Combined Loan-To-Value
  ## If the calculated CLTV is < 0 or > 200, and the LTV is > 80 and <= 200 settheCLTVtotheLTV. IftheLTVis<80or>200orunknown,set the CLTV to ‘unknown.’ If the CLTV is < LTV, set the CLTV to ‘unknown.’ Unknown is indicated by a null value.
  ## replace NA to mean
  # data.backup <- rawdata
  # names(rawdata)[9]
  # summary(rawdata[[9]])
  mean <- mean(na.omit(rawdata[[9]]))
  rawdata[[9]][is.na(rawdata[[9]])] <- mean
  # summary(rawdata[[9]])
  
  ## v10: Debt-To-Income Ratio
  ## Ratios greater than 65% are indicated by three (3) blank spaces and if unknown are indicated by a Null value.
  ## replace NA to 65% 
  # data.backup <- rawdata
  # names(rawdata)[10]
  # summary(rawdata[[10]])
  rawdata[[10]][is.na(rawdata[[10]])] <- 65
  # summary(rawdata[[10]])
  
  ## v11: UPB
  # data.backup <- rawdata
  # names(rawdata)[11]
  # summary(rawdata[[11]])
  
  ## v12: Loan-To-Value
  ## Ratios below 6% or greater than 105% will be disclosed as “Unknown,” indicated by three (3) blank spaces.
  ## Because v9 also contains 19 NA's
  ## replace NA to mean
  # data.backup <- rawdata
  # names(rawdata)[12]
  # summary(rawdata[[12]])
  mean <- mean(na.omit(rawdata[[12]]))
  rawdata[[12]][is.na(rawdata[[12]])] <- mean
  # summary(rawdata[[12]])
  
  ## v13: Interest rate
  ## use this columns as y
  # data.backup <- rawdata
  # names(rawdata)[13]
  # summary(rawdata[[13]])
  
  ## v14: Channel
  # data.backup <- rawdata
  # names(rawdata)[14]
  # table(rawdata[[14]])
  rawdata[[14]] <- factor(rawdata[[14]])
  # str(rawdata[[14]])
  # table(rawdata[[14]])
  
  ## v15 Prepayment penalty mortgage
  ## replace NA to 0
  # data.backup <- rawdata
  # names(rawdata)[15]
  # table(rawdata[[15]])
  rawdata[[15]][rawdata[[15]] != "Y"] <- 0
  rawdata[[15]][rawdata[[15]] == "Y"] <- 1
  # table(rawdata[[15]])
  rawdata[[15]] <- factor(rawdata[[15]],
                          levels=c(0,1),
                          labels=c("N","Y"))
  # str(rawdata[[15]])
  # table(rawdata[[15]])
  
  ## v16: Product type
  ## all are FRM
  ## remove this column
  # data.backup <- rawdata
  # names(rawdata)[16]
  # table(rawdata[[16]])
  remove.columns <- c(remove.columns, 16)
  # remove.columns
  
  ## v17: State
  ## remove this column
  remove.columns <- c(remove.columns, 17)
  # remove.columns
  
  ## v18: Property type
  ## only 6 rows are NA, remove these rows
  # data.backup <- rawdata
  # names(rawdata)[18]
  # table(rawdata[[18]])
  # rawdata[rawdata[[18]] %in% c("CO","CP","LH","MH","PU","SF"), ]
  # rawdata[rawdata[[18]] != "  ", ]
  rawdata <- rawdata[rawdata[[18]] != "  ", ]
  rawdata[[18]] <- factor(rawdata[[18]])
  # str(rawdata[[18]])
  
  ## v19: Postall code
  ## remove this column
  remove.columns <- c(remove.columns, 19)
  # remove.columns
  
  ## v20: Loan Sequence number
  ## remove this column
  remove.columns <- c(remove.columns, 20)
  # remove.columns
  
  ## v21: Loan Purpose
  # data.backup <- rawdata
  # names(rawdata)[21]
  # table(rawdata[[21]])
  rawdata[[21]] <- factor(rawdata[[21]])
  # str(rawdata[[21]])
  # table(rawdata[[21]])
  
  ## v22 Original Loan Term
  # data.backup <- rawdata
  # names(rawdata)[22]
  # summary(rawdata[[22]])
  
  ## v23: Number of Borrowers
  ## replace NA to 1
  # data.backup <- rawdata
  # names(rawdata)[23]
  # summary(rawdata[[23]])
  rawdata[[23]][is.na(rawdata[[23]])] <- 1
  # summary(rawdata[[23]])
  
  ## v24: Seller name
  ## remove this column
  remove.columns <- c(remove.columns, 24)
  # remove.columns
  
  ## v25: Servicer name
  ## remove this column
  remove.columns <- c(remove.columns, 25)
  # remove.columns
  
  ## v26: Super Conforming Flag
  ## All rows are NA, remove this column
  # data.backup <- rawdata
  # names(rawdata)[26]
  # table(rawdata[[26]])
  remove.columns <- c(remove.columns, 26)
  # remove.columns
  
  # save necessary columns, and get clean data
  # remove #2,4,16,17,19,20,24,25,26 columns
  cleandata <- rawdata[, -remove.columns]
  # return(cleandata)
  
  ## dummy 
  # occpy_sts(8), channel(14), prop_type(18), loan_purpose(21)
  # install.packages("mlr")
  library(mlr)
  dummydata <- createDummyFeatures(cleandata, cols = c("occpy_sts", "channel", "prop_type", "loan_purpose"), method = "reference")
  return(dummydata)
  
}

# Function to calculate MAE, RMSE, MAPE
MAE <- function(a, b) {
  mean(abs(a-b))
}

RMSE <- function(a, b) {
  sqrt(mean((a-b)^2))
}

MAPE <- function(a, b) {
  mean(abs((a-b)/b)) * 100
}

