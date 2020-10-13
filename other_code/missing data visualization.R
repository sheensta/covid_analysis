library(VIM)

setwd("C:/Users/seanx/OneDrive/Desktop/YORK ML/ML1000/Assignments/Assignment 1/mexican covid data")
covid <- read.csv('covid.csv')
covid <- subset(covid,covid$covid_res == 1)
precondition_cols = c('pneumonia','diabetes','copd','asthma','inmsupr','hypertension','other_disease','cardiovascular','obesity','renal_chronic', 'tobacco')


#imputing NA in precondition columns
proc_NA <- function(df){
  df[, names(df) %in% precondition_cols][df[, names(df) %in% precondition_cols] == 98] <- NA
  df[, names(df) %in% precondition_cols][df[, names(df) %in% precondition_cols] == 'Ignored'] <- NA
  df[, names(df) %in% precondition_cols][df[, names(df) %in% precondition_cols] == 97] <- NA
  df[, names(df) %in% precondition_cols][df[, names(df) %in% precondition_cols] == 'Not applicable'] <- NA
  df[, names(df) %in% precondition_cols][df[, names(df) %in% precondition_cols] == 99] <- NA
  df[, names(df) %in% precondition_cols][df[, names(df) %in% precondition_cols] == 'Unknown'] <- NA
  return(df)
}

#making character imputation of categorical columns columns, ignoring AGE (column 9)
proc_char <- function(df){
  df[, !names(df) %in% c('age', 'survtime', 'sex')][df[, !names(df) %in% c('age', 'survtime', 'sex')] == 1] <- 'Y'
  df[, !names(df) %in% c('age', 'survtime', 'sex')][df[, !names(df) %in% c('age', 'survtime', 'sex')] == 2] <- 'N'
  df$sex[df$sex == 1] <- 'F' 
  df$sex[df$sex == 2] <- 'M'
  df[, !names(df) %in% c('age', 'survtime')][df[, !names(df) %in% c('age', 'survtime')] == 97] <- 'Not applicable'
  df[, !names(df) %in% c('age', 'survtime')][df[, !names(df) %in% c('age', 'survtime')] == 98] <- 'Ignored'
  df[, !names(df) %in% c('age', 'survtime')][df[, !names(df) %in% c('age', 'survtime')] == 99] <- 'Unknown'
  return(df)
}

#MISSING Visualizations
aggr_plot = aggr(proc_NA(covid), col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), ylab=c("Histogram of missing data","Pattern"))