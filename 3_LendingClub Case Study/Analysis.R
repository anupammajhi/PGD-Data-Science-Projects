#Remove all variables
rm(list=ls())

library(gdata)
library(magrittr)
library(dplyr)
library(stringr)

# Unzip compressed file. Not required if already extracted
# unzip("loan.zip")

# Read CSV File
loanData <- read.csv("loan.csv",na.strings=c("","NA"),stringsAsFactors = F) #Also replaces blanks with NA


#======= Data Cleaning and Preparation ========

# ---- .. Remove Irrelevant Data .. ----

# Get rid of columns with all NA values

# By comparing total num of NA in a column with total num of rows in dataframe
loanData <- loanData[,colSums(is.na(loanData)) != nrow(loanData)]

# Get rid of rows with all NA values  

# By comparing total num of NA in a row with total num of rows in dataframe
loanData <-loanData[rowSums(is.na(loanData)) != ncol(loanData),]

# Getting rid of unnecessary columns
# Columns that have all same values or just have one value and NA. 
# Such Columns do not contribute to any valuable analysis. Hence remove them.
# Columns with just one level when treated as factor means they don't have enough values to analyse
# NOTE : unique() function not used as it considers NA as a unique value too
Vague_Columns <- names(which(lapply(loanData,function(x){length(levels(as.factor(x)))}) == 1))
Vague_Columns

# url: no new data that can be obtained
Vague_Columns <- append(Vague_Columns,"url")

# id and member_id
loanData[which(duplicated(loanData$id)),] # 0 Duplicates, hence unique
loanData[which(duplicated(loanData$member_id)),]  # 0 Duplicates, hence unique
# Since both id and member_id are unique, we will exclude member_id

# funded_amnt_inv, out_prncp_inv, total_pymnt_inv : Not required for this analysis as we are not analysing investor's action
# desc, purpose and title : purpose is a better categorical variable for this study, as desc and title are quite random. Hence just use purpose and remove others.
# emp_title : not relevant for this analysis
Vague_Columns <- append(Vague_Columns,c("member_id","funded_amnt_inv","out_prncp_inv","total_pymnt_inv",
                                        "desc","title","emp_title"))
# last_credit_pull_d,next_pymnt_d,last_pymnt_amnt,last_pymnt_d,collection_recovery_fee,recoveries,out_prncp : not relevant for this analysis, as these variables are helpful only post default or full payment
Vague_Columns <- append(Vague_Columns,c("last_credit_pull_d","next_pymnt_d","last_pymnt_amnt","last_pymnt_d","collection_recovery_fee","recoveries","out_prncp"))

# Removing unnecessary columns from dataframe saved as list "Vague_Columns"
loanData <- loanData[,!names(loanData) %in% Vague_Columns]

#---- .. Data Consistency .. ----  

# Convert All characters to upper case to maintain consistency in cases
loanData <- data.frame(lapply(loanData, function(x){
  if(is.character(x)){
    return(toupper(x))
  }else{
    return(x)
  }
}),stringsAsFactors = F)

# Check for spelling issues and inconsistency

lapply(loanData,function(x){
  if(is.character(x))
  {
    return(unique(x))
  }
}
) # No spelling issues or other inconsistency

#---- .. Find and Impute NA values .. ----

names(which(lapply(loanData,function(x){sum(is.na(x))}) > 0))

# No need to impute the NA's. NA is correctly assigned

#---- .. Treat Values to be useful for analysis .. ----

# int_rate : Correcting value to have numbers instead of string with %
loanData <- mutate(loanData,int_rate = as.double(strsplit(int_rate,"%")))

# revol_util : Correcting value to have numbers instead of string with %
loanData <- mutate(loanData,revol_util = as.double(strsplit(revol_util,"%")))

# emp_length : converting to numbers by combining <1 years and 1 years and dropping the texts and symbols
# Correct different representations of NA
loanData[which(loanData$emp_length == "n/a"),"emp_length"] <- 0
# Convert to number
loanData <- mutate(loanData,emp_length = as.numeric(str_replace_all(emp_length,"[a-zA-Z\\+\\s</]","")))

# format date

CorrectDate <- function(x){
  x <- paste("01-",x,sep = '')
  yy <- formatC(as.numeric(substr(x,8,9)),width=2,flag=0)
  if(yy<=17){
    x <- paste(substr(x,1,7),"20",as.character(yy),sep = "")
  }else{
    x <- paste(substr(x,1,7),"19",as.character(yy),sep = "")
  }
  
  as.Date(x,"%d-%b-%Y")
}

loanData$issue_d <- as.Date(sapply(loanData$issue_d,CorrectDate),origin = '1970-01-01')
loanData$earliest_cr_line <- as.Date(sapply(loanData$earliest_cr_line,CorrectDate),origin = '1970-01-01')


#---- .. Derived Variables .. ----

# annual_inc_rank : Creating new bucket based on income with bin size 25000. Higher the Level, higher the income.
# Data driven Metric
loanData$annual_inc_level <- NA
loanData$annual_inc_level <- sapply(loanData$annual_inc, function(x){
  return(paste(formatC(ceiling(x/25000),width=3,flag = 0),"L",sep=""))
})

# default_loss : Measure of loss incurred by a person who defaulted on loan
# Business & data driven Metric
loanData <- mutate(loanData,default_loss = (installment*as.numeric(str_replace(term," MONTHS","")))-total_pymnt)
#NA for Fully Paid and Current
loanData[which(loanData$loan_status != "CHARGED OFF"),"default_loss"] <- NA 

# issue_m_name : Loan issued month Name
# Data driven metric
loanData <- mutate(loanData,issue_m_name = format(issue_d,"%b"))

# days_since_first_credit_line : number of days since first credit line. This gives an idea about a person's experience with loan.
# Data driven metric
loanData <- mutate(loanData,days_since_first_credit_line =  Sys.Date() - earliest_cr_line)
loanData$days_since_first_credit_line <- as.numeric(loanData$days_since_first_credit_line)



#======= Data Analysis ========


library(ggplot2)     

#---- .. Univariate Analysis .. ----
