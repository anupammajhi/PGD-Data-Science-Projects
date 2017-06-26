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

