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
