# Remove all variables
rm(list=ls())

library(dplyr)

# Read CSV File
UberData <- read.csv("Uber Request Data.csv")

#======= Data Cleaning and Preparation ========

# Check duplicate values

  sum(duplicated(UberData$Request.id)) #No Duplicates

# Checking and validating missing values

  # All rows should have Request Time Stamp, Pickup point and Request ID
  sum(is.na(UberData$Request.timestamp)) #No missing data
