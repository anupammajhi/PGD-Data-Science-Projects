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
  sum(is.na(UberData$Request.id)) #No missing data
  sum(is.na(UberData$Pickup.point)) #No missing data
  
  # Check Similar values
  levels(UberData$Status) #All distinct values, unambiguous, No need to lower case
  levels(UberData$Pickup.point) #All distinct values, unambiguous, No need to lower case

  # Request.id and Pickup.point should not have any missing Values
  sum(is.na(UberData$Request.id)) #No missing values
  sum(is.na(UberData$Pickup.point)) #No missing values

  # Completed Trips should have Both Pickup and Drop Time stamps and Driver ID
