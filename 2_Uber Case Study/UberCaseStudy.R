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
  CompletedTrips <- UberData[which(UberData$Status == "Trip Completed"),]
  sum(is.na(CompletedTrips$Drop.timestamp)) #All Timestamps present
  sum(is.na(CompletedTrips$Driver.id)) #All Timestamps present
  
  # Cancelled Trips should have Pickup Time stamps and Driver ID
  CancelledTrips <- UberData[which(UberData$Status == "Cancelled"),]
  sum(is.na(CancelledTrips$Request.timestamp)) #All Timestamps present
  sum(is.na(CancelledTrips$Driver.id)) #All Timestamps present


# COLUMN Request.id : OK
# COLUMN Pickup.point : OK. 2 unambiguous levels. No issues. Not required to lowercase.
# COLUMN Driver.id : OK
# COLUMN Status : OK. 3 Unambiguous levels. No issues. Not required to lowercase.
# COLUMN Request.timestamp and Drop.timestamp
    # Should be a date vector
  

    # NOT USING LUBRIDATE LIBRARY, TRYING TO DO WITH R INBUILT FUNCTION
  
    
    #------ .. CORRECTING REQUEST TIME -------
    
    # Identifying rows with Date format %d/%m/%Y %H:%M using regex
    slashDates_Request <- grep(pattern = '^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$',x=UberData$Request.timestamp)
    
    # Identifying rows with Date format %d-%m-%Y %H:%M:%S using regex
    hyphenDates_Request <- grep(pattern = '^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$',x=UberData$Request.timestamp)
    
    # Check for missed format by taking count of identified patterns in previous step and compare with dataset
    length(slashDates_Request) + length(hyphenDates_Request) == length(UberData$Request.timestamp) #TRUE , Hence No other date formats
  
    # New POSIXct column named POSIXRequestTime
