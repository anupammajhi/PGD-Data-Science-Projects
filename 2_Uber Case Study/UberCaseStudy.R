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
    UberData$POSIXRequestTime <- as.POSIXct(NA)
    
    # Update POSIXct values to new column, hence standardising Date Time using respective identified format
    UberData$POSIXRequestTime[slashDates_Request] <- as.POSIXct(UberData$Request.timestamp[slashDates_Request],format = '%d/%m/%Y %H:%M')
    UberData$POSIXRequestTime[hyphenDates_Request] <- as.POSIXct(UberData$Request.timestamp[hyphenDates_Request],format = '%d-%m-%Y %H:%M:%S')
    
    # Check for any missing values
    which(is.na(UberData$POSIXRequestTime)) #No missing Values
    
    
    
    #------ .. CORRECTING DROP TIME ------
    
    # Identifying rows with Date format %d/%m/%Y %H:%M using regex
    slashDates_Drop <- grep(pattern = '^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{1,2}$',x=UberData$Drop.timestamp)
    
    # Identifying rows with Date format %d-%m-%Y %H:%M:%S using regex
    hyphenDates_Drop <- grep(pattern = '^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4} [0-9]{1,2}:[0-9]{1,2}:[0-9]{1,2}$',x=UberData$Drop.timestamp)
    
    # Check for missed format by taking count of identified patterns in previous step as well as NA in source column and compare with dataset
    length(slashDates_Drop) + length(hyphenDates_Drop) + sum(is.na(UberData$Drop.timestamp)) == length(UberData$Drop.timestamp) #TRUE , Hence No other date formats
    
    # New POSIXct column names POSIXRequestTime
    UberData$POSIXDropTime <- as.POSIXct(NA)
    
    # Update POSIXct values to new column, hence standardising Date Time
    UberData$POSIXDropTime[slashDates_Drop] <- as.POSIXct(UberData$Drop.timestamp[slashDates_Drop],format = '%d/%m/%Y %H:%M')
    UberData$POSIXDropTime[hyphenDates_Drop] <- as.POSIXct(UberData$Drop.timestamp[hyphenDates_Drop],format = '%d-%m-%Y %H:%M:%S')
    
    # Removing unwanted Columns Request.timestamp and Drop.timestamp as new columns created with POSIXct
    UberData <- UberData[,c(1:4,7:8)]
    
    # Deriving Columns
    UberData$HourOfTheDay <- NA # New Column to identify Which Clock Hour of the Day the request was made
    UberData$HourOfTheDay <- as.factor(format(UberData$POSIXRequestTime,"%H"))
    
    UberData$PartOfDay <- NA # New Column to identify Which Part of the Day the request was made
    UberData$PartOfDay <- sapply(as.numeric(as.vector(UberData$HourOfTheDay)),function(x){
      if(x %in% c(4,5,6)){
        return('Early Morning')
      }else if(x %in% c(7,8,9)){
        return('Morning')
      }else if(x %in% c(10,11)){
        return('Late Morning')
      }else if(x %in% c(12,13,14)){
        return('Afternoon')
      }else if(x %in% c(15,16,17)){
        return('Early Evening')
      }else if(x %in% c(18,19,20)){
        return('Late Evening')
      }else if(x %in% c(21,22,23)){
        return('Night')
      }else if(x %in% c(0,1,2,3)){
        return('Late Night')
      }else{
