


#____________________________________________________________________________
#                           Case Study - TIME SERIES
#____________________________________________________________________________




# Clear All Variables

rm(list=ls())




#Loading required libraries

library(lubridate)
library(cowplot)
library(forecast)
library(tseries)
library(graphics)

#Reading data in
store <- read.csv("Global Superstore.csv")

summary(store$Market)
summary(store$Segment)

length(levels(store$Market))

str(store$Order.Date)



#------------------------
#  DATA CLEANING
#------------------------


#Converting Order.Date to a date time object

store$Order.Date <- as.Date(store$Order.Date, format = '%d-%m-%Y')

store$Date <- floor_date(store$Order.Date, unit = 'month')

