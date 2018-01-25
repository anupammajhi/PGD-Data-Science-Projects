
# load SparkR
library(SparkR)
library(magrittr)
library(ggplot2)

# Initialize spark session
sparkR.session(master='local')

###################################################################################################################
########################################## Data Preparation and Cleaning ##########################################

# Reading the CSV files from S3 bucket
NYCParking_2015 <- SparkR::read.df("s3://nycparkinghari/Parking_Violations_Issued_-_Fiscal_Year_2015.csv",source="csv",header="true",inferSchema="true")
NYCParking_2016 <- SparkR::read.df("s3://nycparkinghari/Parking_Violations_Issued_-_Fiscal_Year_2016.csv",source="csv",header="true",inferSchema="true")
NYCParking_2017 <- SparkR::read.df("s3://nycparkinghari/Parking_Violations_Issued_-_Fiscal_Year_2017.csv",source="csv",header="true",inferSchema="true")

# Examining structure
str(NYCParking_2015) # 51 variables
