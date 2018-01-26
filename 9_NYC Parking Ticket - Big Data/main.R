
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
str(NYCParking_2016) # 51 variables
str(NYCParking_2017) # 43 variables

#In 2017 Data Following headers are missing
# => Latitude
# => Longitude
# => Community Board
# => Community Council
# => Census Tract
# => BIN
# => BBL
# => NTA

# Number of rows each dataframe
nrow(NYCParking_2015) #11809233
nrow(NYCParking_2016) #10626899
nrow(NYCParking_2017) #10803028

# Looking for duplicate rows (only retaining distinct rows)
nrow(distinct(NYCParking_2015)) #10951257 - Hence there are duplicate rows
NYCParking_2015 <- distinct(NYCParking_2015) # Removing duplicates

nrow(distinct(NYCParking_2016)) #10626899 - No duplicate rows
nrow(distinct(NYCParking_2017)) #10803028 - No duplicate rows

# Sampling of each dataset and converting it to R Data Frames. This will help us analyse the data and understand what cleaning needs to be performed.

NYCParking_2015_sample <- sample(NYCParking_2015,withReplacement = F,fraction = 0.01)
NYCParking_2015_sample_R <- collect(NYCParking_2015_sample)

NYCParking_2016_sample <- sample(NYCParking_2016,withReplacement = F,fraction = 0.01) 
NYCParking_2016_sample_R <- collect(NYCParking_2016_sample)

NYCParking_2017_sample <- sample(NYCParking_2017,withReplacement = F,fraction = 0.01) 
NYCParking_2017_sample_R <- collect(NYCParking_2017_sample)

# Checking columns for number of missing values

sapply(NYCParking_2015_sample_R, function(x) sum(is.na(x)))
sapply(NYCParking_2016_sample_R, function(x) sum(is.na(x)))
sapply(NYCParking_2017_sample_R, function(x) sum(is.na(x)))

# We can see that columns from 'No Standing or Stopping Violation' are empty in all three datasets.
# Therefore, we need to remove them
# Hence we will only use columns 1 to 40

NYCParking_2015 <- NYCParking_2015[,1:40]
columns(NYCParking_2015)

NYCParking_2016 <- NYCParking_2016[,1:40]
columns(NYCParking_2016)

NYCParking_2017 <- NYCParking_2017[,1:40]
columns(NYCParking_2017)

# Adding ParsedIssue Date and a column for Fiscal Year to combine all 3 years data

NYCParking_2015 <- NYCParking_2015 %>% withColumn("Issue Date Parsed", to_date(NYCParking_2015$`Issue Date`,  "MM/dd/yyyy")) %>% 
  withColumn("Fiscal Year", "2015")

str(NYCParking_2015)

NYCParking_2016 <- NYCParking_2016 %>% withColumn("Issue Date Parsed", to_date(NYCParking_2016$`Issue Date`,  "MM/dd/yyyy")) %>% 
  withColumn("Fiscal Year", "2016")

str(NYCParking_2016)

NYCParking_2017 <- NYCParking_2017 %>% withColumn("Issue Date Parsed", to_date(NYCParking_2017$`Issue Date`,  "MM/dd/yyyy")) %>% 
  withColumn("Fiscal Year", "2017")

