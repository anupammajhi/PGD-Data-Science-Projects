# Removing all variables
# rm(list=ls())

#===================================================
# BUSINESS UNDERSTANDING 
#===================================================

# Loading bank marketting data from csv
bank_data<- read.csv("bank_marketing.csv")

str(bank_data)

summary(bank_data)


# Response rate of prospect customers

response <- sum(bank_data$response == "yes")/nrow(bank_data)
response 
# 11.27%


# Checking for missing values in columns of dataset

sapply(bank_data, function(x){ sum(is.na(x)) })
#No NA data found



# Loading ggplot
library(ggplot2)

# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()


#====== Check the outlier in the variables 

# Box plot 

boxplot(bank_data$age)
# Several outliers present above about 70

quantile(bank_data$age,seq(0,1,0.01))
# The age at the 99th percentile is 71
