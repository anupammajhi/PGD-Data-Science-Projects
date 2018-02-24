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

# Capping the upper value of age at 71

bank_data[(which(bank_data$age > 71)),]$age <- 71

# Binning the age variable and storing it in variable "binning.age"

bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Changing the response value to numbers i.e yes = 1 and no = 0

bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Checking the numeric value of response rate in each bucket

agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect

count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
