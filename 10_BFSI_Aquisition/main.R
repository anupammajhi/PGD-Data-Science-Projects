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

