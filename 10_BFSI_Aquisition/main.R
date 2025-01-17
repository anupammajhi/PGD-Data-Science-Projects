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

agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age

# Checking the response rate of each age bucket in the plot

ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
# Response rate is very high for age groups below 20 and above 60

# Checking the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)



#===== Variable : Job

# Checking the levels of the job

levels(bank_data$job)


# Plotting bar graph for job variable.

# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate, fill = as.numeric(response_rate))) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_text(size = 3, vjust = -0.5) + 
    labs(x = var_name, fill = "Response Rate") +
    scale_fill_gradient(low = "#ff0000", high = "#00ff00", space = "Lab", na.value = "grey50", guide = "colourbar")
  
}

plot_response(bank_data$job, "job")

# We can see that the response rate is the highest for Students and those that are Retired


# Checking Marital status

#===== Variable : Marital Status

summary(bank_data$marital)

# Assumtion : Replacing Unknown level to married as number is really low

levels(bank_data$marital)[4] <- "married"

# Plotting marital status

plot_response(bank_data$marital,"marital")
# The Respone rate for those that are Single, is slightly higher than the rest


#===== Variable : Education
# Let's see the education variables

plot_response(bank_data$education,"Education")


# Reducing the levels of education variable

levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot

plot_response(bank_data$education,"Education_levels")

# 'Unknown' and 'Tertiary Education' have higher Response Rates

#===== Variable : default

table(bank_data$default)

plot_response(bank_data$default, "Default")

# The "default" variable provides us with no valuable insight. Therefore, we will remove it.

bank_data <- bank_data[,-5]

#===== Variable : housing

summary(bank_data$housing)


plot_response(bank_data$housing, "Housing")

# The response rates are almost similar across all categories

#===== Variable : loan

summary(bank_data$loan)

plot_response(bank_data$loan, "Loan Status")

# The response rates are almost similar across all categories


#===== Variable : Contact

summary(bank_data$contact)

plot_response(bank_data$contact, "Contact")

# Response rate for Cellular communication is significantly higher than those with telephone communication


#===== Variable : month

summary(bank_data$month)

plot_response(bank_data$month, "Month")

# The months of March, December,September and October show significantly higher Response Rates


#===== Variable : day_of_week

summary(bank_data$day_of_week)

plot_response(bank_data$day_of_week, "Day")

# The Response Rate is fairly similar across all the days of the week


#===== Variable : duration

ggplot(bank_data,aes(duration))+geom_histogram()

summary(bank_data$duration)

# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)
Avg_duration

bank_data <- bank_data[,-22]

# We have outliers present in the dataset
# Checking the percentile distribution of duration 

quantile(bank_data$duration,seq(0,1,0.01))


# So, capping the duration seconds at 99% which is 1271.3sec 

bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13

# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

#===== Variable : campaign

# Checking the summary of this variable 
summary(bank_data$campaign)

# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)

quantile(bank_data$campaign,seq(0,1,0.01))

# We can see Outliers.Capping the value at 99% which is 14

bank_data[which(bank_data$campaign>14),]$campaign <- 14

# Plot campaign variable

ggplot(bank_data,aes(campaign))+geom_histogram()


#===== Variable : pdays

#converting to factor type

bank_data$pdays<- as.factor(bank_data$pdays)

# Checking summary

summary(bank_data$pdays)

levels(bank_data$pdays)

# 999 is a valid value, which means this was the first time contact with the customer

# Reducing the levels of this variable to 3.

levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"


# Also,lets see the response rate of each levels. 

plot_response(bank_data$pday,"Pday")
# Response Rate is significantly higher for prospects that have been contacted within 27 days as compared to first time contacts

# Number of prospects under each category

table(bank_data$pdays)

#===== Variable : previous
# "previous" means, number of contacts performed before this campaign

summary(bank_data$previous)
# Max = 7, converting this variable to factor

bank_data$previous <- as.factor(bank_data$previous)

levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"

summary(bank_data$previous)

plot_response(bank_data$previous,"Previous_contacts")

# Those contacts that have been contacted previously more than 3 times display higher Response Rates


#===== Variable : Poutcome

summary(bank_data$poutcome)

plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

# The Response Rate is significantly higher for those prospects,where a previous campaign was successful


#===== Social and Economic Context Attributes

#===== Variable : emp.var.rate
summary(bank_data$emp.var.rate)

# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()

#===== Variable : cons.price.idx
summary(bank_data$cons.price.idx)

# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()

#===== Variable cons.conf.idx
summary(bank_data$cons.conf.idx)
# negative due to market crash

#===== Variable : euribor3m
summary(bank_data$euribor3m)

#===== Variable : nr.employed
summary(bank_data$nr.employed)


#===========================================================================

# Keeping copies of data for different models

bank_data_LR <- bank_data
bank_data_DT <- bank_data
bank_data_RF <- bank_data

#===========================================================================




#===================================================
# MODEL BUILDING 
#===================================================


#=============== Logistic Regression ===============

# Loading libraries
library(caret)
library(caTools)
library(dummies)

# Removing binning variables 

bank_data_LR <- bank_data_LR[, -21]
summary(bank_data_LR)

# Removing duration column

duration <- data.frame(bank_data_LR$duration)

bank_data_LR <-bank_data_LR[, -10]
summary(bank_data_LR)

# Creating dummy variables

bank_data_LR$response <- as.integer(bank_data_LR$response)

bank_data_LR <- dummy.data.frame(bank_data_LR)

# Converting response to factor with yes and no values

bank_data_LR$response <- as.factor(ifelse(bank_data_LR$response == 1, "yes", "no"))
summary(bank_data_LR)

# Dividing into test and train datasets

set.seed(100)

split_indices_LR <- sample.split(bank_data_LR$response, SplitRatio = 0.70)

train_LR <- bank_data_LR[split_indices_LR, ]
test_LR <- bank_data_LR[!split_indices_LR, ]

nrow(train_LR)/nrow(bank_data_LR) #70%
nrow(test_LR)/nrow(bank_data_LR) #30%

# Loading libraries
library(MASS)
library(car)

logistic_1 <- glm(response ~ ., family = "binomial", data = train_LR)

summary(logistic_1)
# AIC: 15907


# Using StepAIC to remove insignificant variables

logistic_2 <- stepAIC(logistic_1, direction = "both")

summary(logistic_2)
# AIC: 15876

sort(vif(logistic_2), decreasing = T)

# euribor3m has high VIF and low significance, hence collinearity is high, removing  this variable

logistic_3 <- glm(response ~ age + jobretired + loanno + contactcellular + monthaug + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + previousLess_than_3_times + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_3)
# AIC: 15879

sort(vif(logistic_3), decreasing = T)


# nr.employed has high VIF and low significance, hence collinearity is high, removing  this variable

logistic_4 <- glm(response ~ age + jobretired + loanno + contactcellular + monthaug + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + previousLess_than_3_times + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_4)
# AIC: 15895

sort(vif(logistic_4), decreasing = T)


# previousLess_than_3_times has high VIF and low significance, hence collinearity is high, removing  this variable

logistic_5 <- glm(response ~ age + jobretired + loanno + contactcellular + monthaug + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_5)
# AIC: 15897

sort(vif(logistic_5), decreasing = T)

# cons.price.idx has high VIF but is also very significant.
# monthaug has very low significance, removing  this variable

logistic_6 <- glm(response ~ age + jobretired + loanno + contactcellular + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    day_of_weekthu + day_of_weektue + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_6)
# AIC: 15898

sort(vif(logistic_6), decreasing = T)


# day_of_weekthu has very low significance, removing  this variable

logistic_7 <- glm(response ~ age + jobretired + loanno + contactcellular + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    day_of_weektue + campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_7)
# AIC: 15899

sort(vif(logistic_7), decreasing = T)


# day_of_weektue has very low significance, removing  this variable

logistic_8 <- glm(response ~ age + jobretired + loanno + contactcellular + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_8)
# AIC: 15898

sort(vif(logistic_8), decreasing = T)


# educationTertiary_Education has very low significance, removing  this variable

logistic_9 <- glm(response ~ age + jobretired + loanno + contactcellular + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_9)
# AIC: 15899

sort(vif(logistic_9), decreasing = T)


# loanno has very low significance, removing  this variable

logistic_10 <- glm(response ~ age + jobretired + contactcellular + monthdec + 
                    monthjun + monthmar + monthmay + monthnov + day_of_weekfri + day_of_weekmon + 
                    campaign + pdaysContacted_in_first_10days + 
                    pdaysContacted_after_10days + poutcomefailure + 
                    emp.var.rate + cons.price.idx + cons.conf.idx + 
                    `jobblue-collar` + jobservices, 
                  family = "binomial", data = train_LR)

summary(logistic_10)
# AIC: 15900

sort(vif(logistic_10), decreasing = T)


# day_of_weekfri has very low significance , removing  this variable

logistic_11 <- glm(response ~ age + jobretired + contactcellular + monthdec + 
                     monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + 
                     emp.var.rate + cons.price.idx + cons.conf.idx + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train_LR)

summary(logistic_11)
# AIC: 15903

sort(vif(logistic_11), decreasing = T)


# monthdec has very low significance , removing  this variable

logistic_12 <- glm(response ~ age + jobretired + contactcellular +
                     monthjun + monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + 
                     emp.var.rate + cons.price.idx + cons.conf.idx + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train_LR)

summary(logistic_12)
# AIC: 15905

sort(vif(logistic_12), decreasing = T)


# jobretired has very low significance , removing  this variable

logistic_13 <- glm(response ~ age + contactcellular + monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + 
                     emp.var.rate + cons.price.idx + cons.conf.idx + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train_LR)

summary(logistic_13)
# AIC: 15908

sort(vif(logistic_13), decreasing = T)

# age has very low significance , removing  this variable

logistic_14 <- glm(response ~ contactcellular + monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + 
                     emp.var.rate + cons.price.idx + cons.conf.idx + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train_LR)

summary(logistic_14)
# AIC: 15908

sort(vif(logistic_14), decreasing = T)


# jobservices  has very low significance , removing  this variable

logistic_15 <- glm(response ~ contactcellular + monthjun + 
                     monthmar + monthmay + monthnov + day_of_weekmon + 
                     campaign + pdaysContacted_in_first_10days + 
                     pdaysContacted_after_10days + poutcomefailure + 
                     emp.var.rate + cons.price.idx + cons.conf.idx + 
                     `jobblue-collar`, 
                   family = "binomial", data = train_LR)

summary(logistic_15)
# AIC: 15914

sort(vif(logistic_15), decreasing = T)

# All the variables are significant now


final_LR_model <- logistic_15




#===================================================
# MODEL EVALUATION
#===================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(e1071)
library(ROCR)
library(DAAG)

# predicting response probability based on final model
prediction_test_LR <- predict(final_LR_model,type="response", newdata = test_LR[,-60])

summary(prediction_test_LR)

test_LR$prob <- prediction_test_LR


#===== Accuracy, Sensitivity and Specificity

prediction_test_LR_response <- factor(ifelse(prediction_test_LR >= 0.50, "yes", "no"))

# Comparing prediction with actual value in test data
table(test_LR$response,prediction_test_LR_response)

# Confusion matrix
confusion_LR <- confusionMatrix(prediction_test_LR_response,test_LR$response,positive = "yes")
confusion_LR

# Accuracy = 0.898
# Sensitivity = 0.21480
# Specificity = 0.98477

# Choosing cutoff
perform_fn <- function(cutoff) 
{
  test_pred_resp <- factor(ifelse(prediction_test_LR >= cutoff, "yes", "no"))
  conf <- confusionMatrix(test_pred_resp, test_LR$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from 0.01 to 0.5

s = seq(.01,.5,length=100)
OUT = matrix(0,100,3)

for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Calculating cutoff where we have similar accuracy and sensitivity
cutoff <- s[which(abs(OUT[,1]-OUT[,2]) < 0.03)]
cutoff 
# 0.07434

test_LR_cutoff_response <- factor(ifelse(prediction_test_LR >= cutoff, "yes", "no"))


confusion_LR_final <- confusionMatrix(test_LR_cutoff_response, test_LR$response, positive = "yes")

acc <- confusion_LR_final$overall[1]
sens <- confusion_LR_final$byClass[1]
spec <- confusion_LR_final$byClass[2]

acc
# 0.7249

sens
# 0.7069

spec
# 0.7272



# Checking model accuracy with k-fold cross verification

cv.binary(final_LR_model, nfolds = 100)

# Internal estimate of accuracy = 0.901
# Cross-validation estimate of accuracy = 0.901


# KS Statistic

test_LR_cutoff_response <- ifelse(test_LR_cutoff_response == "yes", 1, 0)
test_LR$response <- ifelse(test_LR$response == "yes", 1, 0)

pred_LR_object_test <- prediction(test_LR_cutoff_response, test_LR$response)
performance_LR_measures_test <- performance(pred_LR_object_test, "tpr", "fpr")  

ks_table_test <- attr(performance_LR_measures_test, "y.values")[[1]] - (attr(performance_LR_measures_test, "x.values")[[1]])

max(ks_table_test)
# 0.4341

plot(performance_LR_measures_test,main=paste0(' KS=',round(max(ks_table_test*100,1)),'%'), colorize = T)
lines(x=c(0,1),y=c(0,1))



# Area under the curve
auc <- performance(pred_LR_object_test, "auc")
auc@y.values[[1]] 
# 0.717



#===== Lift and Gain

# performance_measures_test <- performance(pred_LR_object_test, "lift", "rpp")
# plot(performance_LR_measures_test, main = 'Lift', colorize = T)

lift <- function(labels,predicted_prob,groups=10){
  if(is.factor(labels)){labels <- as.integer(as.character(labels))}
  if(is.factor(predicted_prob)){predicted_prob <- as.integer(as.character(predicted_prob))}
  helper = data.frame(cbind(labels,predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"],groups)
  gaintable = helper %>% group_by(bucket) %>%
    summarise_at(vars(labels ),funs(total = n(),totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}


aquisition_decile = lift(test_LR$response, prediction_test_LR, groups = 10)
View(aquisition_decile)


# Lift Chart

plot(aquisition_decile$bucket, aquisition_decile$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket",  main = 'Lift Chart')




#===== Checkpoint 3

# Create a data frame with the variables
# prospect ID, actual response, predicted response, predicted probability of response, duration of call in seconds, and cost of call

# Using complete dataset

bank_prob_LR <- predict(final_LR_model, type = "response", newdata = bank_data_LR[,-60])
bank_pred_resp_LR <- factor(ifelse(bank_prob_LR >= cutoff, "yes", "no"))
prospect_ID <- seq(1:nrow(bank_data_LR))
final_all_LR <- cbind(prospect_ID, bank_data_LR$response, bank_pred_resp_LR, bank_prob_LR, duration)
colnames(final_all_LR) <- c('prospect_ID','Actual_Response','Predicted_Response','Predicted_Probability_of_Response','Duration')

# It's important to mention here that the variable 'Duration' had outliers, 
# which were capped during the data preparation and EDA phase

final_all_LR$Cost <- (0.033*final_all_LR$Duration) + 0.8



#===== Checkpoint 4

# Find the number of top X% prospects you should target to meet the business objective
# Report the average call duration for targeting the top X% prospects to the CMO (report this as a comment in the R file)

final_all_LR$Actual_Response_num <- ifelse(final_all_LR$Actual_Response == "yes",1,0) 

#using lift function created earlier

aquisition_decile_all = lift(final_all_LR$Actual_Response_num, final_all_LR$Predicted_Probability_of_Response, groups = 10)
View(aquisition_decile_all)

# Lift Chart
plot(aquisition_decile_all$bucket, aquisition_decile_all$Cumlift, type="l", ylab="Cumulative lift", xlab="Bucket",  main = 'Lift Chart')

# 5th Decile has a total gain of 81%
# Hence to achieve a 80% of total responders at minimum cost, bank needs to call first 50% of the prospects.

# Calculating average call duration 

# Sorting based on probability of response
Prospects_ordered_probablity <- final_all_LR[order(-(final_all_LR$Predicted_Probability_of_Response)),]

# Taking 5th decile / 50% of total prospects
prospects_to_call <- Prospects_ordered_probablity[1:(nrow(Prospects_ordered_probablity)*0.50),]

# Average call duration for targetting prospects
mean(prospects_to_call$Duration)
# (267 seconds) or (4 mins and 27 seconds)

# Total cost for 50% prospects
sum(prospects_to_call$Cost)
# Rs. 1,97,923.4

# Average cost per call for these prospects
mean(prospects_to_call$Cost)
# Rs. 9.61 per call on average

#===== Checkpoint 5

# Create a lift chart
# The x-axis contains the number of prospects contacted 
# the y-axis contains the ratio: response rate using the model/ response rate without using the model

plot(((aquisition_decile_all$bucket)/10)*nrow(final_all_LR), aquisition_decile_all$Cumlift, type="l",  xlab="Total people Called", ylab="Response rate ratio",  main = 'Lift Chart')
