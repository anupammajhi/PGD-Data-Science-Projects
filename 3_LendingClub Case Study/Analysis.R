#Remove all variables
rm(list=ls())

library(gdata)
library(magrittr)
library(dplyr)
library(stringr)

# Unzip compressed file. Not required if already extracted
# unzip("loan.zip")

# Read CSV File
loanData <- read.csv("loan.csv",na.strings=c("","NA"),stringsAsFactors = F) #Also replaces blanks with NA


#======= Data Cleaning and Preparation ========

# ---- .. Remove Irrelevant Data .. ----

# Get rid of columns with all NA values

# By comparing total num of NA in a column with total num of rows in dataframe
loanData <- loanData[,colSums(is.na(loanData)) != nrow(loanData)]

# Get rid of rows with all NA values  

# By comparing total num of NA in a row with total num of rows in dataframe
loanData <-loanData[rowSums(is.na(loanData)) != ncol(loanData),]

# Getting rid of unnecessary columns
# Columns that have all same values or just have one value and NA. 
# Such Columns do not contribute to any valuable analysis. Hence remove them.
# Columns with just one level when treated as factor means they don't have enough values to analyse
# NOTE : unique() function not used as it considers NA as a unique value too
Vague_Columns <- names(which(lapply(loanData,function(x){length(levels(as.factor(x)))}) == 1))
Vague_Columns

# url: no new data that can be obtained
Vague_Columns <- append(Vague_Columns,"url")

# id and member_id
loanData[which(duplicated(loanData$id)),] # 0 Duplicates, hence unique
loanData[which(duplicated(loanData$member_id)),]  # 0 Duplicates, hence unique
# Since both id and member_id are unique, we will exclude member_id

# funded_amnt_inv, out_prncp_inv, total_pymnt_inv : Not required for this analysis as we are not analysing investor's action
# desc, purpose and title : purpose is a better categorical variable for this study, as desc and title are quite random. Hence just use purpose and remove others.
# emp_title : not relevant for this analysis
Vague_Columns <- append(Vague_Columns,c("member_id","funded_amnt_inv","out_prncp_inv","total_pymnt_inv",
                                        "desc","title","emp_title"))
# last_credit_pull_d,next_pymnt_d,last_pymnt_amnt,last_pymnt_d,collection_recovery_fee,recoveries,out_prncp : not relevant for this analysis, as these variables are helpful only post default or full payment
Vague_Columns <- append(Vague_Columns,c("last_credit_pull_d","next_pymnt_d","last_pymnt_amnt","last_pymnt_d","collection_recovery_fee","recoveries","out_prncp"))

# Removing unnecessary columns from dataframe saved as list "Vague_Columns"
loanData <- loanData[,!names(loanData) %in% Vague_Columns]

#---- .. Data Consistency .. ----  

# Convert All characters to upper case to maintain consistency in cases
loanData <- data.frame(lapply(loanData, function(x){
  if(is.character(x)){
    return(toupper(x))
  }else{
    return(x)
  }
}),stringsAsFactors = F)

# Check for spelling issues and inconsistency

lapply(loanData,function(x){
  if(is.character(x))
  {
    return(unique(x))
  }
}
) # No spelling issues or other inconsistency

#---- .. Find and Impute NA values .. ----

names(which(lapply(loanData,function(x){sum(is.na(x))}) > 0))

# No need to impute the NA's. NA is correctly assigned

#---- .. Treat Values to be useful for analysis .. ----

# int_rate : Correcting value to have numbers instead of string with %
loanData <- mutate(loanData,int_rate = as.double(strsplit(int_rate,"%")))

# revol_util : Correcting value to have numbers instead of string with %
loanData <- mutate(loanData,revol_util = as.double(strsplit(revol_util,"%")))

# emp_length : converting to numbers by combining <1 years and 1 years and dropping the texts and symbols
# Correct different representations of NA
loanData[which(loanData$emp_length == "n/a"),"emp_length"] <- 0
# Convert to number
loanData <- mutate(loanData,emp_length = as.numeric(str_replace_all(emp_length,"[a-zA-Z\\+\\s</]","")))

# format date

CorrectDate <- function(x){
  x <- paste("01-",x,sep = '')
  yy <- formatC(as.numeric(substr(x,8,9)),width=2,flag=0)
  if(yy<=17){
    x <- paste(substr(x,1,7),"20",as.character(yy),sep = "")
  }else{
    x <- paste(substr(x,1,7),"19",as.character(yy),sep = "")
  }
  
  as.Date(x,"%d-%b-%Y")
}

loanData$issue_d <- as.Date(sapply(loanData$issue_d,CorrectDate),origin = '1970-01-01')
loanData$earliest_cr_line <- as.Date(sapply(loanData$earliest_cr_line,CorrectDate),origin = '1970-01-01')


#---- .. Derived Variables .. ----

# annual_inc_rank : Creating new bucket based on income with bin size 25000. Higher the Level, higher the income.
# Data driven Metric
loanData$annual_inc_level <- NA
loanData$annual_inc_level <- sapply(loanData$annual_inc, function(x){
  return(paste(formatC(ceiling(x/25000),width=3,flag = 0),"L",sep=""))
})

# default_loss : Measure of loss incurred by a person who defaulted on loan
# Business & data driven Metric
loanData <- mutate(loanData,default_loss = (installment*as.numeric(str_replace(term," MONTHS","")))-total_pymnt)
#NA for Fully Paid and Current
loanData[which(loanData$loan_status != "CHARGED OFF"),"default_loss"] <- NA 

# issue_m_name : Loan issued month Name
# Data driven metric
loanData <- mutate(loanData,issue_m_name = format(issue_d,"%b"))

# days_since_first_credit_line : number of days since first credit line. This gives an idea about a person's experience with loan.
# Data driven metric
loanData <- mutate(loanData,days_since_first_credit_line =  Sys.Date() - earliest_cr_line)
loanData$days_since_first_credit_line <- as.numeric(loanData$days_since_first_credit_line)



#======= Data Analysis ========


library(ggplot2)     

#---- .. Univariate Analysis .. ----

#---- .... Unordered Categorical Variables .... ----      

# home_ownership summary
summary(as.factor(loanData$home_ownership))

# plot for home_ownership
loanData %>%
  ggplot(aes(x=home_ownership)) + 
  geom_bar() +
  geom_text(aes(y= ..count.., label = ..count..),stat="count",vjust=-0.5)+
  coord_cartesian(ylim = c(0, 20000)) +
  labs(x = "Home Ownership", 
       y="Count", 
       title = "Home Ownership Frequency")
# people staying in rented houses tend to take more loans, followed by people having morgaged house


# verification_status of income of the borrower summary
summary(as.factor(loanData$verification_status))

# plot for verification_status
loanData %>%
  ggplot(aes(x=verification_status)) + 
  geom_bar() +
  geom_text(aes(y= ..count.., label = ..count..),stat="count",vjust=-0.5)+
  coord_cartesian(ylim = c(0, 18000)) +
  labs(x = "Verification Status", 
       y="Count", 
       title = "Verification Status Frequency")
#maximum people don't have verified income


# purpose : reason the borrower is taking loan
summary(as.factor(loanData$purpose))

# plot for purpose
loanData %>%
  ggplot(aes(x=reorder(purpose,purpose,function(x)length(x)),y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label =..count..),stat="count",vjust=-0.5)+
  coord_cartesian(ylim = c(0, 20000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_line(aes(y=..count..,group = 1),stat="count",color="blue",size=1.2)+
  labs(x = "Purpose", 
       y="Count", 
       title = "Purpose Rank-Frequency")
#Debt consolidation and credit card are the top purposes for taking loan


# addr_state : borrower's address state : summary
summary(as.factor(loanData$addr_state))

# plot for addr_state
loanData %>%
  ggplot(aes(x=reorder(addr_state,addr_state,function(x)length(x)),y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label =..count..),stat="count",hjust=-0.3,angle=90,size=3.2)+
  coord_cartesian(ylim = c(0, 8000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  geom_line(aes(y=..count..,group = 1),stat="count",color="blue",size=1.2,alpha=0.4)+
  labs(x = "State", 
       y="Count", 
       title = "State Rank-Frequency")
# CA, NY, FL, TX, NJ are the top customers for loan



#---- .... Ordered Categorical Variables .... ----   

#issue_m_name : Loan issue month name irrespective of year
loanData$issue_m_name <- factor(loanData$issue_m_name,levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
summary(loanData$issue_m_name)

loanData %>%
  ggplot(aes(x=issue_m_name,y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label =..count..),stat="count",hjust=-0.3,angle=90,size=3.2)+
  coord_cartesian(ylim = c(0, 8000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  geom_line(aes(y=..count..,group = 1),stat="count",color="blue",size=1.2,alpha=0.4)+
  labs(x = "Month", 
       y="Count", 
       title = "Month Time-Frequency")
# There is an increasing trend of taking loan over time


#issue_d : Loan issue month with year
summary(loanData$issue_d)

#plot for issue_d
loanData %>%
  ggplot(aes(x=issue_d,y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label =..count..),stat="count",hjust=-0.3,angle=90,size=3.2)+
  coord_cartesian(ylim = c(0, 2500)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  geom_line(aes(y=..count..,group = 1),stat="count",color="blue",size=1.2,alpha=0.4)+
  labs(x = "Year", 
       y="Count", 
       title = "Time-Frequency")
# There is an increasing trend of borrowing loan over time
# there was a drop in mid-2008, probably because of market crash


#annual_inc_level : level of annual income categorised in buckets of 25000
summary(as.factor(loanData$annual_inc_level))

#plot for annual_inc_level
loanData %>%
  ggplot(aes(x=annual_inc_level,y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label =..count..),stat="count",hjust=-0.3,angle=90,size=3.2)+
  coord_cartesian(ylim = c(0, 15000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  geom_line(aes(y=..count..,group = 1),stat="count",color="blue",size=1.2,alpha=0.4)+
  labs(x = "Annual Income Level", 
       y="Count", 
       title = "Freuency of Loans based on Income")
#Higher the salary bracket, people tend to take less loans


#loan_status : overall status of loan
summary(as.factor(loanData$loan_status))

#plot for loan_status
loanData %>%
  ggplot(aes(x=loan_status,y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))),stat="count",vjust=-0.3,size=4)+
  coord_cartesian(ylim = c(0, 35000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  labs(x = "Loan Status", 
       y="Count", 
       title = "Loan Status")
#14.2% of loans have defaulted and contributed to loss.


#grade : grade of loan based on mainly interest rate
summary(as.factor(loanData$grade))

#plot for grade
loanData %>%
  ggplot(aes(x=grade,y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..))),stat="count",vjust=-0.3,size=4)+
  coord_cartesian(ylim = c(0, 15000)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  labs(x = "Grade", 
       y="Count", 
       title = "Grade Frequency - Based on Interest rate")
# Higher the grade of loan, the number of loans go down


#sub-grade : further drilled down grade
summary(as.factor(loanData$sub_grade))

#plot for sub-grade
loanData %>%
  ggplot(aes(x=sub_grade,y=..count..),stat="count") + 
  geom_bar(fill="#aaaaaa") +
  geom_text(aes(label = (..count..)),stat="count",hjust=-0.1,size=3,angle = 90)+
  coord_cartesian(ylim = c(0, 3500)) +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1))+
  labs(x = "Sub-Grade", 
       y="Count", 
       title = "Sub-Grade Frequency - Based on Interest rate")
# Higher the sub-grade of loan, the number of loans go down


#term : tenure of the loan
summary(as.factor(loanData$term))

#plot for term
loanData %>%
  ggplot(aes(x=term)) + 
  geom_bar() +
  geom_text(aes(y= ..count.., label = ..count..),stat="count",vjust=-0.5)+
  coord_cartesian(ylim = c(0, 30000)) +
  labs(x = "Term", 
       y="Count", 
       title = "Term Frequency") 
#36 months term is more popular


#delinq_2yrs : In 2 years, number of delinquences by borrower
summary(as.factor(loanData$delinq_2yrs))

#plot for delinq_2yrs
loanData %>%
  ggplot(aes(x=delinq_2yrs)) + 
  geom_bar() +
  geom_text(aes(y= ..count.., label = ..count..),stat="count",vjust=-0.5)+
  coord_cartesian(ylim = c(0, 40000)) +
  labs(x = "Delinquencies in last 2 years", 
       y="Count", 
       title = "Delinquencies in last 2 years") 
#most people have low or no delinquencies in last 2 years


#emp_length : number of years employed
summary(as.factor(loanData$emp_length))

loanData %>%
  ggplot(aes(x=emp_length)) + 
  geom_bar() +
  geom_text(aes(y= ..count.., label = ..count..),stat="count",vjust=-0.5)+
  coord_cartesian(ylim = c(0, 30000)) +
  labs(x = "Employment Length", 
       y="Count", 
       title = "Employment Length") 
#Higher the employment length lower probability to take loans
# Employment length 10 include people with more than 10 years employement length. Hence, we see a spike.



#---- .... Quantitative Variables .... ----          

#Function to calculate median and quantiles based on loan status
Loan_Status_Summary <- function(x){
print("FULLY PAID")
print(summary(loanData[which(loanData$loan_status == "FULLY PAID"),x]))
print("CHARGED OFF")
print(summary(loanData[which(loanData$loan_status == "CHARGED OFF"),x]))
print("CURRENT")
print(summary(loanData[which(loanData$loan_status == "CURRENT"),x]))
}


# Loan Status based on Loan Amount requested
Loan_Status_Summary("loan_amnt")

loanData %>%
  ggplot(aes(y=loan_amnt)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
# Loan requests of higher amount have more chances to default. Both median and mean loan amount is high for defaulted loans.



# Loan Status based on Loan Amount funded by lender
Loan_Status_Summary("funded_amnt")

loanData %>%
  ggplot(aes(y=funded_amnt)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
# Loan funded with higher amount have more chances to default. Both median and mean loan amount is high for defaulted loans.



# Loan status based on interest rate
Loan_Status_Summary("int_rate")

loanData %>%
  ggplot(aes(y=int_rate)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
# Loan funded with higher int_rate have more chances to default. Both median and mean loan amount is high for defaulted loans.


# Loan status based on monthly installment 
Loan_Status_Summary("installment")

loanData %>%
  ggplot(aes(y=installment)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
# Loan funded with higher monthly intallment have more chances to default. Both median and mean loan amount is high for defaulted loans.


# Loan status based on monthly debt to income ratio. Higher dti means higher debt than income 
Loan_Status_Summary("dti")

loanData %>%
  ggplot(aes(y=dti)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
# Loan funded with higher dti have more chances to default. Both median and mean loan amount is high for defaulted loans.


# loan status based on delinq_2yrs
Loan_Status_Summary("delinq_2yrs")

loanData %>%
  ggplot(aes(y=delinq_2yrs)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
#no significant result. There are only few outliers with high number of delinquencies


#revol_bal : amount of balance revolving after certain amount is repayed.
Loan_Status_Summary("revol_bal")

loanData %>%
  ggplot(aes(y=revol_bal)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
#Not very significant, but borrowers having more revolving balance tend to default slightly more.


#revol_util : percentage utilization of revolving balance
Loan_Status_Summary("revol_util")

loanData %>%
  ggplot(aes(y=revol_util)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
#higher the utilization of revolving balance, the borrower tends to default much more


Loan_Status_Summary("total_acc")

loanData %>%
  ggplot(aes(y=total_acc)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
#no significant result.



Loan_Status_Summary("total_pymnt")

loanData %>%
  ggplot(aes(y=total_pymnt)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
# Since the defaulted loans have less amount paid, they will be low in the scale.


Loan_Status_Summary("days_since_first_credit_line")



loanData %>%
  ggplot(aes(y=days_since_first_credit_line)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
#nothing significant


Loan_Status_Summary("annual_inc")

loanData %>%
  ggplot(aes(y=annual_inc)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)
#Need to treat outliers



#Replacing Outliers
qnt <- quantile(loanData$annual_inc, probs=c(.25, .75), na.rm = T)
caps <- quantile(loanData$annual_inc, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(loanData$annual_inc, na.rm = T)
loanData$annual_inc[loanData$annual_inc < (qnt[1] - H)] <- caps[1]
loanData$annual_inc[loanData$annual_inc > (qnt[2] + H)] <- caps[2]

Loan_Status_Summary("annual_inc")

loanData %>%
  ggplot(aes(y=annual_inc)) +
  geom_boxplot(aes(x=loan_status),width=0.6)+
  stat_summary(geom="text", fun.y=quantile,aes(x=loan_status,label=sprintf("%1.1f", ..y..)),position=position_nudge(x=0.2), size=3.5,vjust = -0.5)



#---- .. Bivariate Analysis .. ----

# Correlation among variables

continuous_vars <- loanData[,c("loan_amnt","funded_amnt","int_rate","installment","annual_inc","dti","inq_last_6mths","open_acc","pub_rec","revol_bal","total_acc","total_pymnt","total_rec_prncp","total_rec_int","days_since_first_credit_line")]

cormat <- round(cor(continuous_vars),2)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- get_upper_tri(cormat)

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

library(reshape2)
cormat <- reorder_cormat(cormat)

upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



#annual_inc vs funded_amnt with status 
loanData %>%
 ggplot(aes(x =annual_inc, y = funded_amnt)) + geom_point(aes(color = loan_status), alpha = 0.4)
#higher the salary, better chances of full payment


#annual_inc vs funded_amnt with status 
loanData %>%
  ggplot(aes(x =annual_inc, y = revol_bal)) + geom_point(aes(color = loan_status), alpha = 0.4)
#higher the salary, more the revolving balance


#grade vs loan status
loanData %>%
  filter(loan_status != "CURRENT") %>%
ggplot(aes(x=grade,fill=loan_status)) +
  geom_bar()+
  geom_text(aes(y=..count..,label=..count..),stat = "count", position=position_stack(vjust = 0.5))

loanData %>%
  filter(loan_status != "CURRENT") %>%
  ggplot(aes(x=grade,fill=loan_status)) +
  geom_bar(position = position_fill()) 
#with increasing grade, charged off percentage also increases


