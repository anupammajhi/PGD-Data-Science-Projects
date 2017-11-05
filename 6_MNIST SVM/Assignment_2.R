# Clearing Variables
rm(list = ls())
# Loading libraries

library(kernlab)
library(readr)
library(lattice)
library(ggplot2)
library(caret)
library(e1071)

#===== Data Preparation and Cleaning =====

# Importing csv train file 
# With no header as the file contains no header

mnistTrainDF = read.csv("mnist_train.csv",header = F)
mnistTestDF = read.csv("mnist_test.csv",header = F)

# Check NA values

sapply(mnistTrainDF,function(x){sum(is.na(x))}) #No NA Value found
sapply(mnistTestDF,function(x){sum(is.na(x))}) #No NA Value found

