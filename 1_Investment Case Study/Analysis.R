#Clear all variables
rm(list=ls())

# CHECKPOINT 1: Data Cleaning 1

# Load the companies and rounds data
companies <- read.delim("companies.txt",stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv",stringsAsFactors = F)

# Loading libraries stringr, tidyr and dplyr
library(stringr)
library(dplyr)
library(tidyr)

# How many unique companies are present in rounds2?
