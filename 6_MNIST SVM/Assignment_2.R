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
