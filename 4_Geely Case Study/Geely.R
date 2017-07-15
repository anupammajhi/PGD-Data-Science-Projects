# Clear variables
rm(list=ls())

library("MASS")
library("car")

# Import dataset
carsDF <- read.csv("CarPrice_Assignment.csv")

str(carsDF)

carsDF$company <- as.vector(sapply(as.vector(carsDF$CarName), function(x){strsplit(x," ")[[1]][1]}))

# Checking quality issues
levels(as.factor(carsDF$company))

# Correcting company names
