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
carsDF[which(carsDF$company == "maxda"),"company"] <- "mazda"
carsDF[which(carsDF$company == "Nissan"),"company"] <- "nissan"
carsDF[which(carsDF$company == "porcshce"),"company"] <- "porsche"
carsDF[which(carsDF$company == "toyouta"),"company"] <- "toyota"
carsDF[which(carsDF$company == "vokswagen" | carsDF$company == "vw"),"company"] <- "volkswagen"
carsDF[which(carsDF$company == "alfa-romero"),"company"] <- "alfa-romeo"

# Checking NA
which(is.na(carsDF)) #No NA Value found

# Ignoring unwanted columns
carsDF <- carsDF[,!names(carsDF) %in% c("car_ID","CarName")]
