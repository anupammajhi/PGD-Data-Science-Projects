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
str(carsDF)


# DUMMY VARIABLES

# fueltype
summary(carsDF$fueltype)
levels(carsDF$fueltype) <- c(0,1) # diesel = 0 , gas = 1
carsDF$fueltype <- as.numeric(levels(carsDF$fueltype))[carsDF$fueltype]
summary(carsDF$fueltype)

# aspiration
summary(carsDF$aspiration)
levels(carsDF$aspiration) <- c(0,1) # std = 0 , turbo = 1
carsDF$aspiration <- as.numeric(levels(carsDF$aspiration))[carsDF$aspiration]
summary(carsDF$aspiration)


# doornumber
summary(carsDF$doornumber)
levels(carsDF$doornumber) <- c(4,2) # four = 4 , two = 2
carsDF$doornumber <- as.numeric(levels(carsDF$doornumber))[carsDF$doornumber]
summary(carsDF$doornumber)

# enginelocation
summary(carsDF$enginelocation)
levels(carsDF$enginelocation) <- c(0,1) # front = 0 , rear = 1
carsDF$enginelocation <- as.numeric(levels(carsDF$enginelocation))[carsDF$enginelocation]
summary(carsDF$enginelocation)

# carbody
dummy_carbody <- data.frame(model.matrix(~carbody,data = carsDF))
carsDF <- cbind(carsDF[,-5],dummy_carbody[,-1])

# drivewheel
dummy_drivewheel <- data.frame(model.matrix(~drivewheel,data=carsDF))
carsDF <- cbind(carsDF[,-5],dummy_drivewheel[,-1])

# enginetype
dummy_enginetype <- data.frame(model.matrix(~enginetype,data = carsDF))
carsDF <- cbind(carsDF[,-11],dummy_enginetype[,-1])
