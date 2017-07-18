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

# cylindernumber
summary(carsDF$cylindernumber)
levels(carsDF$cylindernumber) <- c(8,5,4,6,3,12,2)
carsDF$cylindernumber <- as.numeric(levels(carsDF$cylindernumber))[carsDF$cylindernumber]
summary(carsDF$cylindernumber)

# fuelsystem
dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem,data = carsDF))
carsDF <- cbind(carsDF[,-13],dummy_fuelsystem[,-1])

# company
dummy_company <- data.frame(model.matrix(~company,data = carsDF))
carsDF <- cbind(carsDF[,-21],dummy_company[,-1])

# DERIVED VARIABLES
# hp2wRatio : power to weight ratio = horsepower / curbweight
carsDF$hp2wRatio <- carsDF$horsepower/carsDF$curbweight

# bore : bore size
carsDF$bore <- carsDF$boreratio * carsDF$stroke

# cc : Engine displacement in CC . i.e. cc = (pi/4) x bore^2 x stroke x number of cylinder (also convert in to cm).
carsDF$cc <- (pi/4) * (carsDF$bore^2) * (carsDF$stroke * 2.54 ) * carsDF$cylindernumber

# wb2lRatio : wheelbase to length ratio
carsDF$wb2lRatio <- carsDF$wheelbase / carsDF$carlength

# rpm2hpRatio : RPM to Power(HP) ratio
carsDF$rpm2hpRatio <- carsDF$peakrpm / carsDF$horsepower


# Linear Regression

# Set Seed to regenerate random number
set.seed(100)

# Define Training and Test Sets : 70% train, 30% test
trainIndices <- sample(1:nrow(carsDF),round(0.7*nrow(carsDF)))
cars.Train <- carsDF[trainIndices,]
cars.Test <- carsDF[-trainIndices,]


# MODELLING

# Model 1 : dependent variable against all independent variables
model_1 <- lm(price~.,data = cars.Train)
summary(model_1) # 5 Not defined due to singularity
# R-squared: 0.977 , Adjusted-R-squared:0.9609


# Model 2 : StepAIC to remove variables on model_1
model_2 <- stepAIC(model_1,direction = "both")
summary(model_2)
# R-squared: 0.9761 , Adjusted-R-squared:0.9644
sort(vif(model_2)) 
# bore,stroke,horsepower has high vif but still significant.
# boreratio has high vif and less significant. Hence removing from next model.


