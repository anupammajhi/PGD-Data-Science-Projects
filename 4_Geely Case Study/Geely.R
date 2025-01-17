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


# Model 3 : after removing boreratio
model_3 <- lm(price~symboling+fueltype+aspiration+doornumber+enginelocation+wheelbase+carlength+carwidth+carheight+curbweight+cylindernumber+enginesize+
              stroke+compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+drivewheelrwd+
              enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
              companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
              companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_3)
# R-squared: 0.9753 , Adjusted-R-squared:0.9635 . Hence not much change in result.
sort(vif(model_3))
# horsepower, fueltype, hp2wratio, compressionratio has high vif but still significant.
# curbweight has high vif and less significant. Hence removing from next model.


# Model 4 : after removing curbweight
model_4 <- lm(price~symboling+fueltype+aspiration+doornumber+enginelocation+wheelbase+carlength+carwidth+carheight+cylindernumber+enginesize+
                stroke+compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+drivewheelrwd+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_4)
# R-squared: 0.9747 , Adjusted-R-squared:0.9631 . Hence not much change in result.
sort(vif(model_4))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize has high vif but still significant.
# cylindernumber has high vif and less significant. Hence removing from next model.


# Model 5 : after removing cylindernumber
model_5 <- lm(price~symboling+fueltype+aspiration+doornumber+enginelocation+wheelbase+carlength+carwidth+carheight+enginesize+
                stroke+compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+drivewheelrwd+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_5)
# R-squared: 0.9746 , Adjusted-R-squared:0.9634 . Hence not much change in result.
sort(vif(model_5))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, fuelsystemmpfi, carlength has high vif but still significant.
# stroke has high vif and less significant. Hence removing from next model.


# Model 6 : after removing stroke
model_6 <- lm(price~symboling+fueltype+aspiration+doornumber+enginelocation+wheelbase+carlength+carwidth+carheight+enginesize+
                compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+drivewheelrwd+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_6)
# R-squared: 0.9741  Adjusted-R-squared:0.9629 . Hence not much change in result.
sort(vif(model_6))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, fuelsystemmpfi, carlength, carbodysedan, fuelsystem2bbl, wheelbase has high vif but still significant.
# carwidth has high vif and less significant. Hence removing from next model.


# Model 7 : after removing carwidth
model_7 <- lm(price~symboling+fueltype+aspiration+doornumber+enginelocation+wheelbase+carlength+carheight+enginesize+
                compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+drivewheelrwd+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_7)
# R-squared: 0.9732  Adjusted-R-squared:0.9620 . Hence not much change in result.
sort(vif(model_7))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, fuelsystemmpfi, carlength, carbodysedan, fuelsystem2bbl, wheelbase, carbodyhatchback, citympg, carbodywagon has high vif but still significant.
# aspiration has high vif and less significant. Hence removing from next model.


# Model 8 : after removing aspiration
model_8 <- lm(price~symboling+fueltype+doornumber+enginelocation+wheelbase+carlength+carheight+enginesize+
                compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+drivewheelrwd+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_8)
# R-squared: 0.9722  Adjusted-R-squared:0.9610 . Hence not much change in result.
sort(vif(model_8))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, fuelsystemmpfi, carlength, carbodysedan, fuelsystem2bbl, wheelbase, carbodyhatchback, citympg, carbodywagon, symboling, enginetypeohcv has high vif but still significant.
# drivewheelrwd has high vif and less significant. Hence removing from next model.


# Model 9 : after removing drivewheelrwd
model_9 <- lm(price~symboling+fueltype+doornumber+enginelocation+wheelbase+carlength+carheight+enginesize+
                compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystem2bbl+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_9)
# R-squared: 0.9720  Adjusted-R-squared:0.9611 . Hence not much change in result.
sort(vif(model_9))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, fuelsystemmpfi, carlength, carbodysedan, wheelbase, carbodyhatchback, citympg, carbodywagon, symboling, enginetypeohcv has high vif but still significant.
# fuelsystem2bbl has high vif and less significant. Hence removing from next model.


# Model 10 : after removing fuelsystem2bbl
model_10 <- lm(price~symboling+fueltype+doornumber+enginelocation+wheelbase+carlength+carheight+enginesize+
                compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystemmfi+fuelsystemmpfi+fuelsystemspdi+companyaudi+companybmw+
                companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
              data = cars.Train)
summary(model_10)
# R-squared: 0.9709  Adjusted-R-squared:0.9600 . Hence not much change in result.
sort(vif(model_10))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carlength, carbodysedan, wheelbase, carbodyhatchback, citympg, carbodywagon, symboling, carheight has high vif but still significant.
# fuelsystemmpfi has high vif and less significant. Hence removing from next model.


# Model 11 : after removing fuelsystemmpfi
model_11 <- lm(price~symboling+fueltype+doornumber+enginelocation+wheelbase+carlength+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystemmfi+fuelsystemspdi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_11)
# R-squared: 0.9703  Adjusted-R-squared:0.9596 . Hence not much change in result.
sort(vif(model_11))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize  has high vif but still significant.
# carlength has high vif and less significant. Hence removing from next model.


# Model 12 : after removing carlength
model_12 <- lm(price~symboling+fueltype+doornumber+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetypeohcv+enginetyperotor+fuelsystemmfi+fuelsystemspdi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_12)
# R-squared: 0.9692  Adjusted-R-squared:0.9585 . Hence not much change in result.
sort(vif(model_12))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg  has high vif but still significant.
# enginetypeohcv has high vif and less significant. Hence removing from next model.


# Model 13 : after removing enginetypeohcv
model_13 <- lm(price~symboling+fueltype+doornumber+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetyperotor+fuelsystemmfi+fuelsystemspdi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_13)
# R-squared: 0.9684  Adjusted-R-squared:0.9577 . Hence not much change in result.
sort(vif(model_13))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# doornumber has high vif and less significant. Hence removing from next model.


# Model 14 : after removing doornumber
model_14 <- lm(price~symboling+fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetyperotor+fuelsystemmfi+fuelsystemspdi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companytoyota+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_14)
# R-squared: 0.9678  Adjusted-R-squared:0.9573 . Hence not much change in result.
sort(vif(model_14))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companytoyota has high vif and less significant. Hence removing from next model.


# Model 15 : after removing companytoyota
model_15 <- lm(price~symboling+fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetyperotor+fuelsystemmfi+fuelsystemspdi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_15)
# R-squared: 0.9670  Adjusted-R-squared:0.9567 . Hence not much change in result.
sort(vif(model_15))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# symboling has high vif and less significant. Hence removing from next model.


# Model 16 : after removing symboling
model_16 <- lm(price~fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetyperotor+fuelsystemmfi+fuelsystemspdi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_16)
# R-squared: 0.9658  Adjusted-R-squared:0.9555 . Hence not much change in result.
sort(vif(model_16))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# fuelsystemspdi has high vif and less significant. Hence removing from next model.


# Model 17 : after removing fuelsystemspdi
model_17 <- lm(price~fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetypeohcf+enginetyperotor+fuelsystemmfi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_17)
# R-squared: 0.9658  Adjusted-R-squared:0.9559 . Hence not much change in result.
sort(vif(model_17))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# enginetypeohcf has high vif and less significant. Hence removing from next model.


# Model 18 : after removing enginetypeohcf
model_18 <- lm(price~fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+fuelsystemmfi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companynissan+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_18)
# R-squared: 0.9653  Adjusted-R-squared:0.9557 . Hence not much change in result.
sort(vif(model_18))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companynissan has high vif and less significant. Hence removing from next model.


# Model 19 : after removing companynissan
model_19 <- lm(price~fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+fuelsystemmfi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyhonda+companyisuzu+companyjaguar+companymazda+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_19)
# R-squared: 0.9645  Adjusted-R-squared:0.9551 . Hence not much change in result.
sort(vif(model_19))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companyhonda is less significant. Hence removing from next model.


# Model 20 : after removing companyhonda
model_20 <- lm(price~fueltype+enginelocation+wheelbase+carheight+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+fuelsystemmfi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyisuzu+companyjaguar+companymazda+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_20)
# R-squared: 0.9635  Adjusted-R-squared:0.9542 . Hence not much change in result.
sort(vif(model_20))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# carheight has high vif and is less significant. Hence removing from next model.


# Model 21 : after removing carheight
model_21 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+fuelsystemmfi+companyaudi+companybmw+
                 companybuick+companychevrolet+companyisuzu+companyjaguar+companymazda+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_21)
# R-squared: 0.9623  Adjusted-R-squared:0.9531 . Hence not much change in result.
sort(vif(model_21))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companychevrolet is less significant. Hence removing from next model.


# Model 22 : after removing companychevrolet
model_22 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+fuelsystemmfi+companyaudi+companybmw+
                 companybuick+companyisuzu+companyjaguar+companymazda+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_22)
# R-squared: 0.9623  Adjusted-R-squared:0.9535 . Hence not much change in result.
sort(vif(model_22))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# fuelsystemmfi is less significant. Hence removing from next model.


# Model 23 : after removing fuelsystemmfi
model_23 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyisuzu+companyjaguar+companymazda+companyporsche+companyrenault+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_23)
# R-squared: 0.9623  Adjusted-R-squared:0.9539 . Hence not much change in result.
sort(vif(model_23))
# horsepower, fueltype, hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companyrenault is less significant. Hence removing from next model.


# Model 24 : after removing companyrenault
model_24 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyisuzu+companyjaguar+companymazda+companyporsche+
                 companysaab+companyvolkswagen+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_24)
# R-squared: 0.9621  Adjusted-R-squared:0.9540 . Hence not much change in result.
sort(vif(model_24))
# horsepower, fueltype,hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companyvolkswagen is less significant. Hence removing from next model.


# Model 25 : after removing companyvolkswagen
model_25 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyisuzu+companyjaguar+companymazda+companyporsche+
                 companysaab+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_25)
# R-squared: 0.9621  Adjusted-R-squared:0.9540 . Hence not much change in result.
sort(vif(model_25))
# horsepower, fueltype,hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# companyisuzu is less significant. Hence removing from next model.


# Model 26 : after removing companyisuzu
model_26 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+companyvolvo+hp2wRatio+bore,
               data = cars.Train)
summary(model_26)
# R-squared: 0.9616  Adjusted-R-squared:0.9543 . Hence not much change in result.
sort(vif(model_26))
# horsepower, fueltype,hp2wratio, compressionratio, enginesize, carbodysedan, carbosyhatchback, carbodywagon, wheelbase,citympg and many others have high vif but still significant.
# bore is less significant. Hence removing from next model.


# Model 27 : after removing bore
model_27 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 compressionratio+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+companyvolvo+hp2wRatio,
               data = cars.Train)
summary(model_27)
# R-squared: 0.9598  Adjusted-R-squared:0.9525 . Hence not much change in result.
sort(vif(model_27))
# compressionratio has high vif and is less significant. Hence removing from next model.


# Model 28 : after removing compressionratio
model_28 <- lm(price~fueltype+enginelocation+wheelbase+enginesize+
                 horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+companyvolvo+hp2wRatio,
               data = cars.Train)
summary(model_28)
# R-squared: 0.9563  Adjusted-R-squared:0.9488 . Hence not much change in result.
sort(vif(model_28))
# fueltype is less significant. Hence removing from next model.


# Model 29 : after removing fueltype
model_29 <- lm(price~enginelocation+wheelbase+enginesize+
                 horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+companyvolvo+hp2wRatio,
               data = cars.Train)
summary(model_29)
# R-squared: 0.9562  Adjusted-R-squared:0.9491 . Hence not much change in result.
sort(vif(model_29))
# companyvolvo is less significant. Hence removing from next model.


# Model 30 : after removing companyvolvo
model_30 <- lm(price~enginelocation+wheelbase+enginesize+
                 horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+hp2wRatio,
               data = cars.Train)
summary(model_30)
# R-squared: 0.9548  Adjusted-R-squared:0.9479 . Hence not much change in result.
sort(vif(model_30))
# enginesize has hight vif and is less significant. Hence removing from next model.


# Model 31 : after removing enginesize
model_31 <- lm(price~enginelocation+wheelbase+
                horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 enginetyperotor+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+hp2wRatio,
               data = cars.Train)
summary(model_31)
# R-squared: 0.9529  Adjusted-R-squared:0.9461 . Hence not much change in result.
sort(vif(model_31))
# enginetyperotor is less significant. Hence removing from next model.


# Model 32 : after removing enginetyperotor
model_32 <- lm(price~enginelocation+wheelbase+
                 horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+
                 companysaab+hp2wRatio,
               data = cars.Train)
summary(model_32)
# R-squared: 0.9520  Adjusted-R-squared:0.9455 . Hence not much change in result.
sort(vif(model_32))
# companysaab is less significant. Hence removing from next model.


# Model 33 : after removing companysaab
model_33 <- lm(price~enginelocation+wheelbase+horsepower+peakrpm+citympg+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 companyaudi+companybmw+companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_33)
# R-squared: 0.9509  Adjusted-R-squared:0.9447 . Hence not much change in result.
sort(vif(model_33))
# citympg is less significant. Hence removing from next model.


# Model 34 : after removing citympg
model_34 <- lm(price~enginelocation+wheelbase+horsepower+peakrpm+carbodyhardtop+carbodyhatchback+carbodysedan+carbodywagon+
                 companyaudi+companybmw+companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_34)
# R-squared: 0.9498  Adjusted-R-squared:0.9439 . Hence not much change in result.
sort(vif(model_34))
# carbodyhardtop is less significant. Hence removing from next model.


# Model 35 : after removing carbodyhardtop
model_35 <- lm(price~enginelocation+wheelbase+horsepower+peakrpm+carbodyhatchback+carbodysedan+carbodywagon+
                 companyaudi+companybmw+companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_35)
# R-squared: 0.9482  Adjusted-R-squared:0.9425 . Hence not much change in result.
sort(vif(model_35))
# peakrpm is less significant. Hence removing from next model.


# Model 36 : after removing peakrpm
model_36 <- lm(price~enginelocation+wheelbase+horsepower+carbodyhatchback+carbodysedan+carbodywagon+
                 companyaudi+companybmw+companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_36)
# R-squared: 0.9452  Adjusted-R-squared:0.9398 . Hence not much change in result.
sort(vif(model_36))
# carbodysedan is less significant. Hence removing from next model.


# Model 37 : after removing carbodysedan
model_37 <- lm(price~enginelocation+wheelbase+horsepower+carbodyhatchback+carbodywagon+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_37)
# R-squared: 0.9426  Adjusted-R-squared:0.9373 . Hence not much change in result.
sort(vif(model_37))
# carbodyhatchback is less significant. Hence removing from next model.


# Model 38 : after removing carbodyhatchback
model_38 <- lm(price~enginelocation+wheelbase+horsepower+carbodywagon+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_38)
# R-squared: 0.9422  Adjusted-R-squared:0.9374 . Hence not much change in result.
sort(vif(model_38))
# carbodywagon is less significant. Hence removing from next model.


# Model 39 : after removing carbodywagon
model_39 <- lm(price~enginelocation+wheelbase+horsepower+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_39)
# R-squared: 0.9394  Adjusted-R-squared:0.9349 . Hence not much change in result.
sort(vif(model_39))
# wheelbase is less significant. Hence removing from next model.


# Model 40 : after removing wheelbase
model_40 <- lm(price~enginelocation+horsepower+companyaudi+companybmw+
                 companybuick+companyjaguar+companymazda+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_40)
# R-squared: 0.9354  Adjusted-R-squared:0.9311 . Hence not much change in result.
sort(vif(model_40))
# companymazda is less significant. Hence removing from next model.


# Model 41 : after removing companymazda
model_41 <- lm(price~enginelocation+horsepower+companyaudi+companybmw+
                 companybuick+companyjaguar+companyporsche+hp2wRatio,
               data = cars.Train)
summary(model_41)
# R-squared: 0.9312  Adjusted-R-squared:0.9271 . Hence not much change in result.
sort(vif(model_41))
# companyporsche is less significant. Hence removing from next model.


# Model 42 : after removing companyporsche
model_42 <- lm(price~enginelocation+horsepower+companyaudi+companybmw+
                 companybuick+companyjaguar+hp2wRatio,
               data = cars.Train)
summary(model_42)
# R-squared: 0.9262  Adjusted-R-squared:0.9224 . Hence not much change in result.
sort(vif(model_42))
# companyaudi is less significant. Hence removing from next model.


# Model 43 : after removing companyaudi
model_43 <- lm(price~enginelocation+horsepower+companybmw+
                 companybuick+companyjaguar+hp2wRatio,
               data = cars.Train)
summary(model_43)
# R-squared: 0.9210  Adjusted-R-squared:0.9176 . Hence not much change in result.
sort(vif(model_43))
# companyjaguar is less significant. Hence removing from next model.


# Model 44 : after removing companyjaguar
model_44 <- lm(price~enginelocation+horsepower+companybmw+
                 companybuick+hp2wRatio,
               data = cars.Train)
summary(model_44)
# R-squared: 0.9163  Adjusted-R-squared:0.9133 . Hence not much change in result.
sort(vif(model_44))

# Since all the variables are significant now, we will check and predict values from test set
predict_1 <- predict(model_44,cars.Test[,names(cars.Test) != 'price'])

cars.Test$test_price <- predict_1

#Check correlation on test data
r <- (cor(predict_1,cars.Test$price))^2
r
rsquared <- r^2
rsquared

# According to the model enginelocation,horsepower,companybmw,companybuick,hp2wRatio are the important variables that decide the price of the cars