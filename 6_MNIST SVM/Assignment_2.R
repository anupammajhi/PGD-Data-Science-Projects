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

# Renaming first column as number
colnames(mnistTrainDF)[1] <- "number"
colnames(mnistTestDF)[1] <- "number"

# Remaining columns contain the pixel values

# Converting the number to factor
mnistTrainDF$number <- as.factor(mnistTrainDF$number)
mnistTestDF$number <- as.factor(mnistTestDF$number)

#===== Preparing Train and Test data =====

set.seed(100)

# sampling the train data to 20% (12000 rows) as the train dataset is huge, to make the computation faster

trainIndices <- sample(1:nrow(mnistTrainDF),0.20*nrow(mnistTrainDF))
train <- mnistTrainDF[trainIndices,]

#===== Constructing the model =====

# Using Linear Kernel
modelLinear <- ksvm(number~., data=train, scale= FALSE, kernel = "vanilladot")
predictLinear <- predict(modelLinear, mnistTestDF)

confusionMatrix(predictLinear,mnistTestDF$number)   # Accuracy : 0.9181
# The accuracy is good enough in linear but since this is a 28x28 pixel data, we may get better accuracy with a RBF kernel


# Using RBF kernel
modelRBF <- ksvm(number~., data=train, scale= FALSE, kernel = "rbfdot")
predictRBF <- predict(modelRBF, mnistTestDF)

confusionMatrix(predictRBF,mnistTestDF$number)    # Accuracy : 0.9615
# The accuracy is much better with RBF Kernel


# Using Polynomial kernel
modelPoly <- ksvm(number~., data=train, scale= FALSE, kernel = "polydot")
predictPoly <- predict(modelPoly, mnistTestDF)

confusionMatrix(predictPoly,mnistTestDF$number)    # Accuracy : 0.9181
# The accuracy is good


# Hyperparameter tuning and cross validation

# using cross validation method in 5 folds and Accuracy metric
trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"

set.seed(200)

# ----- LINEAR KERNEL
# computing results with above setting and Linear kernel
gridLinear <- expand.grid(.C=c(1,2,3,4,5))
tuningSvmLinear <- train(number~., data=train, method="svmLinear", metric=metric,tuneGrid=gridLinear, trControl=trainControl)
#No difference for different C values

print(tuningSvmLinear)

plot(tuningSvmLinear)
#Accuracy : 0.9103


# ----- RADIAL KERNEL
# preparing grid in combination of s=(0.025 and 0.05) and c=(0.1, 0.5, 1 and 2) to find best combination
gridRadial <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )

# computing results with above setting and Radial kernel
tuningSvmRadial <- train(number~., data=train, method="svmRadial", metric=metric,tuneGrid=gridRadial, trControl=trainControl)
# Accuracy : 0.1136
# There is no significant difference for different sigma and c values

print(tuningSvmRadial)

plot(tuningSvmRadial)


# ----- POLY KERNEL
# computing results with above setting and Poly kernel
gridPoly <- expand.grid(.scale=c(0.01,0.02,0.03,0.04,0.05), .C=c(0.1,0.5,1,2,3), .degree=c(1,2,3))
tuningSvmPoly <- train(number~., data=train, method="svmPoly", metric=metric,tuneGrid=gridPoly, trControl=trainControl)
# There is no significant difference for different degree and c values

print(tuningSvmPoly)

plot(tuningSvmPoly)


# We can see that a Linear Kernel gives the best result for accuracy when we cross validate.
# Hence we should go with the linear Model 