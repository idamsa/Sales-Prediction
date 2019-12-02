#THIS IS THE CODE FILE FOR THE MULTIPLE REHRESSSION TASK

#Add library and files#
library(readr)
library(caret)
library(corrplot)
library(e1071)
library (randomForest)
library(ISLR) 
# Reading the file
existing<-read.csv("existingproductattributes2017.csv")

# Explore data#
summary(existing)
str(existing)

## Pre-processing data##

# Dummify the data
existingdummy <-dummyVars ("~.", data = existing)
readyData <- data.frame(predict(existingdummy, newdata = existing))
readyData
str(readyData)

# Removing Missing Values
is.na(existing)
existing$BestSellersRank <- NULL
readyData$BestSellersRank <- NULL

# Builind the correlation matrix
corrData <- cor(readyData)

# Prints the correlation matrix
corrData

# Heatmap correlation
corrplot(corrData)

# Removing highly correlated features
readyData$x5StarReviews <- NULL

#Normalize numerical features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


readyDatan<-apply(readyData[c(14:26)],2,normalize)
readyData <- readyData[-c(14:26)]
readyData<- cbind(readyData,readyDatan) 
  View(readyData)
readyData$ProductNum <- NULL

#Splitting the data and set seed

set.seed(123)
inTrain <- createDataPartition(y= readyData$Volume,p=.75, list = FALSE)

# Partitioning the data

training <- readyData[ inTrain,]
testing <- readyData[-inTrain,]
training

### Building the linear model###

linearmodel<-lm(Volume ~ .,training)

# Summary linear model
summary(linearmodel)

predictionslinearmodel <- predict(linearmodel, testing)

### Building the SVM-model ###

#Using tuned parameters(SVM)#
tuned_parameters<-tune.svm(Volume~., data = training, gamma = 10^(-6:-2), cost=10^(1:2))
summary(tuned_parameters)
tuneValues <- tuned_parameters$best.model
predictionSvm2 <- predict(tuneValues, testing)
summary(predictionSvm2)
postResample(predictionSvm2, testing$Volume)

#Not using tuned parameters(SVM)#
svmModel <- svm(Volume~., data = training, kernel = "sigmoid", cost = 100, gamma = 0.01, scale = FALSE)
print(svmModel)
plot(svmModel, training)
predictionSvm <-predict(svmModel, testing)
predictionSvm
summary(predictionSvm)

postResample(predictionSvm, testing$Volume)

### Building Random Forest Model ###

# Tuned RF #
rfTuned <- tuneRF(x = testing, y = testing$Volume, mtryStart = 1)


#Non-tuned RF#
rfModel <- randomForest(Volume~., data = training, mtry = 26, ntree = 1000) 
rfModel
predictionRf <- predict(rfModel, testing)
summary(predictionRf)
postResample (predictionRf, testing$Volume)


### Building k-NN model ###
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
kNNmodel <- train(Volume~., data = training, method ="knn", trControl = ctrl)
kNNmodel
