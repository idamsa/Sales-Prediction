#THIS IS THE CODE FILE FOR THE MULTIPLE REHRESSSION TASK

#Add library and files#
library(readr)
library(caret)
library(corrplot)
library(e1071)
library (randomForest)
library(ISLR)

library(ggplot2)

library(outliers)
library(OneR)
library(Hmisc)
library(C50)
library(rattle)
library(rpart)
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
ready
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
readyData<-readyData[c(1:13,15,17,19,20,21)]

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
predictionslinearmodel


### Building the SVM-model ###

#Using tuned parameters(SVM)#
tuned_parameters<-tune.svm(Volume~., data = training, cost=10^(1:2))
summary(tuned_parameters)
tuneValues <- tuned_parameters$best.model
predictionSvm2 <- predict(tuneValues, testing)


summary(predictionSvm2)
postResample(predictionSvm2, testing$Volume)


### Building Random Forest Model ###

# Tuned RF #
rfTuned <- tuneRF(x = testing, y = testing$Volume, mtryStart = 1)


#Non-tuned RF#
rfModel <- randomForest(Volume~., data = training,mtry=3,treesize=1000, importance=T)
rfModel
predictionRf <- predict(rfModel, testing)
summary(predictionRf)
postResample (predictionRf, testing$Volume)
varImp(rfModel)

### Building k-NN model ###
ctrl <- trainControl(method = "repeatedcv", repeats = 3)
kNNmodel <- train(Volume~., data = training, method ="knn", trControl = ctrl)
kNNmodel
predictedknn <- predict(kNNmodel,testing)
predictedknn                        


# Bring the new product data

newProducts <- read.csv("newproductattributes2017.csv")

# Dummyfying the new data

newDataFrame2 <- dummyVars(" ~ .", data = newProducts)

readyData2 <- data.frame(predict(newDataFrame2, newdata = newProducts))

readyData2

# Preprocessing the new data

readyData2$BestSellersRank <- NULL
readyData2$ProductNum <- NULL
View(readyData2) 
readyDatan2<-apply(readyData2[c(13:20)],2,normalize)
readyData2 <- readyData2[-c(13:20)]
readyData2<- cbind(readyData2,readyDatan2) 
readyData2<-readyData2[c(1:13,22,24,26,27)]


#  Applying the model to the new data
