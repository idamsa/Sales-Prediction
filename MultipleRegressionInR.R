#THIS IS THE CODE FILE FOR THE MULTIPLE REGRESSION TASK

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


#Reading the file

existing<-read.csv("existingproductattributes2017.csv")

#Explore data#

summary(existing)
str(existing)

##Pre-processing data##

# Dummify the data

existingdummy <-dummyVars ("~.", data = existing)
readyData <- data.frame(predict(existingdummy, newdata = existing))
readyData
str(readyData)

# emoving Missing Values
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

# Normalize numerical features
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


readyDatan<-apply(readyData[c(14:26)],2,normalize)
readyData <- readyData[-c(14:26)]
readyData<- cbind(readyData,readyDatan) 
View(readyData)
readyData$ProductNum <- NULL
readyData<-readyData[c(1:13,15,17,19,20,21)]

# Outliers

outlier_volume <- outlier(readyData$Volume,logical = TRUE)
sum(outlier_volume)

# Assigning the outliers to a vector

find_outlierVolume <- which(outlier_volume == TRUE, arr.ind = TRUE)

# Removing the outliers in vOLUME

readyData= readyData[-find_outlierVolume,]


# Splitting the data and set seed

set.seed(123)
inTrain <- createDataPartition(y= readyData$Volume,p=.75, list = FALSE)

# Partitioning the data

training <- readyData[ inTrain,]
testing <- readyData[-inTrain,]
training

### Building the linear model ###

linearmodel<-lm(Volume ~ .,training)


# Summary linear model

summary(linearmodel)

predictionslinearmodel <- predict(linearmodel, testing)
predictionslinearmodel


### Building the SVM-model ###

# Using tuned parameters(SVM)#

tuned_parameters<-tune.svm(Volume~., data = training, epsilon = seq(0, 0.9, 0.1),  cost = 2^(2:8))
summary(tuned_parameters)
tuneValues <- tuned_parameters$best.model
predictionSvm2 <- predict(tuneValues, testing)


summary(predictionSvm2)
postResample(predictionSvm2, testing$Volume)


# Plotting SVM-model, predcidted

testingSVM<- testing
testingSVM<- cbind(testingSVM,predictionSvm2)

ggplot(testingSVM, aes(x=predictionSvm2, y=Volume)) + 
  geom_point()+
  labs(title="Pred vs Actual SVM")

### Building Random Forest Model ###

# Tune the RF

rfTuned <- tuneRF(x = testing, y = testing$Volume, mtryStart = 1)


# Applying Tuned RF

rfModel <- randomForest(Volume~., data = training, mtry=8)
rfModel
predictionRf <- predict(rfModel, testing)
summary(predictionRf)

# Erros metrics random forest

postResample (predictionRf, testing$Volume)


### Building k-NN model ###

kNNmodel <- train(Volume~., data = training, method ="knn")
kNNmodel
predictedknn <- predict(kNNmodel,testing)
predictedknn                        

# Ploting the observed vs predicted in RF

dataForPlot<-read.csv("existingproductattributes2017.csv")
r<-rownames(testing)
testingfull <- dataForPlot[c(r), ]
testingfull<- cbind(testingfull,predictionRf)
ggplot(testingfull, aes(x=predictionRf, y=Volume,col=ProductType)) +
  geom_point()+
  labs(title="Pred vs Actual RF")+
  geom_line()

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

predictionNew<- predict(rfModel, readyData2)
predictionNew

#Creating data frame with predicted values of volume for new data

newProducts$predicted <- predictionNew
newProducts <-newProducts[c(1:17,19)]
newProducts$predicted<- round(newProducts$predicted, digits = 0)
View(newProducts)


#Creating plot for predicted volume of new products

ggplot(newProducts, aes(x=predicted,fill=ProductType)) +
  geom_histogram(bins=10)+labs(title="Predicted volume")
 
# PieChart

ggplot(newProducts, aes(x="", fill=ProductType))+
  geom_bar() + 
  coord_polar(theta="y") +
  labs(x=" ",y=" ", title = "Proportion of Product Type Sales Predict") +
  theme_light()
