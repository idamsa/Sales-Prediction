#THIS IS THE CODE FILE FOR THE MULTIPLE REHRESSSION TASK

#Add library and files#
library(readr)
library(caret)
library(corrplot)

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

# Ptints the correlation matrix
corrData

# Heatmap correlation
corrplot(corrData)

#Splitting the data

inTrain <- createDataPartition(y= readyData$Volume,p=.75, list = FALSE)

# Partitioning the data

training <- readyData[ inTrain,]
testing <- readyData[-inTrain,]
training
# Building the linear model

linearmodel<-lm(Volume ~ .,training)

# Summary linear model
summary(linearmodel)

