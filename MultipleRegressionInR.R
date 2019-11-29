#THIS IS THE CODE FILE FOR THE MULTIPLE REHRESSSION TASK

#Add library and files#
library(readr)
library (caret)

existing<-read.csv("existingproductattributes2017.csv")

#Explore data#
summary(existing)
str(existing)
is.na(existing)


##Pre-processing data##

#Dummify the data
existingdummy <-dummyVars ("~.", data = existing)
readyData <- data.frame(predict(existingdummy, newdata = existing))
readyData
str(readyData)
