#THIS IS THE CODE FILE FOR THE MULTIPLE REHRESSSION TASK

#Add library and files#
library(readr)

library(caret)
install.packages("corrplot")
library(corrplot)


existing<-read.csv("existingproductattributes2017.csv")

# Explore data#
summary(existing)
str(existing)

View(existing)
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

# Removing unecessary features
existing <- existing[c(5,7,9,10)]
existing

