#THIS IS THE CODE FILE FOR THE MULTIPLE REHRESSSION TASK

#Add library and files#
library(readr)
library(caret)
install.packages("corrplot")
library(corrplot)

existing<-read.csv("existingproductattributes2017.csv")

#Explore data#
summary(existing)
str(existing)



# Removing Missing Values
is.na(existing)
existing$BestSellersRank <- NULL


# Builind the correlation matrix
corrData <- cor(readyData)

# Ptints the correlation matrix
corrData


# Heatmap correlation
corrplot(corrData)