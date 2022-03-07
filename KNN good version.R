--
  title: "Assigment - kNN DIY"
author:
  - Author - Esli
- Reviewer- Sander
--
install.packages("plotly")
  
  
  ```{r}
library(tidyverse)
library(googlesheets4)
library(class)
library(caret)
library(magrittr)
library(plotly)


---
  # Understanding the data
  The data set contains laboratory values of blood donors, Hepatitis C patients and their demographic values.

# Obtaining the data

url <- "https://raw.githubusercontent.com/HAN-M3DM-Data-Mining/assignments/master/datasets/KNN-hcvdat0.csv"
rawDF <- read_csv(url, )
str(rawDF)


# Preparating the data

# Cleaning the data

cleanDF <- na.omit(rawDF[-1])
head(cleanDF)


# Identifying the data (defined as either "Donor" or "Hepatitis")

cleanDF$Category <- factor(cleanDF$Category)
levels(cleanDF$Category) <- c("Donor", "Donor", "Hepatitis", "Hepatitis", "Hepatitis")
levels(cleanDF$Category)
table(cleanDF$Category)

# Visualizing the data

p1 <- ggplot(cleanDF) +
  geom_point(aes(ALB, ALP, colour = Category))
p1



# Normalize numeric features
# Setting a function 

normalize <- function(x) 
  return ((x - min(x)) / (max(x) - min(x)) 


# Testing the function 

testSet1 <- c(1:5)
testSet2 <- c(1:5) * 10
cat("testSet1:", testSet1, "\n")
cat("Normalized testSet1:", normalize(testSet1), "\n")
cat("testSet2:", testSet2, "\n")
cat("Normalized testSet2:", normalize(testSet2), "\n")




nCols <- dim(cleanDF)[2]
cleanDFnorm <- sapply(4:nCols,
                      function(x) {
                        normalize(cleanDF[, x])
                      }) %>% as.data.frame()
head(cleanDFnorm)


# Preparing train and testing the datasets

s <- sample(c(1:dim(cleanDF)[1]), 469)

#Splitting training and testing data sets
trainDFfeat <- cleanDFnorm[s,  ]
testDFfeat <- cleanDFnorm[-s,  ]

#Creating training and test data sets with labels
trainDFlabels <- cleanDF[s, 1]
testDFlabels <- cleanDF[-s, 1]



# Modeling and evaluating

cleanDFpred <- knn(train = as.matrix(trainDFfeat), test = as.matrix(testDFfeat), cl = as.matrix(trainDFlabels), k = 7)
head(cleanDFpred)
confusionMatrix(cleanDFpred, testDFlabels[[1]], positive = NULL, dnn = c("Prediction", "True"))
dim(cleanDFpred)
dim(testDFlabels)

