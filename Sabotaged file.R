--
  title: "Assigment - KNN DIY"
author:
- Author - Esli
- Reviewer- Sander
--
  install.packages("tidyverse")
install.packages("googlesheets4")
install.packages("class")
install.pakcages("caret")
install.packages("class")
install.packages("caret")
install.packages("e1071")
library(tidyverse)
library(googlesheets4)
library(class)
library(caret)
library(forcats)
library(readr)

#R-Script KNN_HCV

#prepare data

str(rawDF)

cleanDF <- rawDF[-1]
head(cleanDF)

cntDiag <- table(cleanDF$diagnosis)
propDiag <- round(prop.table(cntDiag) * 100 , digits = 1)
cntDiag

propDiag

cleanDF$diagnosis <- factor(cleanDF$diagnosis, levels = c("B", "M"), labels = c("Benign", "Malignant")) 

head(cleanDF, 10)

summary(cleanDF[c("radius_mean", "area_mean", "smoothness_mean")])

normalize <- function(x) { # Function takes in a vector
  return ((x - min(x)) / (max(x) - min(x))) # distance of item value - minimum vector value divided by the range of all vector values
}

#########
## TEST ##
#########

testSet1 <- c(1:5)

testSet2 <- c(1:5) * 10
cat("testSet1:", testSet1, "\n")


cat("Normalized testSet1:", normalize(testSet1), "\n")

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

# normalize  --------------------------------------------------------------

nCols <- dim(cleanDF)[2]
cleanDF_n <- sapply(2:nCols,
                    function(x) {
                      normalize(cleanDF[,x])
                    }) %>% as.data.frame()

summary(cleanDF_n[c("radius_mean", "area_mean", "smoothness_mean")])
summary(cleanDF_n[c("radius_mean", "area_mean", "smoothness_mean")])

summary(cleanDF_n[c("radius_mean", "area_mean", "smoothness_mean")])

### creating training sets
trainDF_feat <- cleanDF_n[1:469,  ]
testDF_feat <- cleanDF_n[470:569,  ]


### seperating sets
trainDF_labels <- cleanDF[1:469,  1]
testDF_labels <- cleanDF[470:569,  1]

### making matrix

cleanDF_test_pred <- knn(train = as.matrix(trainDF_feat), test = as.matrix(testDF_feat), cl = as.matrix(trainDF_labels), k = 21)
head(cleanDF_test_pred)

confusionMatrix(cleanDF_test_pred, testDF_labels[[1]], negative = NULL, dnn = c("Prediction", "True"))
