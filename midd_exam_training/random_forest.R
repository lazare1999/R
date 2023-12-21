library(cvms)
library(tibble)
library(caTools)
library(randomForest)

setwd("dataset")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis_result <- factor(prc$diagnosis_result)

split <- sample.split(prc$diagnosis_result, SplitRatio = 0.8)
str(split)
train <- subset(prc, split == "TRUE")
test <- subset(prc, split == "FALSE")
class(test)
classifier_RF <- randomForest(x = train[-1],
                              y = train$diagnosis_result,
                              ntree = 500)

# Predicting the Test set results
y_pred <- predict(classifier_RF, newdata = test[-1])

# Confusion Matrix
confusion_mtx <- table(test[, 1], y_pred)
confusion_mtx
ac_test <- sum(diag(confusion_mtx)) / sum(confusion_mtx)
ac_test