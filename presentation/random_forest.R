library(cvms)
library(tibble)
library(caTools)
library(randomForest)

# setwd("dataset")
cars <- read.csv("dataset/Cars 1.csv", stringsAsFactors = FALSE)

cars$Engine.Information.Driveline <- factor(cars$Engine.Information.Driveline)
cars$Engine.Information.Driveline <- as.numeric(cars$Engine.Information.Driveline)

cars$Engine.Information.Engine.Type <- factor(cars$Engine.Information.Engine.Type)
cars$Engine.Information.Engine.Type <- as.numeric(cars$Engine.Information.Engine.Type)

cars$Engine.Information.Hybrid <- factor(cars$Engine.Information.Hybrid)
cars$Engine.Information.Hybrid <- as.numeric(cars$Engine.Information.Hybrid)

cars$Engine.Information.Transmission <- factor(cars$Engine.Information.Transmission)
cars$Engine.Information.Transmission <- as.numeric(cars$Engine.Information.Transmission)

cars$Fuel.Information.Fuel.Type <- factor(cars$Fuel.Information.Fuel.Type)
cars$Fuel.Information.Fuel.Type <- as.numeric(cars$Fuel.Information.Fuel.Type)

cars$Identification.Classification <- factor(cars$Identification.Classification)
cars$Identification.Classification <- as.numeric(cars$Identification.Classification)

cars$Identification.ID <- factor(cars$Identification.ID)
cars$Identification.ID <- as.numeric(cars$Identification.ID)

cars$Identification.Model.Year <- factor(cars$Identification.Model.Year)
cars$Identification.Model.Year <- as.numeric(cars$Identification.Model.Year)

cars$Identification.Make <- factor(cars$Identification.Make)

split <- sample.split(cars$Identification.Make, SplitRatio = 0.8)
train <- subset(cars, split == "TRUE")
test <- subset(cars, split == "FALSE")
class(test)
classifier_RF <- randomForest(x = train[-14],
                              y = train$Identification.Make,
                              ntree = 500)

# Predicting the Test set results
y_pred <- predict(classifier_RF, newdata = test[-14])

# Confusion Matrix
confusion_mtx <- table(test[, 14], y_pred)
ac_test <- sum(diag(confusion_mtx)) / sum(confusion_mtx)
ac_test