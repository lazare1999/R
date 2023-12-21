library(e1071)
library(caTools)
library(caret)

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

split <- sample.split(cars, SplitRatio = 0.8)
train_cl <- subset(cars, split == "TRUE")
test_cl <- subset(cars, split == "FALSE")

# Fitting Naive Bayes Model
# to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(Identification.Make ~ ., data = train_cl)

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$Identification.Make, y_pred)

ac_test <- sum(diag(cm)) / sum(cm)
ac_test