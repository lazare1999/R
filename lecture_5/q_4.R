# Loading package
library(e1071)
library(caTools)
library(caret)
# Splitting data into train
# and test data

prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis_result <- factor(prc$diagnosis_result)

split <- sample.split(prc, SplitRatio = 0.8)
train_cl <- subset(prc, split == "TRUE")
test_cl <- subset(prc, split == "FALSE")
# Feature Scaling
train_scale <- scale(train_cl[, 2:9])
test_scale <- scale(test_cl[, 2:9])

# Fitting Naive Bayes Model
# to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(diagnosis_result ~ ., data = train_cl)

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$diagnosis_result, y_pred)

# Model Evaluation
confusionMatrix(cm)

ac_test <- sum(diag(cm)) / sum(cm)
ac_test


# naiveBayes : 0.8636364
# dessision trees : 0.8695652
# random forrest : 0.85