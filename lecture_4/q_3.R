library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis_result <- factor(prc$diagnosis_result)

sample_data <- sample.split(prc, SplitRatio = 0.8)
train_data <- subset(prc, sample_data == TRUE)
test_data <- subset(prc, sample_data == FALSE)
png(file = "decision_tree.png")

model<-ctree(diagnosis_result ~ radius + texture + perimeter + area + smoothness + compactness + symmetry + fractal_dimension,  data = train_data)


# Plot the tree.
plot(model)

# Save the file.
dev.off()

predict_model<-predict(model, test_data)

m_at <- table(test_data$diagnosis, predict_model)
m_at
ac_test <- sum(diag(m_at)) / sum(m_at)
ac_test


####################Random Forest###########
library(cvms)
library(tibble)
library(caTools)
library(randomForest)

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