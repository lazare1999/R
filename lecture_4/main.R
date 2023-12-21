library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

data("readingSkills")
head(readingSkills)
sample_data <- sample.split(readingSkills, SplitRatio = 0.8)
train_data <- subset(readingSkills, sample_data == TRUE)
test_data <- subset(readingSkills, sample_data == FALSE)
train_data
test_data
png(file = "decision_tree.png")

model<-ctree(  nativeSpeaker ~ age + shoeSize + score,  data = train_data)
#model<- ctree(nativeSpeaker ~ ., train_data)

#output.tree <- ctree(
#  nativeSpeaker ~ age + shoeSize + score,
#  data = input.dat)

# Plot the tree.
plot(model)

# Save the file.
dev.off()

predict_model<-predict(model, test_data)

# creates a table to count how many are classified
# as native speakers and how many are not
m_at <- table(test_data$nativeSpeaker, predict_model)
m_at
ac_test <- sum(diag(m_at)) / sum(m_at)
ac_test


# Installing package
#install.packages("caTools")       # For sampling the dataset
#install.packages("randomForest")  # For implementing random forest algorithm
#install.packages("cvms")
#install.packages("tibble")
library(cvms)
library(tibble)
# Loading package
library(caTools)
library(randomForest)
####################Random Forest###########
# Splitting data in train and test data
str(iris)
split <- sample.split(iris$Species, SplitRatio = 0.8)
str(split)
train <- subset(iris, split == "TRUE")
test <- subset(iris, split == "FALSE")
class(test)
classifier_RF <- randomForest(x = train[-5],
                              y = train$Species,
                              ntree = 500)

classifier_RF

# Predicting the Test set results
y_pred <- predict(classifier_RF, newdata = test[-5])

# Confusion Matrix
confusion_mtx <- table(test[, 5], y_pred)
confusion_mtx
ac_test_2 <- sum(diag(confusion_mtx)) / sum(confusion_mtx)
ac_test_2