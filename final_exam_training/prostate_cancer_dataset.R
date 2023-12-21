# Prostate Cancer Dataset:
# https://drive.google.com/file/d/1oDn4Wa1Qoi9mzndzaZLlH-aIVUez_7Xz/view?usp=sharing
#
# 1. დატასეტის დაყოფა ტრეინინგ და ტესტ დატასეტებად
# 2. გამოიყენეთ Random Forest და Decision Trees ალგორითმები
# 3. გამოიყენეთ Naïve Bayes ალგორითმი
# 4. Prostate_Cancer.csv ფაილში არებული მონაცემები გადაიტანეთ dataframe- ში და ძირითადი
#    კომპონენტების ანალიზისა და პოვნის (PCA) ამოცანისათვის შეარჩიეთ Prostate_Cancer.csv ფაილის
#    c(3:9) სვეტები. გამოთვალეთ რა სიზყსტის მიღწევა შეიძლება სხვადასხვა ძირითადი კომპონენტებით (PC1, PC2..)
#    მოახდინეთ ვივიზუალიზაციის პროცესში მოახდინეთ მონაცემების დაჯგუფება და ელიფსური გრაფიკების ქვეშ გაერთიანება
#     Prostate_Cancer.csv ფაილის diagnosis_result სვეტის მიხედვით, რომელშიც მოცემული გვაქვს კლასიფიკაციის
#     მნიშვნელობები( M და B ).

library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(cvms)
library(tibble)
library(randomForest)
library(e1071)
library(caret)
library(devtools)
library(ggbiplot)

# setwd("dataset")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis_result <- factor(prc$diagnosis_result)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

y <- prc[1]
x <- prc[2:9]

x_n <- as.data.frame(lapply(x, normalize))


# 1. დატასეტის დაყოფა ტრეინინგ და ტესტ დატასეტებად
x_train <- x_n[1:90,]
x_test <- x_n[91:100,]

y_train <- y[1:90,]
y_test <- y[91:100,]



# 2. გამოიყენეთ Random Forest და Decision Trees ალგორითმები

classifier_RF <- randomForest(x = x_train,
                              y = y_train,
                              ntree = 500)

y_pred <- predict(classifier_RF, newdata = x_test)

confusion_mtx <- table(y_test, y_pred)
ac_test <- sum(diag(confusion_mtx)) / sum(confusion_mtx)
print(paste("randomForest: ", ac_test, sep=" "))


png(file = "decision_tree.png")
x_train$diagnosis_result <- y_train
model<-ctree(diagnosis_result ~ .,  data = x_train)

x_test$diagnosis_result <- y_test
predict_model<-predict(model, x_test)

m_at <- table(x_test$diagnosis_result, predict_model)
ac_test <- sum(diag(m_at)) / sum(m_at)
print(paste("ctree: ", ac_test, sep=" "))


# 3. გამოიყენეთ Naïve Bayes ალგორითმი

set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(diagnosis_result ~ ., data = x_train)

y_pred <- predict(classifier_cl, newdata = x_test)

cm <- table(y_test, y_pred)
ac_test <- sum(diag(cm)) / sum(cm)
print(paste("naiveBayes: ", ac_test, sep=" "))


# 4. Prostate_Cancer.csv ფაილში არებული მონაცემები გადაიტანეთ dataframe- ში და ძირითადი
#    კომპონენტების ანალიზისა და პოვნის (PCA) ამოცანისათვის შეარჩიეთ Prostate_Cancer.csv ფაილის
#    c(3:9) სვეტები. გამოთვალეთ რა სიზყსტის მიღწევა შეიძლება სხვადასხვა ძირითადი კომპონენტებით (PC1, PC2..)
#    მოახდინეთ ვივიზუალიზაციის პროცესში მოახდინეთ მონაცემების დაჯგუფება და ელიფსური გრაფიკების ქვეშ გაერთიანება
#     Prostate_Cancer.csv ფაილის diagnosis_result სვეტის მიხედვით, რომელშიც მოცემული გვაქვს კლასიფიკაციის
#     მნიშვნელობები( M და B ).

prc.pca <- prcomp(prc[, 3:9], center = TRUE, scale = TRUE)
summary(prc.pca)$importance


ggbiplot(prc.pca)
ggbiplot(prc.pca, labels=rownames(prc))
ggbiplot(prc.pca, labels=rownames(prc), ellipse=TRUE, groups=prc$diagnosis_result)
ggbiplot(prc.pca, labels=rownames(prc), ellipse=TRUE, groups=prc$diagnosis_result, choices=c(3,4))
ggbiplot(prc.pca, labels=rownames(prc), ellipse=TRUE, groups=prc$diagnosis_result, circle=TRUE)
ggbiplot(prc.pca, labels=rownames(prc), ellipse=TRUE, groups=prc$diagnosis_result, obs.scale = 1, var.scale = 1)
ggbiplot(prc.pca, labels=rownames(prc), ellipse=TRUE, groups=prc$diagnosis_result, obs.scale = 1, var.scale = 1, var.axes=FALSE)











