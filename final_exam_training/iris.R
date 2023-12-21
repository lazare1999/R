# Iris Dataset
# https://drive.google.com/file/d/13jFXlYnM6NhdZj4lpyc7NoZtMY28nmST/view?usp=sharing
# 1. დატასეტის დაყოფა ტრეინინგ და ტესტ დატასეტებად
# 2. გამოიყენეთ K-NN და Decision Trees ალგორითმები
# 3. გამოიყენეთ Naïve Bayes ალგორითმი
# 4. IRIS.csv ფაილში არებული მონაცემები გადაიტანეთ dataframe- ში და ძირითადი კომპონენტების ანალიზისა და
#    პოვნის (PCA) ამოცანისათვის შეარჩიეთ IRIS.csv ფაილის c(1:4) სვეტები. გამოთვალეთ თუ რა სიზუსტით ახასიათებენ
#    მონაცემებს PC1, PC2, PC3 კომპონენტები. მონაცემთა ვიზუალიზაციისათვის გამოიყენეთ ggbiplot ბიბლიოთეკა.
#    მოახდინეთ მონაცემების ვიზუალიზაცია. ვიზუალიზაციის პროცესში, კოორდინატების მნიშვნელობების როლში აიღეთ PC2, PC3
#    ძირითადი კომპონენტების მნიშვნელობები, მოახდინეთ მონაცემების გრაფიკული დაჯგუფება და ელიფსური გრაფიკების ქვეშ გაერთიანება
#    IRIS.csv ფაილის მე-5 სვეტის მიხედვით, რომელშიც მოცემული გვაქვს კლასიფიკაციის მნიშვნელობები( Iris-setosa, Iris-versicolor
#    და Iris-virginica ).

library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)
library(e1071)
library(caret)
library(devtools)
library(ggbiplot)
library("class")

iris$Species <- factor(iris$Species)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

y <- iris[5]
x <- iris[1:4]


x_n <- as.data.frame(lapply(x, normalize))


# 1. დატასეტის დაყოფა ტრეინინგ და ტესტ დატასეტებად
x_train <- x_n[1:130,]
x_test <- x_n[131:150,]

y_train <- y[1:130,]
y_test <- y[131:150,]

# 2. გამოიყენეთ K-NN და Decision Trees ალგორითმები
y_test_pred <- knn(train = x_train, test = x_test, cl = y_train, k=8)

df1 <- data.frame(LableId = 1:20, classific1=y_test)
df2 <- data.frame(LableId = 1:20, classific2=y_test_pred)
df <- merge(df1, df2)

v1<-df$classific1
v2<-df$classific2
v3 <- ifelse(v1==v2, 0, 1)
ac_test <- 1-sum(v3)/length(v3)
print(paste("knn: ", ac_test, sep=" "))


png(file = "decision_tree.png")

x_train$Species <- y_train
model<-ctree(Species ~ .,  data = x_train)

x_test$Species <- y_test
predict_model<-predict(model, x_test)

m_at <- table(x_test$Species, predict_model)
ac_test <- sum(diag(m_at)) / sum(m_at)
print(paste("ctree: ", ac_test, sep=" "))

# 3. გამოიყენეთ Naïve Bayes ალგორითმი
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(Species ~ ., data = x_train)

y_pred <- predict(classifier_cl, newdata = x_test)

cm <- table(y_test, y_pred)
ac_test <- sum(diag(cm)) / sum(cm)
print(paste("naiveBayes: ", ac_test, sep=" "))

# 4. IRIS.csv ფაილში არებული მონაცემები გადაიტანეთ dataframe- ში და ძირითადი კომპონენტების ანალიზისა და
#    პოვნის (PCA) ამოცანისათვის შეარჩიეთ IRIS.csv ფაილის c(1:4) სვეტები. გამოთვალეთ თუ რა სიზუსტით ახასიათებენ
#    მონაცემებს PC1, PC2, PC3 კომპონენტები. მონაცემთა ვიზუალიზაციისათვის გამოიყენეთ ggbiplot ბიბლიოთეკა.
#    მოახდინეთ მონაცემების ვიზუალიზაცია. ვიზუალიზაციის პროცესში, კოორდინატების მნიშვნელობების როლში აიღეთ PC2, PC3
#    ძირითადი კომპონენტების მნიშვნელობები, მოახდინეთ მონაცემების გრაფიკული დაჯგუფება და ელიფსური გრაფიკების ქვეშ გაერთიანება
#    IRIS.csv ფაილის მე-5 სვეტის მიხედვით, რომელშიც მოცემული გვაქვს კლასიფიკაციის მნიშვნელობები( Iris-setosa, Iris-versicolor
#    და Iris-virginica ).

iris.pca <- prcomp(iris[, 1:4], center = TRUE, scale = TRUE)

summary(iris.pca)$importance

ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, choices=c(2,3))

ggbiplot(iris.pca)
ggbiplot(iris.pca, labels=rownames(iris))
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, circle=TRUE)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, obs.scale = 1, var.scale = 1)
ggbiplot(iris.pca, labels=rownames(iris), ellipse=TRUE, groups=iris$Species, obs.scale = 1, var.scale = 1, var.axes=FALSE)
