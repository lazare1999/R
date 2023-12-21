# setwd("KNN")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

prc_n <- as.data.frame(lapply(prc[2:9], normalize))

prc_train <- prc_n[1:75,]
prc_test <- prc_n[76:100,]

prc_train_labels <- prc[1:75, 1]
prc_test_labels <- prc[76:100, 1]


library("class")
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=7)


df1 <- data.frame(LableId = 1:25, classific1=prc_test_labels)
df2 <- data.frame(LableId = 1:25, classific2=prc_test_pred)
df <- merge(df1, df2)


v1<-df$classific1
v2<-df$classific2
v3 <- ifelse(v1==v2, 1, 0)
result <- sum(v3)/length(v3)
print(paste("KNN Accuracy =",sprintf("%1.2f%%", 100*result)))
