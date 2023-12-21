setwd("dataset")
prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis <- factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))

# Normalizing numeric data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

prc_n <- as.data.frame(lapply(prc[2:9], normalize))

# Creating training and test data set
prc_train <- prc_n[1:65,]
prc_test <- prc_n[66:100,]

prc_train_labels <- prc[1:65, 1]
prc_test_labels <- prc[66:100, 1]


# Step 3 – Training a model on data
library("class")
prc_test_pred <- knn(train = prc_train, test = prc_test, cl = prc_train_labels, k=7)


df1 <- data.frame(LableId = 1:35, classific1=prc_test_labels)
df2 <- data.frame(LableId = 1:35, classific2=prc_test_pred)
df <- merge(df1, df2)


v1<-df$classific1
v2<-df$classific2
v3 <- ifelse(v1==v2, 0, 1)
sum(v3)/length(v3)
