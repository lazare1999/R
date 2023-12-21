library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

setwd("dataset")
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