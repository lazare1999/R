library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(dplyr)       # for data manipulation (dplyr)
library(broom)       # for making model summary tidy
library(visreg)      # for potting logodds and probability
library(margins)     # to calculate Average Marginal Effects
library(rcompanion)  # to calculate pseudo R2
library(ROCR)        # to compute and plot Reciever Opering Curve


prc <- read.csv("Prostate_Cancer.csv", stringsAsFactors = FALSE)
prc <- prc[-1]

prc$diagnosis_result <- factor(prc$diagnosis_result)
# prc$diagnosis_result <- factor(prc$diagnosis_result, levels = 0:1)


prc <- na.omit(prc) #removing NA values
levels(prc$diagnosis_result) <- 0:1

n <- nrow(prc)
n_train <- round(0.90 * n)
set.seed(123)
train_indices <- sample(1:n, n_train)
train <- prc[train_indices, ]
test <- prc[-train_indices, ]

model_logi <- glm(diagnosis_result~., data = train, family = "binomial")

pred <- predict(model_logi, test, type="response")
predicted <- round(pred)
tab <- table(Predicted = predicted, Reference = test$diagnosis_result)

ans <- sum(diag(tab)) / sum(tab)
ans