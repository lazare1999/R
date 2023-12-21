library(datasets)
library(caTools)
library(party)
library(dplyr)
library(magrittr)

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

sample_data <- sample.split(cars, SplitRatio = 0.8)
train_data <- subset(cars, sample_data == TRUE)
test_data <- subset(cars, sample_data == FALSE)
png(file = "decision_tree.png")


model<-ctree(Identification.Make ~ ., data = train_data)


# Plot the tree.
plot(model)

# Save the file.
dev.off()

predict_model<-predict(model, test_data)

m_at <- table(test_data$Identification.Make, predict_model)
m_at
ac_test <- sum(diag(m_at)) / sum(m_at)
ac_test