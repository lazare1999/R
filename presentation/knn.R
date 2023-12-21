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

# Creating training and test data set
sample_data <- sample.split(cars, SplitRatio = 0.8)
train_data <- subset(cars[-14], sample_data == TRUE)
test_data <- subset(cars[-14], sample_data == FALSE)

train_l <- nrow(train_data)
test_l <- nrow(test_data)
cars_l <- nrow(cars)

cars_train_labels <- cars[1:train_l, 14]
train_l <- train_l+1
cars_test_labels <- cars[train_l:cars_l, 14]


# Step 3 – Training a model on data
library("class")
cars_test_pred <- knn(train = train_data, test = test_data, cl = cars_train_labels, k=7)


df1 <- data.frame(LableId = seq_along(cars_test_labels), classific1=cars_test_labels)
df2 <- data.frame(LableId = seq_along(cars_test_labels), classific2=cars_test_pred)
df <- merge(df1, df2)


v1<-df$classific1
v2<-df$classific2
v3 <- ifelse(v1==v2, 0, 1)
sum(v3)/length(v3)
