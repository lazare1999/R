library(mlbench)     # for PimaIndiansDiabetes2 dataset
library(dplyr)       # for data manipulation (dplyr)
library(broom)       # for making model summary tidy
library(visreg)      # for potting logodds and probability
library(margins)     # to calculate Average Marginal Effects
library(rcompanion)  # to calculate pseudo R2
library(ROCR)        # to compute and plot Reciever Opering Curve


# load the diabetes dataset
data(PimaIndiansDiabetes2)
head(PimaIndiansDiabetes2)


# See the data strcuture
glimpse(PimaIndiansDiabetes2)


Diabetes <- na.omit(PimaIndiansDiabetes2) #removing NA values
levels(Diabetes$diabetes) <- 0:1
glimpse(Diabetes)


# Total number of rows in the Diabetes data frame
n <- nrow(Diabetes)
# Number of rows for the training set (80% of the dataset)
n_train <- round(0.80 * n)
# Create a vector of indices which is an 80% random sample
set.seed(123)
train_indices <- sample(1:n, n_train)
# Subset the Diabetes data frame to training indices only
train <- Diabetes[train_indices, ]
# Exclude the training indices to create the test set
test <- Diabetes[-train_indices, ]





paste("train sample size: ", dim(train)[1])
paste("test sample size: ", dim(test)[1])


#Fitting a binary logistic regression
model_logi <- glm(diabetes~., data = train, family = "binomial")
#Model summary
summary(model_logi)



# predict the test dataset
pred <- predict(model_logi, test, type="response")
pred
predicted <- round(pred) # round of the value; >0.5 will convert to
predicted
# 1 else 0
# Creating a contigency table
tab <- table(Predicted = predicted, Reference = test$diabetes)
tab





#Linear Regression
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Apply the lm() function.
relation <- lm(y~x)

print(relation)


# Find weight of a person with height 170.
a <- data.frame(x = 170)
a
result <-  predict(relation,a)
print(result)


# Multiple Linear Regression
input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))

input <- mtcars[,c("mpg","disp","hp","wt")]

# Create the relationship model.
model <- lm(mpg~disp+hp+wt, data = input)

# Show the model.
print(model)

# Get the Intercept and coefficients as vector elements.
cat("# # # # The Coefficient Values # # # ","\n")

a <- coef(model)[1]
print(a)

Xdisp <- coef(model)[2]
Xhp <- coef(model)[3]
Xwt <- coef(model)[4]

print(Xdisp)
print(Xhp)
print(Xwt)
head(input)
#Y = a+Xdisp.x1+Xhp.x2+Xwt.x3
Ypr <- a+Xdisp*225+Xhp*105+Xwt*3.460
Ypr
