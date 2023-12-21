sex <- factor(c("male", "female", "female", "male"))
levels(sex)
nlevels(sex)


food <- factor(c("low", "high", "medium", "high", "low", "medium", "high"))
levels(food)

food <- factor(food, levels=c("low", "medium", "high"))
levels(food)
# min(food) ## doesn't work

food <- factor(food, levels=c("low", "medium", "high"), ordered=TRUE)
levels(food)
min(food) ## works!



# Create the vectors for data frame.
height <- c(132,151,162,139,166,147,122)
weight <- c(48,49,66,53,67,52,40)
gender <- c("male","male","female","female","male","female","male")

# Create the data frame.
input_data <- data.frame(height,weight,gender, stringsAsFactors = TRUE)
print("----------------------------------------------------------")
print(input_data)

# Test if the gender column is a factor.
print(is.factor(input_data$gender))

# Print the gender column so see the levels.
print(input_data$gender)
print("----------------------------------------------------------")


data <- c("East","West","East","North","North","East","West",
          "West","West","East","North")
# Create the factors
factor_data <- factor(data)
print(factor_data)

# Apply the factor function with required order of the level.
new_order_data <- factor(factor_data,levels = c("East","West","North"))
print(new_order_data)
