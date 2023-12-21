data <- read.csv("diabetes.csv" )

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$Pregnancies <- normalize(data$Pregnancies)
data$Glucose <- normalize(data$Glucose)
data$BloodPressure <- normalize(data$BloodPressure)
data$SkinThickness <- normalize(data$SkinThickness)
data$Insulin <- normalize(data$Insulin)
data$BMI <- normalize(data$BMI)
data$DiabetesPedigreeFunction <- normalize(data$DiabetesPedigreeFunction)
data$Age <- normalize(data$Age)

set.seed(222)
inp <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
training_data <- data[inp==1, ]
test_data <- data[inp==2, ]

library(neuralnet)
set.seed(333)
n <- neuralnet(Outcome~Pregnancies + Glucose + BloodPressure + SkinThickness + Insulin + BMI + DiabetesPedigreeFunction + Age,
               data = training_data,
               hidden = 8,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 5,
               algorithm = "rprop+",
               stepmax = 1000000)

plot(n, rep = 5, col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = T,
     information = F,
     fill = 'lightblue')

n$result.matrix

head(training_data)
head(training_data[, -9])

output <- neuralnet::compute(n, rep = 1, test_data[, -9])
head(output$net.result)

# confusion Matrix $Misclassification error -Training data

p1 <- output$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, test_data$Outcome)

ac_test <- sum(diag(tab1)) / sum(tab1)
ac_test

# შედეგი: 0.6651584