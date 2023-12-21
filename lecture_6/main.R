data <- read.csv("binary.csv" )
str(data)
hist(data$gre)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

data$gre <- (data$gre - min(data$gre)) / (max(data$gre) - min(data$gre))
hist(data$gre)

data$gpa <- (data$gpa - min(data$gpa)) / (max(data$gpa) - min(data$gpa))
hist(data$gpa)

data$rank <- (data$rank - min(data$rank)) / (max(data$rank) - min(data$rank))
hist(data$rank)
set.seed(222)
inp <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
inp
training_data <- data[inp==1, ]
test_data <- data[inp==2, ]
library(neuralnet)
set.seed(333)
n <- neuralnet(admit~gre + gpa + rank,
               data = training_data,
               hidden = 3,
               err.fct = "ce",
               linear.output = FALSE,
               lifesign = 'full',
               rep = 3,
               algorithm = "rprop+",
               stepmax = 100000)


plot(n, rep = 3, col.hidden = 'darkgreen',
     col.hidden.synapse = 'darkgreen',
     show.weights = T,
     information = F,
     fill = 'lightblue')

n$result.matrix

head(training_data)
head(training_data[, -1])

output <- neuralnet::compute(n, rep = 1, test_data[, -1])
head(output$net.result)


# confusion Matrix $Misclassification error -Training data

p1 <- output$net.result
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(pred1, test_data$admit)
tab1


n <- neuralnet(medv ~ crim+zn+indus+chas+nox+rm+age+dis+rad+tax+ptratio+b+lstat,
               data = data,
               hidden = c(12,7),
               linear.output = F,
               lifesign = 'full',
               rep=1)

plot(n,col.hidden = 'darkgreen',
col.hidden.synapse = 'darkgreen',
show.weights = F,
information = F,
fill = 'lightblue')