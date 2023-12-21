# Loading package
library(fpc)

# Remove label form dataset
iris_1 <- iris[-5]

# Fitting DBScan clustering Model
# to training dataset
set.seed(220)  # Setting seed
Dbscan_cl <- dbscan(iris_1, eps = 0.45, MinPts = 5)
Dbscan_cl

# Checking cluster
Dbscan_cl$cluster

# Table
table(Dbscan_cl$cluster, iris$Species)

# Plotting Cluster
plot(Dbscan_cl, iris_1, main = "DBScan")
plot(Dbscan_cl, iris_1, main = "Petal Width vs Sepal Length")