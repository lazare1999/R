# create matrix A and B using given equations
A <- rbind(c(1, 2, 3),
           c(2, 2, 3),
           c(3, 2, 8))
B <- c(20, 100, 200)

# Solve them using solve function in R
solve(A, B)

# R program to illustrate
# Eigenvalues and eigenvectors of matrix

# Create a 3x3 matrix
A <- matrix(1:9, 3, 3)

cat("The 3x3 matrix:\n")
print(A)

# Calculating Eigenvalues and eigenvectors
print(eigen(A))



# use a simple two dimensional dataset to illustrate PCA
x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)

plot(x, y, pch = 19)

mean(x)
# [1] 1.81
mean(y)
# [1] 1.91

x1 <- x - mean(x)
x1
# [1]  0.69 -1.31  0.39  0.09  1.29  0.49  0.19 -0.81 -0.31 -0.71
summary(x1)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
 # -1.310  -0.610   0.140   0.000   0.465   1.290

y1 <- y - mean(y)
y1
# [1]  0.49 -1.21  0.99  0.29  1.09  0.79 -0.31 -0.81 -0.31 -1.01
summary(y1)
 #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
 # -1.210  -0.685  -0.010   0.000   0.715   1.090

plot(x1, y1, pch = 19)

cov(x1, y1)
#[1] 0.6154444

cov(x1, x1)
#[1] 0.6165556

cov(y1, y1)
#[1] 0.7165556

m <- matrix(c(cov(x1, x1), cov(x1, y1), cov(y1, x1),cov(y1, y1)),
            nrow=2,
            ncol=2,
            byrow=TRUE,
            dimnames=list(c("x","y"),c("x","y")))

m
#           x         y
# x 0.6165556 0.6154444
# y 0.6154444 0.7165556

e <- eigen(m)
e
# eigen() decomposition
# $values
# [1] 1.2840277 0.0490834
#
# $vectors
#           [,1]       [,2]
# [1,] 0.6778734 -0.7351787
# [2,] 0.7351787  0.6778734

pc1 <- x1 * e$vectors[1,1] + y1 * e$vectors[2,1]
pc1
#  [1]  0.82797019 -1.77758033  0.99219749  0.27421042  1.67580142  0.91294910 -0.09910944 -1.14457216 -0.43804614
# [10] -1.22382056

pc2 <- x1 * e$vectors[1,2] + y1 * e$vectors[2,2]
pc2
#  [1] -0.17511531  0.14285723  0.38437499  0.13041721 -0.20949846  0.17528244 -0.34982470  0.04641726  0.01776463
# [10] -0.16267529

data.frame(PC1 = pc1, PC2 = pc2)
#            PC1         PC2
# 1   0.82797019 -0.17511531
# 2  -1.77758033  0.14285723
# 3   0.99219749  0.38437499
# 4   0.27421042  0.13041721
# 5   1.67580142 -0.20949846
# 6   0.91294910  0.17528244
# 7  -0.09910944 -0.34982470
# 8  -1.14457216  0.04641726
# 9  -0.43804614  0.01776463
# 10 -1.22382056 -0.16267529

plot(pc1, pc2, pch = 19)

data <- data.frame(x,y)
data.pca <- prcomp(data)
data.pca
# Standard deviations (1, .., p=2):
# [1] 1.1331495 0.2215477
#
# Rotation (n x k) = (2 x 2):
#          PC1        PC2
# x -0.6778734  0.7351787
# y -0.7351787 -0.6778734

names(data.pca)
# [1] "sdev"     "rotation" "center"   "scale"    "x"

data.pca$x
#              PC1         PC2
# [1,] -0.82797019  0.17511531
# [2,]  1.77758033 -0.14285723
# [3,] -0.99219749 -0.38437499
# [4,] -0.27421042 -0.13041721
# [5,] -1.67580142  0.20949846
# [6,] -0.91294910 -0.17528244
# [7,]  0.09910944  0.34982470
# [8,]  1.14457216 -0.04641726
# [9,]  0.43804614 -0.01776463
#[10,]  1.22382056  0.16267529

plot(data.pca$x[,1], data.pca$x[,2], pch = 19)

eigen(m)
# eigen() decomposition
# $values
# [1] 1.2840277 0.0490834
#
# $vectors
#           [,1]       [,2]
# [1,] 0.6778734 -0.7351787
# [2,] 0.7351787  0.6778734

data.pca
# Standard deviations (1, .., p=2):
# [1] 1.1331495 0.2215477
#
# Rotation (n x k) = (2 x 2):
#          PC1        PC2
# x -0.6778734  0.7351787
# y -0.7351787 -0.6778734