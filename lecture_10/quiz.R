x <- c(3, 2, 16, 55)
y <- c(1.73, 5.0, 6.55, 8.13)
z <- c(1, 65, 27, 2)

x1 <- (x - mean(x))/sd(x)
y1 <- (y - mean(y))/sd(y)
z1 <- (z - mean(z))/sd(z)

m <- matrix(
  c(
    cov(x1, x1), cov(x1, y1), cov(x1, z1),
    cov(y1, x1), cov(y1, y1), cov(y1, z1),
    cov(z1, x1), cov(z1, y1), cov(z1, z1)
  ),
  nrow=3,
  ncol=3,
  byrow=TRUE,
  dimnames=list(
    c("x","y", "z"),
    c("x","y", "z")
  )
)

e <- eigen(m)

x <- e$values
i<-0
for (val in x) {
  i<-i+1
  xw <- val/sum(x) * 100
  pc <- paste0("PC", i)
  ans <- paste(pc, xw, sep=": ")
  if (xw > 70) {
    print(paste(ans, "(აღემატება 70%)", sep=" "))
  } else {
    print(paste(ans, "(არ აღემატება 70%)", sep=" "))
  }

}

pc1 <- x1 * e$vectors[1,1] + y1 * e$vectors[2,1] + z1 * e$vectors[3,1]
pc2 <- x1 * e$vectors[1,2] + y1 * e$vectors[2,2] + z1 * e$vectors[3,2]
pc3 <- x1 * e$vectors[1,3] + y1 * e$vectors[2,3] + z1 * e$vectors[3,3]

data.frame(PC1 = pc1, PC2 = pc2, PC3 = pc3)
