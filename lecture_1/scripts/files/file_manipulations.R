lazo <- "lazo.txt"
lazo1 <- "lazo1.txt"

conn <- file(lazo, "r")

linn <-readLines(conn, warn=FALSE)
for (i in seq_along(linn)) {
  print(linn[i])
}


conn1 <- file(lazo1, "w")
writeLines(linn, conn1, sep = "\n")

close(conn)
close(conn1)

cat("lazo", file = lazo1, sep = "\n", append = TRUE)

for (i in 1:23) {
  cat(i, file = lazo1, sep = "\n", append = TRUE)
}
