(x<-5)

v <- c(2, 3, 7, 10)
v1 <- c(1, 2, 6, 9)
v2 <- c("a", "b", "c")

v-v1


list(
  course = 'stat',
  date = '04/07/2009',
  num_isc = 7,
  num_cons = 6,
  num_mat = as.character(c(45020, 45679, 46789, 43126, 42345, 47568, 45674)),
  results = c(30, 19, 29, NA, 25, 26 ,27)
)


exam <- data.frame(
  matr = as.character(c(45020, 45679, 46789, 43126, 42345, 47568, 45674)),
  res_S = c(30, 19, 29, NA, 25, 26, 27),
  res_O = c(3, 3, 1, NA, 3, 2, NA),
  res_TOT = c(30,22,30,NA,28,28,27)
)

exam
list( '.11' ="a")

library("stringi")
stri_count_fixed("babab", "b")

