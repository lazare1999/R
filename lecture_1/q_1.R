# ამოცანა 1

Rclass <- data.frame(
  Student_id = 1:4,
  Major = c("MATH","ENGINERIING","SCIENCE","PHYSICS"),
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-11-15")),
  stringsAsFactors = FALSE
)


# ამოცანა 2
result <- Rclass[1:2,]
RclassNew <- data.frame(
  Student_id = result$Student_id,
  Major = result$Major,
  start_date = result$start_date,
  stringsAsFactors = FALSE
)
print("__________________________")
result
print("__________________________")

# ამოცანა 3
RclassAdd <- data.frame(
  Student_id = 5,
  Major = "MATH",
  start_date = as.Date("2012-01-01"),
  stringsAsFactors = FALSE
)
# Bind the two data frames.
Rclass <- rbind(Rclass,RclassAdd)
Rclass

# ამოცანა 3

# NaN - not a number
# NA - not avaible
# missing values are represented by the symbol NA (not available). Impossible values (e.g., dividing by zero) are represented by the symbol NaN (not a number).

a <- 3; B <- 4.5; na<-NA;nan<-NaN

B-nan
#[1] NaN

a+na-na
#[1] NA

a + na
#[1] NA

B-nan
#[1] NaN

a+na-na
#[1] NA
