pow <- function(x, y=2) {
  # function to print x raised to the power y
  result <- x^y
  print(paste(x,"raised to the power", y, "is", result))
}

pow(2, 3)
pow(x=2, y=3)
pow(x=2)

# R Return Value from Function
check <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  return(result)
}

check(1)
check(-1)

check_2 <- function(x) {
  if (x > 0) {
    result <- "Positive"
  }
  else if (x < 0) {
    result <- "Negative"
  }
  else {
    result <- "Zero"
  }
  result
}

check_2(1)
check_2(-1)

# Multiple Returns
multi_return <- function() {
  my_list <- list("color" = "red", "size" = 20, "shape" = "round")
  return(my_list)
}

a <- multi_return()
a$color
a$size
a$shape

# S3 class and S3 objects
s <- list(name = "John", age = 21, GPA = 3.5)
class(s) <- "student"
s

# a constructor function for the "student" class
student <- function(n,a,g) {

  if (typeof(n) != "character") stop("name must be string")
  # we can add our own integrity checks
  if(g>4 || g<0)  stop("GPA must be between 0 and 4")
  value <- list(name = n, age = a, GPA = g)
  # class can be set using class() or attr() function
  attr(value, "class") <- "student"
  value
}

s <- student("lazo", 26, 3.7)
s

# Methods and Generic Functions
print.student <- function(obj) {
  cat(obj$name, "\n")
  cat(obj$age, "years old\n")
  cat("GPA:", obj$GPA, "\n")
}

s

grade <- function(obj) {
  UseMethod("grade")
}

grade.default <- function(obj) {
  cat("This is a generic function\n")
}

grade.student <- function(obj) {
  cat("Your grade is", obj$GPA, "\n")
}

grade(s)

# R S4 Class
student_1 <- setClass("student_1", slots=list(name="character", age="numeric", GPA="numeric"))

s_1 <- new("student_1",name="John", age=21, GPA=3.5)
s_1

isS4(s_1)

student_1(name="John", age=21, GPA=3.5)
s_1@name
s_1@GPA <- 3.7
s_1@name <- "lazo"
s_1

slot(s_1,"name")
slot(s_1,"name") <- "Paul"
s_1

setMethod("show", "student_1",
          function(object) {
            cat(object@name, "\n")
            cat(object@age, "years old\n")
            cat("GPA:", object@GPA, "\n")
          }
)

s_2 <- new("student_1",name="John", age=21, GPA=3.5)
s_2

# R Reference Class
setRefClass("student_2", fields = list(name = "character", age = "numeric", GPA = "numeric"))

student_2 <- setRefClass("student_2", fields = list(name = "character", age = "numeric", GPA = "numeric"))
s_3 <- student_2(name = "John", age = 21, GPA = 3.5)
s_3
s_4 <- s_3
s_3$name
s_3$name <- "Paul"
s_3
s_4

a <- list("x" = 1, "y" = 2)
b <- a
b$y <- 3
a

a <- student_2(name = "John", age = 21, GPA = 3.5)
b <- a$copy()
b$name <- "Paul"
a
b

# Reference Methods
student_2 <- setRefClass("student_2",
                       fields = list(name = "character", age = "numeric", GPA = "numeric"),
                       methods = list(
                         inc_age = function(x) {
                           age <<- age + x
                         },
                         dec_age = function(x) {
                           age <<- age - x
                         }
                       )
)

s_5 <- student_2(name = "John", age = 21, GPA = 3.5)

s_5$inc_age(5)
s_5$age

s_5$dec_age(10)
s_5$age


# R Inheritance
sup_student <- function(n,a,g) {
  value <- list(name=n, age=a, GPA=g)
  attr(value, "class") <- "student"
  value
}
print.sup_student <- function(obj) {
  cat(obj$name, "\n")
  cat(obj$age, "years old\n")
  cat("GPA:", obj$GPA, "\n")
}

s_7 <- list(name="John", age=21, GPA=3.5, country="France")
class(s_7) <- c("InternationalStudent","student")
s_7

print.InternationalStudent <- function(obj) {
  cat(obj$name, "is from", obj$country, "\n")
}
s_7


# Inheritance in S4 Class
setClass("myStudent", slots=list(name="character", age="numeric", GPA="numeric"))

# define class method for the show() generic function
setMethod("show",
          "myStudent",
          function(object) {
            cat(object@name, "\n")
            cat(object@age, "years old\n")
            cat("GPA:", object@GPA, "\n")
          }
)

setClass("InternationalStudent",
         slots=list(country="character"),
         contains="myStudent"
)

setMethod("show",
          "InternationalStudent",
          function(object) {
            cat("is form ", object@country, "\n")
          }
)

s_8 <- new("InternationalStudent",name="John", age=21, GPA=3.5, country="France")
print("aaaaaaaaaaaaaaaaaaa")
show(s_8)

# Inheritance in Reference Class
student <- setRefClass("student_7",
                       fields=list(name="character", age="numeric", GPA="numeric"),
                       methods=list(
                         inc_age = function(x) {
                           age <<- age + x
                         },
                         dec_age = function(x) {
                           age <<- age - x
                         }
                       )
)

InternationalStudent1 <- setRefClass("InternationalStudent1",
                                    fields=list(country="character"),
                                    contains="student_7",
                                    methods=list(
                                      dec_age = function(x) {
                                        if((age - x)<0)  stop("Age cannot be negative")
                                        age <<- age - x
                                      }
                                    )
)


s10 <- InternationalStudent1(name="John", age=21, GPA=3.5, country="France")
s10