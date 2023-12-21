a <- 3; B <- 4.5; cc <- 2; Dd <- 3.8 ;na<-NA;nan<-NaN
a + B
# [1] 7.5

a + B + cc
# [1] 9.5

a + B + cc - Dd
# [1] 5.7

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
