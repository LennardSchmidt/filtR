library(testthat)
library(filtR)

test_check("filtR")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(rep(1:2, 25)), d = c(2:51), f = c(52:101))
