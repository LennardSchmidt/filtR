context("types")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(rep(1:2, 25)), d = c(2:51), e = rep("1"), f=c(52:101))
test1 <- "a"
test2 <- "b"
test3 <- "c"
test4 <- c("d","e","f")

test_that("Within output is correct type (using df)", {
  expect_is(valid(target1 = test1, target2 = test2, df = data), "filtR")
})

test_that("Between output is correct type (using df)", {
  expect_is(valid(target1 = test1, groupvar = test3, df = data), "filtR")
})

test_that("Within output is correct type (using filtervar)", {
  expect_is(valid(target1 = test1, target2 = test2, filtervar = test4, df = data), "filtR")
})

test_that("Between output is correct type (using filtervar)", {
  expect_is(valid(target1 = test1, groupvar = test3, filtervar = test4, df = data), "filtR")
})
