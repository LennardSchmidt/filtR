context("types")

data <- data.frame(a = c(1:50), b = c(201:250), c = factor(rep(1:2, 25)), d = c(201:250), e = c(rep("1")))
test1 <- "a"
test2 <- "b"
test3 <- "c"

test_that("Within output is correct type", {
  expect_is(valid(target1 = test1, target2 = test2, dat = data), "tibble")
})

test_that("Between output is correct type", {
  expect_is(valid(target1 = test1, groupvar = test3, df = data), "tibble")
})
