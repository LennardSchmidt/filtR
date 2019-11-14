context("types")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(rep(1:2, 25)), d = c(2:51), e = factor(rep(3:4,25)), f=c(52:101))

test_that("Within output is correct type (using df)", {
  expect_is(valid(effvar = "a", efffac = "b", df = data), "filtR")
})

test_that("Between output is correct type (using df)", {
  expect_is(valid(effvar = "a", efffac = "c", df = data), "filtR")
})

test_that("Within output is correct type (using filtervar)", {
  expect_is(valid(effvar = "a", efffac = "b", filtervars = c("d","e","f"), df = data), "filtR")
})

test_that("Between output is correct type (using filtervar)", {
  expect_is(valid(effvar = "a", efffac = "c", filtervars = c("d","e","f"), df = data), "filtR")
})
