context("types")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(sample(rep(1:2, 25))), d = c(2:51), e = factor(rep(3:4, 25)))

test_that("Within output is correct type (using df, full)", {
  expect_is(valid(effvar = "a", efffac = "b", df = data), "filtR")
})

test_that("Between output is correct type (using df, full)", {
  expect_is(valid(effvar = "a", efffac = "c", df = data), "filtR")
})

test_that("Within output is correct type (using filtervar, full)", {
  expect_is(valid(effvar = "a", efffac = "b", filtervars = c("d", "e"), df = data), "filtR")
})

test_that("Between output is correct type (using filtervar, full)", {
  expect_is(valid(effvar = "a", efffac = "c", filtervars = c("d", "e"), df = data), "filtR")
})

test_that("Within output is correct type (using df, sample)", {
  expect_is(valid(effvar = "a", efffac = "b", df = data, sample = T), "filtR")
})

test_that("Between output is correct type (using df, sample)", {
  expect_is(valid(effvar = "a", efffac = "c", df = data, sample = T), "filtR")
})

test_that("Within output is correct type (using filtervar, sample)", {
  expect_is(valid(effvar = "a", efffac = "b", filtervars = c("d", "e"), df = data, sample = T), "filtR")
})

test_that("Between output is correct type (using filtervar, sample)", {
  expect_is(valid(effvar = "a", efffac = "c", filtervars = c("d", "e"), df = data, sample = T), "filtR")
})
