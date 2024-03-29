context("length")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(sample(rep(1:2, 25))), d = c(2:51), e = factor(rep(3:4, 25)))
data <- rbind(data, rep(NA, ncol(data)))

test_that("Within output is correct length (using data, full)", {
  expect_length(get_filter(outcome = "a", treatment = "b", data = data)[[1]], 459)
})

test_that("Between output is correct length (using data, full)", {
  expect_length(get_filter(outcome = "a", treatment = "c", data = data)[[1]], 7803)
})

test_that("Within output is correct length (using filtervar, full)", {
  expect_length(get_filter(outcome = "a", treatment = "b", filtervars = c("d", "e"), data = data)[[1]], 153)
})

test_that("Between output is correct length (using filtervar, full)", {
  expect_length(get_filter(outcome = "a", treatment = "c", filtervars = c("d", "e"), data = data)[[1]], 153)
})
