context("types")

#-------------------------

data <- data.frame(haven::read_sav("C:/Users/lenna/OneDrive/Documents/Git Projects/filtR/inst/extdata/Study1.sav"))
data$IdentificationMANIPULATION <- factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[, c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[, c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Within output is correct type (using data, full)", {
  expect_is(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1), "filtR")
})

test_that("Between output is correct type (using data, full)", {
  expect_is(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2), "filtR")
})

test_that("Within output is correct type (using filtervar, full)", {
  expect_is(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1), "filtR")
})

test_that("Between output is correct type (using filtervar, full)", {
  expect_is(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2), "filtR")
})

test_that("Within output is correct type (using data, sample)", {
  expect_is(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1), "filtR")
})

test_that("Between output is correct type (using data, sample)", {
  expect_is(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2), "filtR")
})

test_that("Within output is correct type (using filtervar, sample)", {
  expect_is(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1), "filtR")
})

test_that("Between output is correct type (using filtervar, sample)", {
  expect_is(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2), "filtR")
})
