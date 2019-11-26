context("types")

#-------------------------

data <- data.frame(haven::read_sav("C:/Users/LENNARD.SCHMIDT/Desktop/filtR/inst/extdata/Study1.sav"))
data$IdentificationMANIPULATION <- factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[,c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[,c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Within output is correct type (using data, full)", {
  expect_is(get_filter(effvar = "AllocOut", efffac = "AllocStr", data = data1), "filtR")
})

test_that("Between output is correct type (using data, full)", {
  expect_is(get_filter(effvar = "Coop", efffac = "IdentificationMANIPULATION", data = data2), "filtR")
})

test_that("Within output is correct type (using filtervar, full)", {
  expect_is(get_filter(effvar = "AllocOut", efffac = "AllocStr", filtervars = c("age", "gender"), data = data1), "filtR")
})

test_that("Between output is correct type (using filtervar, full)", {
  expect_is(get_filter(effvar = "Coop", efffac = "IdentificationMANIPULATION", filtervars = c("age", "gender"), data = data2), "filtR")
})

test_that("Within output is correct type (using data, sample)", {
  expect_is(get_filter(effvar = "AllocOut", efffac = "AllocStr", data = data1, sample = T), "filtR")
})

test_that("Between output is correct type (using data, sample)", {
  expect_is(get_filter(effvar = "Coop", efffac = "IdentificationMANIPULATION", data = data2, sample = T), "filtR")
})

test_that("Within output is correct type (using filtervar, sample)", {
  expect_is(get_filter(effvar = "AllocOut", efffac = "AllocStr", filtervars = c("age", "gender"), data = data1, sample = T), "filtR")
})

test_that("Between output is correct type (using filtervar, sample)", {
  expect_is(get_filter(effvar = "Coop", efffac = "IdentificationMANIPULATION", filtervars = c("age", "gender"), data = data2, sample = T), "filtR")
})
