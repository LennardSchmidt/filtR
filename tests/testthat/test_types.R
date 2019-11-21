context("types")

#-------------------------

library(haven)
data <- data.frame(read_sav("C:/Users/LENNARD.SCHMIDT/Desktop/filtR/data/data-for_-unbounded-indirect-reciprocity_is-reputation-based-cooperation-bounded-by-group-membership_/Study1.sav"))
data$IdentificationMANIPULATION <- factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[,c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[,c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Within output is correct type (using df, full)", {
  expect_is(valid(effvar = "AllocOut", efffac = "AllocStr", df = data1), "filtR")
})

test_that("Between output is correct type (using df, full)", {
  expect_is(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", df = data2), "filtR")
})

test_that("Within output is correct type (using filtervar, full)", {
  expect_is(valid(effvar = "AllocOut", efffac = "AllocStr", filtervars = c("age", "gender"), df = data1), "filtR")
})

test_that("Between output is correct type (using filtervar, full)", {
  expect_is(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", filtervars = c("age", "gender"), df = data2), "filtR")
})

test_that("Within output is correct type (using df, sample)", {
  expect_is(valid(effvar = "AllocOut", efffac = "AllocStr", df = data1, sample = T), "filtR")
})

test_that("Between output is correct type (using df, sample)", {
  expect_is(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", df = data2, sample = T), "filtR")
})

test_that("Within output is correct type (using filtervar, sample)", {
  expect_is(valid(effvar = "AllocOut", efffac = "AllocStr", filtervars = c("age", "gender"), df = data1, sample = T), "filtR")
})

test_that("Between output is correct type (using filtervar, sample)", {
  expect_is(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", filtervars = c("age", "gender"), df = data2, sample = T), "filtR")
})
