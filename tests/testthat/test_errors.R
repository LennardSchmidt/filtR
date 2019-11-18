context("errors")
#-------------------------

library(haven)
data <- data.frame(read_sav("C:/Users/LENNARD.SCHMIDT/Desktop/filtR/data/data-for_-unbounded-indirect-reciprocity_is-reputation-based-cooperation-bounded-by-group-membership_/Study1.sav"))
data$IdentificationMANIPULATION <- as.factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[,c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[,c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Wrong exp in point function", {
  expect_error(get_point(effvar = "Coop", efffac = "IdentificationMANIPULATION", exp = c("age==10", "gender== 2"), df = data2), "Filter combination does not exist in data")
})

test_that("Wrong object type in plot function", {
  expect_error(plot.valid(data), "use only with \"filtR\" objects")
})
