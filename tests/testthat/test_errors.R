context("errors")
#-------------------------

data <- data.frame(haven::read_sav("C:/Users/LENNARD.SCHMIDT/Desktop/filtR/inst/extdata/Study1.sav"))
data$IdentificationMANIPULATION <- as.factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[,c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[,c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Wrong exp in point function", {
  expect_error(get_point(effvar = "Coop", efffac = "IdentificationMANIPULATION", exp = c("age==10", "gender== 2"), data = data2), "Filter values do not exist in data")
})

test_that("Wrong object type in plot function", {
  expect_error(plot_filtr(data), "use only with \"filtR\" objects")
})
