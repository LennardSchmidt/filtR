context("output")

#-------------------------

data <- data.frame(haven::read_sav("./inst/extdata/Study1.sav"))
data$IdentificationMANIPULATION <- as.factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[,c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[,c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Output get_filter function (Within)", {
  expect_equal(c(mean(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1)[["results"]]$SS, na.rm = T),
                 mean(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1)[["results"]]$PO, na.rm = T),
                 mean(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1)[["results"]]$ES, na.rm = T),
                 mean(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1)[["results"]]$CL, na.rm = T),
                 mean(get_filter(outcome = "AllocOut", treatment = "AllocStr", data = data1)[["results"]]$CU, na.rm = T)),
               c(281.69135802, 0.05822814, -0.02179166, -0.31557184, 0.27198851))
})

test_that("Output get_filter function (Between)", {
  expect_equal(c(mean(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2)[["results"]]$SS, na.rm = T),
                 mean(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2)[["results"]]$PO, na.rm = T),
                 mean(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2)[["results"]]$ES, na.rm = T),
                 mean(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2)[["results"]]$CL, na.rm = T),
                 mean(get_filter(outcome = "Coop", treatment = "IdentificationMANIPULATION", data = data2)[["results"]]$CU, na.rm = T)),
               c(281.69135802, 0.09718633, -0.11553096, -0.52660735, 0.29554543))
})

test_that("Output point function (config I; lower)", {
  expect_equal(as.numeric(get_point(outcome = "Coop", treatment = "IdentificationMANIPULATION", exp = c("age == 50", "gender == 1"), data = data2)[["lower"]]),
               c(222, 0.05127711, -0.01431362, -0.28042021, 0.25179296))
})

test_that("Output point function (config I; point)", {
  expect_equal(as.numeric(get_point(outcome = "Coop", treatment = "IdentificationMANIPULATION", exp = c("age == 50", "gender == 1"), data = data2)[["point"]]),
               c(342, 0.05190539, -0.01401989, -0.22736255, 0.19932276))
})

test_that("Output point function (config I; upper)", {
  expect_equal(as.numeric(get_point(outcome = "Coop", treatment = "IdentificationMANIPULATION", exp = c("age == 50", "gender == 1"), data = data2)[["upper"]]),
               c(342, 0.05190539, -0.01401989, -0.22736255, 0.19932276))
})

test_that("Output point function (config II; no whitespace; lower)", {
  expect_equal(as.numeric(get_point(outcome = "Coop", treatment = "IdentificationMANIPULATION", exp = c("age==20", "gender== 2"), data = data2)[["lower"]]),
               c(28, 0.3207287, -0.6000259, -1.4123158, 0.2122640))
})

test_that("Output point function (config II; no whitespace; point)", {
  expect_equal(as.numeric(get_point(outcome = "Coop", treatment = "IdentificationMANIPULATION", exp = c("age==20", "gender== 2"), data = data2)[["point"]]),
               c(4, 0.05789685, -0.41379336, -4.76224741, 3.93466069))
})

test_that("Output point function (config II; no whitespace; upper)", {
  expect_equal(as.numeric(get_point(outcome = "Coop", treatment = "IdentificationMANIPULATION", exp = c("age==20", "gender== 2"), data = data2)[["upper"]]),
               c(38, 0.1562135, -0.3157161, -0.9814566, 0.3500245))
})
