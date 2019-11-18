context("output")

#-------------------------

library(haven)
data <- data.frame(read_sav("C:/Users/LENNARD.SCHMIDT/Desktop/filtR/data/data-for_-unbounded-indirect-reciprocity_is-reputation-based-cooperation-bounded-by-group-membership_/Study1.sav"))
data$IdentificationMANIPULATION <- as.factor(data$IdentificationMANIPULATION)
data$gender <- as.factor(data$gender)
data1 <- data[,c("AllocOut", "AllocStr", "age", "gender")]
data2 <- data[,c("IdentificationMANIPULATION", "Coop", "age", "gender")]

#-------------------------

test_that("Output valid function (Within)", {
  expect_equal(c(mean(valid(effvar = "AllocOut", efffac = "AllocStr", df = data1)$Sample.Size, na.rm = T),
                 mean(valid(effvar = "AllocOut", efffac = "AllocStr", df = data1)$Power, na.rm = T),
                 mean(valid(effvar = "AllocOut", efffac = "AllocStr", df = data1)$Effect.Size, na.rm = T)),
               c(226.27777778, 0.05878874, -0.02189552))
})

test_that("Output valid function (Between)", {
  expect_equal(c(mean(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", df = data2)$Sample.Size, na.rm = T),
                 mean(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", df = data2)$Power, na.rm = T),
                 mean(valid(effvar = "Coop", efffac = "IdentificationMANIPULATION", df = data2)$Effect.Size, na.rm = T)),
               c(226.27777778, 0.9866392, 1.8370969))
})

test_that("Output point function (config I)", {
  expect_equal(get_point(effvar = "Coop", efffac = "IdentificationMANIPULATION", exp = c("age == 50", "gender == 1"), df = data2),
               c(342, 0.05190539, -0.01401989))
})

test_that("Output point function (config II, no whitespace)", {
  expect_equal(get_point(effvar = "Coop", efffac = "IdentificationMANIPULATION", exp = c("age==20", "gender== 2"), df = data2),
               c(4, 0.05789685, -0.41379336))
})
