context("output")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(sample(rep(1:2, 25))), d = c(2:51), e = factor(rep(3:4, 25)))

test_that("Output valid function", {
  expect_equal(c(mean(valid(effvar = "a", efffac = "b", df = data)$Sample.Size, na.rm = T),
                 mean(valid(effvar = "a", efffac = "b", df = data)$Power, na.rm = T),
                 mean(valid(effvar = "a", efffac = "b", df = data)$Effect.Size, na.rm = T)),
                 c(11.11111, 0.0729891, -0.1937876))
})

test_that("Output point function (config I)", {
  expect_equal(get_point(effvar = "a", efffac = "b", exp = c("d==50", "e == 4"), df = data), c(24, 0.06273423, -0.07071068))
})

test_that("Output point function (config II", {
  expect_equal(get_point(effvar = "a", efffac = "b", exp = c("d==13", "e == 3"), df = data), c(6, 0.08385908, -0.26726124))
})
