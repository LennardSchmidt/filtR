context("errors")

data <- data.frame(a = c(1:50), b = c(2:51), c = factor(rep(1:2, 25)), d = c(2:51), e = rep("1"), f = c(52:101))
test1 <- "a"
test2 <- "b"
test3 <- "c"
test4 <- c("d", "e", "f")

# test_that("Both options specified", {
#   expect_error(valid(effvar = test1, effac = test2, df = data), "Specify one option only")
# })

# test_that("No option specified", {
#   expect_error(valid(effvar = test1, df = data), "Specify one option")
# })

test_that("Wrong object type in plot", {
  expect_error(plot.valid(data), "use only with \"filtR\" objects")
})
