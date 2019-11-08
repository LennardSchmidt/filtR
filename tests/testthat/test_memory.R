# context("types")
#
# df <- tibble(a = c(1:200), b = c(201:400), c = factor(rep(1:2, 100)), d = c(201:400), e = c(rep("1")))
# target1 <- "a"
# target2 <- "b"
# output <- valid(target1 = target1, target2 = target2, df = df)
#
# test_that("output is correct type", {
#   expect_is(output, "tibble")
# })
