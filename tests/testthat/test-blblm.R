# test_that("expected number of parallel process", {
#
#   fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)
#   expect_equal( fit1$parallel, 1L )
#
#   fit2 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = FALSE)
#   expect_equal( fit2$parallel, 3L )
#
#   fit3 <- blblm(mpg ~ wt * hp, data = mtcars, m = 6, B = 100, use.legacy = FALSE)
#   expect_equal( fit3$parallel, 4L )
#
#   fit4 <- blblm(mpg ~ wt * hp, data = mtcars, m = 6, B = 100, use.legacy = FALSE, parallel = 2L)
#   expect_equal( fit4$parallel, 2L )
#
#
# })

test_that("legacy mode reproducible", {

  set.seed(141)
  fit1a <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)
  set.seed(141)
  fit1b <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)
  expect_equal( fit1a, fit1b )

})

test_that("new version reproducible", {

  set.seed(141)
  fit2a <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  set.seed(141)
  fit2b <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
  expect_equal( fit2a, fit2b )

})

test_that("returned object is blblm", {

  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)
  expect_is( fit1, "blblm" )

})
