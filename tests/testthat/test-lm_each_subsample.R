test_that("lm_each_subsample works", {

  set.seed(141)
  fit1 <- lm_each_subsample( mpg ~ wt * hp, mtcars, n = 1000L, B = 100, use.legacy = TRUE)
  set.seed(141)
  fit2 <- lm_each_subsample( mpg ~ wt * hp, mtcars, n = 1000L, B = 100, use.legacy = FALSE)

  expect_equal( fit1, fit2 )
})
