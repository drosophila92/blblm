test_that("coef.blblm works", {
  set.seed(141)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)
  est <- fit1$estimates
  ans <- map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
  expect_equal(coef(fit1), ans)
})
