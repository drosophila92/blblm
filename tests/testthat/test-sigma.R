test_that("sigma.blblm works", {

  set.seed(141)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)

  est <- fit1$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma"))))
  expect_equal( sigma(fit1), sigma )

  level <- 0.141*5
  alpha <- 1 - level
  limits <- est %>% # use future maps
    map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
    set_names(NULL)
  ans <-  c(sigma = sigma, lwr = limits[1], upr = limits[2])

  expect_equal( sigma(fit1, confidence = TRUE, level = level), ans )


})
