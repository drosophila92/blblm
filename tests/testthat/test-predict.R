test_that("predict.blblm works", {

  set.seed(141)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)

  est <- fit1$estimates
  new_data <- data.frame(wt = c(2.5, 3), hp = c(150, 170))
  X <- model.matrix(reformulate(attr(terms(fit1$formula), "term.labels")), new_data)
  ans <- map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans() )


  expect_equal( predict(fit1, new_data), ans )

  level <- 0.141*6
  ans <-
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
               apply(1, mean_lwr_upr, level = level) %>%
               t())

  expect_equal( predict(fit1, new_data, confidence = TRUE, level = level ), ans )


})
