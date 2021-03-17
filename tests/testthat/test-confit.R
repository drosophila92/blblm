test_that("confint.blblm works", {

  set.seed(141)
  fit1 <- blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100, use.legacy = TRUE)

  parm <- attr(terms(fit1$formula), "term.labels")
  level <- 0.141 ^ 0.141
  alpha <- 1 - level
  est <- fit1$estimates
  ans <- map_rbind(parm, function(p) {
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))
  })
  if (is.vector(ans)) {
    ans <- as.matrix(t(ans))
  }
  dimnames(ans)[[1]] <- parm

  expect_equal( confint(fit1, parm = parm, level = level), ans )

})
