test_that("lm1 works", {
  n <- 1000L
  m <- model.frame( mpg ~ wt * hp, mtcars )
  X <- model.matrix( mpg ~ wt * hp, mtcars )
  y <- model.response( m )

  set.seed(141)
  w <- as.vector( rmultinom( 1, n, rep( 1, nrow(X) ) ) )
  fit <- lm.wfit(X, y, w)
  beta <- fit$coefficients

  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sigma <- sqrt( sum( w * ( e^2 ) ) / ( sum(w) - p ) )

  set.seed(141)
  fit.a <- lm1( X, y, n, use.legacy = TRUE )
  set.seed(141)
  fit.b <- lm1( X, y, n, use.legacy = FALSE )

  expect_equal( fit.a$coef,  beta )
  expect_equal( fit.a$sigma,  sigma )

  expect_equal( fit.b$coef,  beta )
  expect_equal( fit.b$sigma,  sigma )


})
