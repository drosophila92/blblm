test_that("lm1 works", {
  n <- nrow( mtcars )
  m <- model.frame( mpg ~ wt * hp, mtcars )
  X <- model.matrix( mpg ~ wt * hp, mtcars )
  y <- model.response( m )

  set.seed(141)
  freqs <- as.vector( rmultinom( 1, n, rep( 1, nrow(X) ) ) )
  fit <- lm.wfit(X, y, freqs)
  beta <- fit$coefficients

  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  sigma <- sqrt( sum( w * ( e^2 ) ) / ( sum(w) - p ) )

  set.seed(141)
  expect_identical( lm1( X, y, n )$coef,  beta )

  set.seed(141)
  expect_identical( lm1( X, y, n )$sigma,  sigma )


})
