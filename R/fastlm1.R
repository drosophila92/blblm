# compute the regression estimates for a blb dataset
# use fastLm.R (fastLm.cpp under the hood) from RcppArmadillo package

# use the fact that weighted linear regression is equivalent to
# ordinary linear regression after transformation

# Let W be a diagonal matrix of weights
# X_til = W^(1/2) %*% X
# Y_til = W^(1/2) %*% Y

fastlm1 <- function(X, y, n) {
  freqs <- as.vector( rmultinom( 1, n, rep( 1, nrow(X) ) ) )
  new_X <- diag( sqrt( freqs ) ) %*% X
  new_y <- diag( sqrt( freqs ) ) %*% y

  # fit <- lm.fit( X, y )
   fit <- RcppArmadillo::fastLm( new_X, new_y )
  # fit <- lm.wfit(X, y, freqs) # could improve the lm.wfit by Rcpp

  list(coef = blb_coef(fit), sigma = blb_sigma(fit) )
}