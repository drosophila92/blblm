# compute the regression estimates for a blb dataset
# use fastLm.R (fastLm.cpp under the hood) from RcppArmadillo package

# use the fact that weighted linear regression is equivalent to
# ordinary linear regression after transformation

# Let W be a diagonal matrix of weights
# X_til = W^(1/2) %*% X
# Y_til = W^(1/2) %*% Y # this can be optimized by element-wise product between vectors

fastlm1 <- function(X, y, n) {
  freqs <- as.vector( rmultinom( 1, n, rep( 1, nrow(X) ) ) )
  fit <- fastwLm( X, y, freqs )
  list( coef = blbcoef(fit), sigma = blbsigma(fit) )
}