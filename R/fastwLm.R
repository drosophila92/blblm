# a wrapper around

fastwLm <- function(X, y, w) {

  # this check is slow
  # stopifnot(is.matrix(X), is.numeric(y), is.numeric(w),
  #          nrow(y)==nrow(X), nrow(y)==nrow(w)  )

  fit <- .Call('_blblm_fastwLm_impl', PACKAGE = 'blblm', X, y, w)
  names(fit$coefficients) <- colnames(X)
  #names(fit$residuals) <- rownames(X)
  fit

}