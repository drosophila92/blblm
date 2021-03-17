# a wrapper around

fastwLm <- function(X, y, w) {

  # this check is slow
  # stopifnot(is.matrix(X), is.numeric(y), is.numeric(w),
  #          nrow(y)==nrow(X), nrow(y)==nrow(w)  )

  ny <- NCOL(y)
  zero.weights <- any(w == 0)
  if (zero.weights) {
    save.r <- y
    save.w <- w
    ok <- w != 0
    nok <- !ok
    w <- w[ok]
    X0 <- X[!ok, , drop = FALSE]
    X <- X[ok, , drop = FALSE]
    n <- nrow(X)
    y0 <- if (ny > 1L) {
      y[!ok, , drop = FALSE]
    } else {
      y[!ok]
    }
    y <- if (ny > 1L) {
      y[ok, , drop = FALSE]
    } else {
      y[ok]
    }
  }


  fit <- .Call("_blblm_fastwLm_impl", PACKAGE = "blblm", X, y, w)

  coef <- fit$coefficients

  if (zero.weights) {
    coef[is.na(coef)] <- 0
    f0 <- X0 %*% coef

    save.r[ok] <- fit$residuals
    save.r[nok] <- y0 - f0

    fit$residuals <- save.r
    fit$weights <- save.w
  }

  names(fit$coefficients) <- colnames(X)
  # names(fit$residuals) <- rownames(X)
  fit
}
