#' @import purrr
#' @import stats
#' @import furrr
#' @import future
#' @importFrom magrittr %>%
#' @importFrom utils "capture.output"
#' @details
#' Linear Regression with Little Bag of Bootstraps
#' @aliases blblm-package
#'
"_PACKAGE"


## quiets concerns of R CMD check re: the .'s that appear in pipelines
# from https://github.com/jennybc/googlesheets/blob/master/R/googlesheets.R
utils::globalVariables(c("."))


#' @title Linear regression with Bag of Little Bootstraps (BLB)
#' @description to be filled
#' @param formula an object of class [formula]: a symbolic description of the model to be fitted.
#' @param data a data frame containing the variables in the model
#' @param m an integer specifying the number of chunks (Default 10) the data will be sliced into.
#' @param B an integer specifying the number of bootstraps (Default 5000) done within each sub-sample.
#' @param parallel an integer specifying number of parallel processes (Default [future::availableCores()]) used. Set parallel = 1L to turn off.
#' @return
#' blblm returns an object of class "blblm".
#'
#' Like object of class "lm", the generic accessor functions [coef], [confint], [sigma], [predict] extract various useful features of the value returned by `blblm`.
#' @export
#' @examples
#' data(mtcars)
#' blblm(mpg ~ wt * hp, data = mtcars, m = 3, B = 100)
#' closeAllConnections() # close all connections to clean up


# blb <- function( ..., method = c("lm", "rf"(too hard), "logistic"(maybe?) ) )?

blblm <- function( formula, data, m = 10L, B = 5000L, parallel = availableCores() ) {
  data_list <- split_data( data, m )

  if( parallel > 1L )
    plan( multisession, workers = min( m , parallel, availableCores() ) ) # specify with the number of cores (devtools::check() will only use 2 cores by default)
  else
    plan( sequential )

  estimates <- future_map(
    data_list,
    ~ lm_each_subsample( formula = formula, data = ., n = nrow( data ), B = B ),
    .options = furrr_options( seed = TRUE )
  )

  res <- list( estimates = estimates, formula = formula )
  class( res ) <- "blblm"
  invisible( res )

}


# split data into m parts of approximated equal sizes.

split_data <- function( data, m ) {
  idx <- sample.int( m, nrow( data ), replace = TRUE )
  data %>% split( idx )
}

# legacy blblm function

blblm.old <- function(formula, data, m = 10L, B = 5000L) {
  data_list <- split_data(data, m)
  estimates <- map(
    data_list,
    ~ lm_each_subsample(formula = formula, data = ., n = nrow(data), B = B))
  res <- list(estimates = estimates, formula = formula)
  class(res) <- "blblm"
  invisible(res)
}

# compute bootstrap estimates in sub-samples.

lm_each_subsample <- function( formula, data, n, B ) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment( formula ) <- environment()
  m <- model.frame( formula, data )
  X <- model.matrix( formula, m )
  y <- model.response( m )
  replicate( B, lm1( X, y, n ), simplify = FALSE )
}


# compute the regression estimates for a blb dataset

lm1 <- function(X, y, n) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  fit <- lm.wfit(X, y, freqs) # could improve the lm.wfit by Rcpp
  # make fake X, y, freqs data
  # debugonce lm.wfit, see which parts get evaluated
  list(coef = blbcoef(fit), sigma = blbsigma(fit))
}

# compute the coefficients from fit

blbcoef <- function(fit) {
  coef(fit)
}


# compute sigma from fit

blbsigma <- function(fit) {
  p <- fit$rank
  e <- fit$residuals
  w <- fit$weights
  ## could be?? improved by Rcpp?
  sqrt(sum(w * (e^2)) / (sum(w) - p))
}


#' @title to be filled
#' @description to be filled
#' @param x  to be filled
#' @param ... to be filled
#' @return to be filled
#' @method print blblm
#' @export

print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
}


#' @title to be filled
#' @description to be filled
#' @param object  to be filled
#' @param confidence to be filled
#' @param level to be filled
#' @param ... to be filled
#' @return to be filled
#' @method sigma blblm
#' @export

sigma.blblm <- function(object, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  sigma <- mean(map_dbl(est, ~ mean(map_dbl(., "sigma")))) # use map_mean
  if (confidence) {
    alpha <- 1 - level
    limits <- est %>% # use future maps
      map_mean(~ quantile(map_dbl(., "sigma"), c(alpha / 2, 1 - alpha / 2))) %>%
      set_names(NULL)
    return(c(sigma = sigma, lwr = limits[1], upr = limits[2]))
  } else {
    return(sigma)
  }
}
#' @title to be filled
#' @description to be filled
#' @param object  to be filled
#' @param ... to be filled
#' @return to be filled
#' @method coef blblm
#' @export

coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' @title to be filled
#' @description to be filled
#' @param object  to be filled
#' @param parm to be filled
#' @param level to be filled
#' @param ... to be filled
#' @return to be filled
#' @method confint blblm
#' @export

confint.blblm <- function(object, parm = NULL, level = 0.95, ...) {
  if (is.null(parm)) {
    parm <- attr(terms(object$formula), "term.labels")
  }
  alpha <- 1 - level
  est <- object$estimates
  # plan something
  out <- map_rbind(parm, function(p) {
    # use future map for map_mean
    map_mean(est, ~ map_dbl(., list("coef", p)) %>% quantile(c(alpha / 2, 1 - alpha / 2)))

  })
  # stop clusters
  if (is.vector(out)) {
    out <- as.matrix(t(out))
  }
  dimnames(out)[[1]] <- parm
  out
}

#' @title to be filled
#' @description to be filled
#' @param object  to be filled
#' @param new_data to be filled
#' @param confidence to be filled
#' @param level to be filled
#' @param ... to be filled
#' @return to be filled
#' @method predict blblm
#' @export

predict.blblm <- function(object, new_data, confidence = FALSE, level = 0.95, ...) {
  est <- object$estimates
  X <- model.matrix(reformulate(attr(terms(object$formula), "term.labels")), new_data)
  if (confidence) {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>%
      apply(1, mean_lwr_upr, level = level) %>%
      t())
  } else {
    map_mean(est, ~ map_cbind(., ~ X %*% .$coef) %>% rowMeans())
  }
}


mean_lwr_upr <- function(x, level = 0.95) {
  alpha <- 1 - level
  c(fit = mean(x), quantile(x, c(alpha / 2, 1 - alpha / 2)) %>% set_names(c("lwr", "upr")))
}

map_mean <- function(.x, .f, ...) {
  (map(.x, .f, ...) %>% reduce(`+`)) / length(.x)
}

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
