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
#' @param use.legacy an logical specifying whether use the legacy version of function (Default NA to use optimized one).
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

blblm <- function( formula, data, m = 10L, B = 5000L, use.legacy = NA, parallel = availableCores() ) {
  data_list <- split_data( data, m )

  if( is.na( use.legacy ) )
    use.legacy = prod( dim(data), m, B ) < 1e6


  if( !use.legacy & parallel > 1L ){
    plan( multisession, workers = min( m , parallel, availableCores() ) ) # specify with the number of cores (devtools::check() will only use 2 cores by default)
    estimates <- future_map(
      data_list,
      ~ lm_each_subsample( formula = formula, data = ., n = nrow( data ), B = B ),
      .options = furrr_options( seed = TRUE )
    )
  }

  else{
    estimates <- map(
      data_list,
      ~ lm_each_subsample(formula = formula, data = ., n = nrow( data ), B = B ) )
  }

  res <- list( estimates = estimates, formula = formula )
  class( res ) <- "blblm"
  invisible( res )

}


# split data into m parts of approximated equal sizes.

split_data <- function( data, m ) {
  idx <- sample.int( m, nrow( data ), replace = TRUE )
  data %>% split( idx )
}

# compute bootstrap estimates in sub-samples.

lm_each_subsample <- function( formula, data, n, B, use.legacy = FALSE ) {
  # drop the original closure of formula,
  # otherwise the formula will pick a wrong variable from the global scope.
  environment( formula ) <- environment()
  m <- model.frame( formula, data )
  X <- model.matrix( formula, m )
  y <- model.response( m )
  replicate( B, lm1( X, y, n, use.legacy = use.legacy ), simplify = FALSE )
}


# compute the regression estimates for a blb dataset

lm1 <- function(X, y, n, use.legacy = FALSE) {
  freqs <- as.vector(rmultinom(1, n, rep(1, nrow(X))))
  if( use.legacy )
    fit <- lm.wfit( X, y, freqs )
  else
    fit <- fastwLm( X, y, freqs )

  list( coef = blbcoef(fit), sigma = blbsigma(fit) )

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


#' @title Print the BLB linear regression model
#' @description print out the model formula and regression coefficients of a blblm object to the console
#' @param x  a blblm object
#' @param ... additional arguments to be passed
#' @method print blblm
#' @export

print.blblm <- function(x, ...) {
  cat("blblm model:", capture.output(x$formula))
  cat("\n")
  cat("Coefficients:", capture.output(x$estimates))
  cat("\n")

}


#' @title BLB estimator of error term standard deviation (\eqn{\sigma})
#' @description Calculate the point estimation of error term standard deviation (\eqn{\sigma}), with the option to show the confidence internal by user specified confidence level.
#' @param object  a blblm object returned by function `blblm`
#' @param confidence logical, whether to show confidence interval around the point estimate (Default: FALSE)
#' @param level numeric, a number range from (0,1) as the confidence level (Default: 0.95)
#' @param ... additional arguments to be passed
#' @return A single point estimate of error term standard deviation (\eqn{\sigma}). If `confidence=TRUE`, also shows the confidence interval around the point estimation.
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

#' @title BLB estimator of regression coefficients (\eqn{\beta} s)
#' @description Calculate the point estimation of regression coefficients (\eqn{\beta} s).
#' @param object  a blblm object returned by function `blblm`
#' @param ... additional arguments to be passed
#' @return a dataframe of estimated regression coefficients.
#' @method coef blblm
#' @export

coef.blblm <- function(object, ...) {
  est <- object$estimates
  map_mean(est, ~ map_cbind(., "coef") %>% rowMeans())
}

#' @title BLB confidence intervals of regression coefficients (\eqn{\beta} s)
#' @description Calculate the BLB confidence intervals of regression coefficients (\eqn{\beta} s).
#' @param object  a blblm object
#' @param parm a charactor vector, names of parameters (i.e. regression coefficients) whose CI is to be calculated.
#' @param level numeric, a number range from (0,1) as the confidence level (Default: 0.95)
#' @param ... additional arguments to be passed.
#' @return a matrix containing CI estimates on user-specified parameters (`parm`) with user-specified confidence level (`level`)
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

#' @title Predict method for BLB Model Fits
#' @description Predicted values based on BLB model object.
#' @param object  Object of class inheriting from "blblm"
#' @param new_data A data frame in which to look for variables with which to predict.
#' @param confidence logical, whether to show confidence interval around point prediction (Default: FALSE)
#' @param level numeric, a number range from (0,1) as the confidence level (Default: 0.95)
#' @param ... additional arguments to be passed.
#' @return Point predictions for each new observation. If `confidence=TRUE`, also shows the confidence interval around the point point prediction.
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

# needed for blblm.sigma()?
# map_dbl_mean <- function(.x, .f, ...) {
#   map_dbl(.x, .f, ...) %>% mean()
# }

# bench::mark(sigma1 = map_mean2(est, ~ map_mean2(., "sigma")),
#             sigma = mean(map_dbl(est, ~ mean(map_dbl(., "sigma")))),
#             iterations = 1e3 )
# # A tibble: 2 x 13
# expression   min median `itr/sec` mem_alloc `gc/sec` n_itr  n_gc total_time result
# <bch:expr> <bch> <bch:>     <dbl> <bch:byt>    <dbl> <int> <dbl>   <bch:tm> <list>
#   1 sigma1     197ms  248ms      3.88     391KB     5.51  1000  1418      4.29m <dbl ~
#   2 sigma      202ms  275ms      3.45     391KB     4.90  1000  1418      4.83m <dbl ~
#   # ... with 3 more variables: memory <list>, time <list>, gc <list>

map_cbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(cbind)
}

map_rbind <- function(.x, .f, ...) {
  map(.x, .f, ...) %>% reduce(rbind)
}
