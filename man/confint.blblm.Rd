% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{confint.blblm}
\alias{confint.blblm}
\title{BLB confidence intervals of regression coefficients (\eqn{\beta} s)}
\usage{
\method{confint}{blblm}(object, parm = NULL, level = 0.95, ...)
}
\arguments{
\item{object}{a blblm object}

\item{parm}{a charactor vector, names of parameters (i.e. regression coefficients) whose CI is to be calculated.}

\item{level}{numeric, a number range from (0,1) as the confidence level (Default: 0.95)}

\item{...}{additional arguments to be passed.}
}
\value{
a matrix containing CI estimates on user-specified parameters (\code{parm}) with user-specified confidence level (\code{level})
}
\description{
Calculate the BLB confidence intervals of regression coefficients (\eqn{\beta} s).
}
