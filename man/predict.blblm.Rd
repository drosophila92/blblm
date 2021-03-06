% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blblm.R
\name{predict.blblm}
\alias{predict.blblm}
\title{Predict method for BLB Model Fits}
\usage{
\method{predict}{blblm}(object, new_data, confidence = FALSE, level = 0.95, ...)
}
\arguments{
\item{object}{Object of class inheriting from "blblm"}

\item{new_data}{A data frame in which to look for variables with which to predict.}

\item{confidence}{logical, whether to show confidence interval around point prediction (Default: FALSE)}

\item{level}{numeric, a number range from (0,1) as the confidence level (Default: 0.95)}

\item{...}{additional arguments to be passed.}
}
\value{
Point predictions for each new observation. If \code{confidence=TRUE}, also shows the confidence interval around the point point prediction.
}
\description{
Predicted values based on BLB model object.
}
