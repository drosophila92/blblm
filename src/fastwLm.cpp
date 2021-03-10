// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

//' @title An Rcpp function to quickly perform weighted linear regression
//' @name fastwLm
//'
//' @param x design matrix
//' @param y numeric vector of response variables
//' @param w numeric vector of weights
//' @return list
//' @export
//' @useDynLib blblm
//' @importFrom Rcpp sourceCpp

// [[Rcpp::export]]
Rcpp::List fastwLm(const arma::mat& X, const arma::colvec& y, const arma::colvec& w) {
  int n = X.n_rows, k = X.n_cols;

  arma::mat weight = arma::diagmat( arma::sqrt(w) ); // generate weight matrix
  arma::mat weight_ = arma::diagmat( arma::sqrt(1/w) ); // generate weight matrix
  arma::mat new_X = weight * X; // for loop
  arma::mat new_y = weight * y; // vector product

  arma::colvec coef = arma::solve(new_X, new_y);    // fit model new_y ~ new_X
  arma::colvec res  = weight_ * (new_y - new_X * coef);
  //arma::colvec res  = weight_ * (new_y - new_X * coef);         // residuals


  // type conversion
  // https://stackoverflow.com/questions/14253069/convert-rcpparmadillo-vector-to-rcpp-vector
  return List::create(Named("coefficients") = NumericVector(coef.begin(),coef.end()),
                      Named("residuals") = NumericVector(res.begin(),res.end()),
                      Named("rank")  = k,
                      Named("weights") = NumericVector(w.begin(),w.end()));
}

