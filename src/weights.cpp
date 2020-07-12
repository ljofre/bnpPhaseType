#include <Rcpp.h>
#include <stdio.h>
using namespace Rcpp;
using namespace std;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// [[Rcpp::export]] 
NumericVector weights(NumericVector v){
  // fast implementation of weights in stick-braking representation
  NumericVector cp = cumprod(1 - v);
  cp.push_front(1.0);
  return v*cp;
  }

/*** R

library(MCMCpack)

w <- rbeta(n=100000, shape1 = 1, shape2 = 3)

library(microbenchmark)

# Fast version 
v = rbeta(1000, 1, 3)
stick_breaking_process = function(v) 
{ 
  num_weights = length(v)
  remaining_stick_lengths = c(1, cumprod(1 - v))[1:num_weights] 
  weights = remaining_stick_lengths * v  
  weights
} 

microbenchmark(w1 <- stick_breaking_process(v))
sum(w1)

microbenchmark(w2 <- weights(v=v))
sum(w2)

plot(w1, w2)

*/


