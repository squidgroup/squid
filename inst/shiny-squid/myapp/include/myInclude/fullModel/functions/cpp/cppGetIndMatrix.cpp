#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

#include <iostream>
using namespace std;



// [[Rcpp::export]]
mat cppGetIndMatrix(int NI, int NP, vec Mu, mat VCov, int nbIS){
  
   int n     = NI*NP;
   int ncols = VCov.n_cols;   
   
   cout << n << " --- " << ncols << endl;
   
   mat Y     = randn(n, ncols);
   
   cout << Y << endl;
   
   mat ind   =  repmat(Mu, 1, n).t() + Y * chol(VCov);
  
  
  return ind;
}
