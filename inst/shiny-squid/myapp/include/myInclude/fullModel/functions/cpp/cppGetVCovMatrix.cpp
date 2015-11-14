#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix cppGetVCovMatrix(NumericMatrix VCov) {

  int Vdim = VCov.nrow();

  if(Vdim != 1){
    int k = 0;
    for(int i = 0 ; i < (Vdim-1); i++){
      for(int j = 0; j <= i; j++){
        VCov(i+1,j) = VCov(i+1,j)*sqrt(VCov(j,j)*VCov(i+1,i+1));
        VCov(j,i+1) = VCov(i+1,j);
        k++;
      }
    }
  }

  return VCov;
}
