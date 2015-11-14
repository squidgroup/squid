#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;

#include <iostream>
using namespace std;


// [[Rcpp::export]]
rowvec cppDecayRate(rowvec env, float alpha, int Nb){

    mat myMatrix(Nb, Nb); myMatrix.zeros(); 
    
    for(int row=0; row < Nb; row++){
        for(int col=0; col < Nb; col++){
          myMatrix(row, col) = exp(-1*alpha*abs(col-row));
        }
    }
    
    rowvec newEnv(Nb);
    
		for(int i=0; i<Nb; i++)
		{
			newEnv(i) = 0;
			for(int j=0; j<Nb; j++)
				newEnv(i) += myMatrix(j,i)*env(j);
		}	
    
    cout << endl;
    

  return newEnv;
}
