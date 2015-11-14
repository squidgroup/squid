# get_nb_Cov : get the number of Covariance in a VCovariance matrix 

# nb_diag    : the number of element in the matrix diagonal


get_nb_Cov <- function(nb_diag){
  
  nb_Cov <- ((nb_diag^2)-nb_diag)/2
  
  return(nb_Cov)
}