library(boot)

sim_population <- function(parameters, data_structure){
  
  param<-fill_parameters(parameters,data_structure)

  #i <- names(parameters)[1]
  traits <- do.call(cbind, lapply( names(param), function(i){
    p <- param[[i]]
    k <- length(p$mean)
    n <- p$n_level

	## simulate from multivariate normal 
    x <- matrix(rnorm( n*k,  0, 1), n, k) %*% chol(p$cov) + matrix(p$mean, n, k, byrow=TRUE) 
    if(i!="residual") x <- x[data_structure[,p$group],,drop=FALSE]
    colnames(x) <- p$names
    return(x)
  }))

  sds <- do.call(c,lapply(param,function(x) x$sd))

  z_traits <- traits %*% diag(sds)
  colnames(z_traits) <- colnames(traits)

  z <- rowSums(z_traits)
  
  out <- as.data.frame(cbind(z=z,traits,data_structure))
  return(out)
}

## problem that by default the traits and the level IDs will have the same names
## - maybe append "_effects"

