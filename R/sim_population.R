sim_population <- function(parameters, data_structure, model, family="gaussian", link="identity"){

  if(!link %in% c("identity", "log", "inverse", "sqrt", "logit", "probit")) stop("Link must be 'identity', 'log', 'inverse', 'sqrt', 'logit', 'probit'")
  if(!family %in% c("gaussian", "poisson", "binomial")) stop("Family must be 'gaussian', 'poisson', 'binomial'")
  
  param <- fill_parameters(parameters,data_structure)

  traits <- do.call(cbind, lapply( names(param), function(i){
    p <- param[[i]]
    k <- length(p$mean)
    n <- p$n_level

	  ## simulate 'traits' at each level from multivariate normal 
    x <- matrix(rnorm( n*k,  0, 1), n, k) %*% chol(p$cov) + matrix(p$mean, n, k, byrow=TRUE)

    ## expand traits to be the same length as the number of observations using data structure  
    if(i!="residual") x <- x[data_structure[,p$group],,drop=FALSE]
    
    ## use names form parameter list 
    colnames(x) <- p$names
    return(x)
  }))

  ## this isn't going to work if betas has multiple rows 
  betas <- do.call(c,lapply(param,function(x) x$beta))
  z_traits <- traits %*% diag(betas)
  colnames(z_traits) <- colnames(traits)

  ## evaluate model
  ## - if model is missing, add all simulated traits together
  ##- need to integrate multivariate
  if(missing("model")) {
    z <- rowSums(z_traits)
  } else {
  	z <- eval(parse(text=model), envir = as.data.frame(z_traits))
  }

  inv <- function(x) 1/x

  ## convert the link argument into an actual function
  link_function <- if(link=="log") "exp" else 
    if(link=="inverse") "inv" else 
    if(link=="logit") "plogis" else 
    if(link=="probit") "pnorm" else link
  
  ## apply link function to z
  z_link <- get(link_function)(z)
  
  ## sample from poisson or binomial 
  z_family <- if(family=="gaussian") z_link else 
    if(family=="poisson") rpois(length(z_link),z_link) else 
    if(family=="binomial") rbinom(length(z_link),1,z_link)

  out <- as.data.frame(cbind(z=z_family,traits,data_structure))
  return(out)
}

## problem that by default the traits and the level IDs will have the same names
## - maybe append "_effects"

