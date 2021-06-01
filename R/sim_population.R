
sim_population <- function(parameters, data_structure, formula, family="gaussian", link="identity"){

  if(!link %in% c("identity", "log", "inverse", "sqrt", "logit", "probit")) stop("Link must be 'identity', 'log', 'inverse', 'sqrt', 'logit', 'probit'")
  if(!family %in% c("gaussian", "poisson", "binomial")) stop("Family must be 'gaussian', 'poisson', 'binomial'")
  
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

  betas <- do.call(c,lapply(param,function(x) x$beta))

  z_traits <- traits %*% diag(betas)
  colnames(z_traits) <- colnames(traits)

  if(missing("formula")) {
    z <- rowSums(z_traits)
  } else {
  	z <- eval(parse(text=formula), envir = as.data.frame(z_traits))
  }

  inv <- function(x) 1/x
# link="identity"
  # family="gaussian"
  link_function <- if(link=="log") "exp" else 
    if(link=="inverse") "inv" else 
    if(link=="logit") "plogis" else 
    if(link=="probit") "pnorm" else link
  z_link <- get(link_function)(z)
  z_family <- if(family=="gaussian") z_link else 
    if(family=="poisson") rpois(length(z_link),z_link) else 
    if(family=="binomial") rbinom(length(z_link),1,z_link)

  out <- as.data.frame(cbind(z=z_family,traits,data_structure))
  return(out)
}

## problem that by default the traits and the level IDs will have the same names
## - maybe append "_effects"

