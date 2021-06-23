sim_population <- function(parameters, data_structure, model, family="gaussian", link="identity", pedigree){

  if(!link %in% c("identity", "log", "inverse", "sqrt", "logit", "probit")) stop("Link must be 'identity', 'log', 'inverse', 'sqrt', 'logit', 'probit'")
  if(!family %in% c("gaussian", "poisson", "binomial")) stop("Family must be 'gaussian', 'poisson', 'binomial'")
  
  param <- fill_parameters(parameters,data_structure)

  j <- n_phenotypes(param)

  if(j > 1 & !missing("model")) stop("Currently cannot specify multiple phenotypes and a model formula")

  ## check pedigree is list, make one if not
  if(missing(pedigree)){
    pedigree <-list()
  }else{
    if(!is.list(pedigree) | is.data.frame(pedigree)) stop("pedigree needs to be a list")
  }
  ## check pedigree levels match data structure levels
  # lapply(names(pedigree))

  # unique(data_structure[,param[[i]]$group])
  # unique(pedigree[[i]][,1])



  traits <- do.call(cbind, lapply( names(param), function(i){
    p <- param[[i]]
    k <- length(p$mean)
    n <- p$n_level

	  ## simulate 'traits' at each level from multivariate normal 
    ## if name is listed in pedigree argument, link to pedigree
    if(i %in% names(pedigree)){
      x <- MCMCglmm::rbv(pedigree[[i]],p$cov)
    }else{
      x <- matrix(rnorm( n*k,  0, 1), n, k) %*% chol(p$cov) + matrix(p$mean, n, k, byrow=TRUE)  
    }
    
    ## expand traits to be the same length as the number of observations using data structure  
    if(p$group!="residual") x <- x[data_structure[,p$group],,drop=FALSE]
    
    ## use names form parameter list 
    colnames(x) <- p$names
    return(x)
  }))

  ## put all betas together
  betas <- do.call(rbind,lapply(param,function(x) x$beta))

  ## evaluate model
  ## - if model is missing, add all simulated traits together
  if(missing("model")) {
    z <- traits %*% betas
  } else {
    ## for evaluation with model formula 

    z_traits <- cbind(traits %*% diag(as.vector(betas)),traits,data_structure)
    colnames(z_traits) <- c(colnames(traits), paste0(colnames(traits),"_raw"), paste0(colnames(data_structure),"_ID"))

    ## extract extra parameters
    param_names <- c("names", "group", "mean", "cov", "beta", "n_level")
    extra_param <- unlist(sapply(parameters, function(x){
    x[!names(x) %in% param_names]
    }))
    if(!is.null(extra_param)){
      names(extra_param) <- unlist(sapply(parameters, function(x){
        names(x)[!names(x) %in% param_names]
        }))
      ## check extra param names dont clash with z_trait names
      if(any(names(extra_param) %in% colnames(z_traits))) stop("You cannot name extra parameters the same as any variables")
    }

    ## allow I() and subsets to be properly linked to z_traits
    model <- gsub("I\\((\\w+)\\)","\\1_raw",model)
    model <- gsub("\\[(\\w+)\\]","\\[\\1_ID\\]",model)

    # evaluate the formula in the context of _tratis and the extra params
  	z <- eval(parse(text=model), envir = c(as.data.frame(z_traits),as.list(extra_param)))
    if(is.vector(z))
      z <- matrix(z)
  }
  ## add extra list elements into z_traits

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
    if(family=="poisson") matrix(rpois(length(z_link),z_link), nrow(z), ncol(z)) else 
    if(family=="binomial") matrix(rbinom(length(z_link),1,z_link), nrow(z), ncol(z))

  # in output traits, if name matches something in data_stricture, then append "_effects"
  matching_names <- colnames(traits) %in% colnames(data_structure)
  colnames(traits)[matching_names] <- paste0(colnames(traits)[matching_names],"_effects")
  
  if(is.null(colnames(z_family))){
    if(is.null(colnames(z))){
      colnames(z_family) <- if(ncol(z_family)==1)"z" else paste0("z",1:ncol(z_family))
    }
    else{
      colnames(z_family) <- colnames(z)
    }
  }

  out <- as.data.frame(cbind(z_family,traits,data_structure))
  return(out)
}

## problem that by default the traits and the level IDs will have the same names
## - maybe append "_effects"

