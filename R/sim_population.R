sim_predictors <- function(param, data_structure, pedigree){
  
  traits <- do.call(cbind, lapply( names(param), function(i){  

# i<-"individual"
    p <- param[[i]]
    
    ## sort out which are interactions   
    interactions <- grepl(":",p$names)

    # k <- length(p$mean)
    k <- sum(!interactions)
    n <- p$n_level

    ## simulate 'traits' at each level from multivariate normal 
    if(p$fixed){
      
      x<-model.matrix(formula(paste("~ factor(",p$group,")-1")),as.data.frame(data_structure))
      # x<- p$mean[data_structure[,p$group]]

      ## work out what to do with fixed effects and interactions
    }else if(p$covariate){
      x<- matrix(rep(data_structure[,p$group],k),nrow(data_structure),k)
    }else{
      ## if name is listed in pedigree argument, link to pedigree
      if(i %in% names(pedigree)){
          x <- MCMCglmm::rbv(pedigree[[i]],p$cov[!interactions,!interactions])
      }else{
        x <- matrix(rnorm( n*k,  0, 1), n, k) %*% chol(p$cov[!interactions,!interactions]) + matrix(p$mean[!interactions], n, k, byrow=TRUE)  
      }
      
      ## expand traits to be the same length as the number of observations using data structure  
      if(!p$group %in% c("observation","residual")) x <- x[data_structure[,p$group],,drop=FALSE]
    }
    ## use names form parameter list 
    colnames(x) <- p$names[!interactions]

    ## add in interactions
    x_int <- do.call(cbind,lapply(strsplit(p$names[interactions],":"), function(j){
        eval(parse(text=paste(j, collapse="*")), envir = as.data.frame(x) )
    }))
    if(sum(interactions)>0) colnames(x_int) <- p$names[interactions]

    cbind(x,x_int)
  
  }))
  
  return(traits)
}


transform_dist <- function(z, family, link){

  inv <- function(x) 1/x

  j <- ncol(z)

  if(length(link)==1 & j>1) link <- rep(link,j)
  if(length(family)==1 & j>1) family <- rep(family,j)

  ## convert the link argument into an actual function
  link_function <- 
  ifelse(link=="log", "exp", 
  ifelse(link=="inverse", "inv", 
  ifelse (link=="logit", "plogis", 
  ifelse (link=="probit", "pnorm", 
   link))))
  

  z_family <-  sapply(1:j,function(i){
    ## apply link function to z
    z_link <- get(link_function[i])(z[,i])
    ## sample from poisson or binomial 
    if(family[i]=="gaussian") z_link else 
    if(family[i]=="poisson") rpois(length(z_link),z_link) else 
    if(family[i]=="binomial") rbinom(length(z_link),1,z_link)
  })
  
  if(is.null(colnames(z))){
    colnames(z_family) <- if(j==1)"z" else paste0("z",1:j)
  }
  else{
    colnames(z_family) <- colnames(z)
  }

  return(z_family)
}


sim_population <- function(parameters, data_structure, model, family="gaussian", link="identity", pedigree){

  if(missing(data_structure))data_structure <- NULL

  param <- fill_parameters(parameters,data_structure)

  j <- n_phenotypes(param)

  if(j > 1 & !missing("model")) stop("Currently cannot specify multiple responses and a model formula")


  if(!all(link %in% c("identity", "log", "inverse", "sqrt", "logit", "probit"))) stop("Link must be 'identity', 'log', 'inverse', 'sqrt', 'logit', 'probit'")
  if(!all(family %in% c("gaussian", "poisson", "binomial"))) stop("Family must be 'gaussian', 'poisson', 'binomial'")
  
  if(!(length(link)==j || length(link)==1)){
    stop("Link must either be length 1 or same length as the number of parameters")
  }
  if(!(length(family)==j || length(family)==1)){
    stop("Link must either be length 1 or same length as the number of parameters")
  }


  ## index data_structure
  if(is.null(data_structure)){
    str_index <- NULL
  }else{
    str_index <- apply(data_structure,2,function(x) as.numeric(factor(x)))
  }
  
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

  predictors <- sim_predictors(param, str_index, pedigree)

  ## put all betas together
  betas <- do.call(rbind,lapply(param,function(x) x$beta))

  ## evaluate model
  ## - if model is missing, add all simulated predictors together
  if(missing("model")) {
    z <- predictors %*% betas
  } else {
    ## for evaluation with model formula 

    z_predictors <- cbind(predictors %*% diag(as.vector(betas)),predictors,str_index)
    colnames(z_predictors) <- c(colnames(predictors), paste0(colnames(predictors),"_raw"), paste0(colnames(data_structure),"_ID"))

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
      if(any(names(extra_param) %in% colnames(z_predictors))) stop("You cannot name extra parameters the same as any variables")
    }

    ## allow I() and subsets to be properly linked to z_predictors
    model <- gsub("I\\((\\w+)\\)","\\1_raw",model)
    model <- gsub("\\[(\\w+)\\]","\\[\\1_ID\\]",model)

    # evaluate the formula in the context of z_predictors and the extra params
  	z <- eval(parse(text=model), envir = c(as.data.frame(z_predictors),as.list(extra_param)))
    if(is.vector(z))
      z <- matrix(z)
  }


  z_family <- transform_dist(z, family, link)

  # in output predictors, if name matches something in data_stricture, then append "_effects"
  matching_names <- colnames(predictors) %in% colnames(data_structure)
  colnames(predictors)[matching_names] <- paste0(colnames(predictors)[matching_names],"_effects")
  

  out <- as.data.frame(cbind(z_family,predictors,data_structure))
  return(out)
}

## problem that by default the predictors and the level IDs will have the same names
## - maybe append "_effects"

