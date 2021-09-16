index_factors <- function(data_structure){
  
  if(is.null(data_structure)){
    NULL
  }else{
    apply(data_structure,2,function(x) as.numeric(factor(x)))
  }
    
}


sim_predictors <- function(parameters, data_structure, pedigree, ...){
  
  ## index data_structure
  str_index <- index_factors(data_structure)

  traits <- do.call(cbind, lapply( names(parameters), function(i){  

# i<-"individual"
    p <- parameters[[i]]
    
    ## sort out which are interactions   
    interactions <- grepl(":",p$names)

    # k <- length(p$mean)
    k <- sum(!interactions)
    n <- p$n_level

    ## simulate 'traits' at each level from multivariate normal 
    if(p$fixed){
      
      x<-stats::model.matrix(stats::formula(paste("~ factor(",p$group,")-1")),as.data.frame(str_index))
      # x<- p$mean[str_index[,p$group]]

      ## work out what to do with fixed effects and interactions
    }else if(p$covariate){
      x<- matrix(rep(str_index[,p$group],k),nrow(str_index),k)
    }else{
      ## if name is listed in pedigree argument, link to pedigree
      if(i %in% names(pedigree)){
          x <- MCMCglmm::rbv(pedigree[[i]],p$cov[!interactions,!interactions])
      }else{
        x <- matrix(stats::rnorm( n*k,  0, 1), n, k) %*% chol(p$cov[!interactions,!interactions]) + matrix(p$mean[!interactions], n, k, byrow=TRUE)  
      }
      
      ## expand traits to be the same length as the number of observations using data structure  
      if(!p$group %in% c("observation","residual")) x <- x[str_index[,p$group],,drop=FALSE]
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




generate_y <- function(predictors, betas, str_index,  model, y_pred_names,extra_param,...){
  
  ## evaluate model
  ## - if model is missing, add all simulated predictors together
  if(is.null(model)) {
    y <- predictors %*% betas
  } else {
    ## for evaluation with model formula 

    y_predictors <- cbind(predictors %*% diag(as.vector(betas)),predictors,str_index)
    colnames(y_predictors) <- y_pred_names

    ## allow I() and subsets to be properly linked to y_predictors
    model <- gsub("I\\((\\w+)\\)","\\1_raw",model)
    model <- gsub("\\[(\\w+)\\]","\\[\\1_ID\\]",model)

    # evaluate the formula in the context of y_predictors and the extra params
    y <- eval(parse(text=model), envir = c(as.data.frame(y_predictors),as.list(extra_param)))
    if(is.vector(y)) y <- matrix(y)
  }
  
  return(y)
}

generate_y_list <- function(parameters, data_structure, predictors,model,...){

    ## index data_structure
  str_index <- index_factors(data_structure)
  
  ## put all betas together
  betas <- do.call(rbind,lapply(parameters,function(x) x$beta))

  y_pred_names <- c(colnames(predictors[[1]]), paste0(colnames(predictors[[1]]),"_raw"), if(!is.null(str_index)){paste0(colnames(str_index),"_ID")})

  ## extract and name extra parameters
  if(!is.null(model)){

    param_names <- c("names", "group", "mean", "cov", "beta", "n_response", "fixed", "covariate", "n_level")
    extra_param <- unlist(sapply(parameters, function(x){ x[!names(x) %in% param_names] }))
        
    if(!is.null(extra_param)){
      names(extra_param) <- unlist(sapply(parameters, function(x) names(x)[!names(x) %in% param_names]
        ))
      ## check extra param names dont clash with y_trait names
      if(any(names(extra_param) %in% colnames(y_predictors))) stop("You cannot name extra parameters the same as any variables")
    }
  }

  y <- lapply(predictors, function(x) generate_y(x, betas=betas, str_index=str_index,  model=model, y_pred_names=y_pred_names,extra_param=extra_param))

  return(y)

}


transform_dist <- function(y, family, link,...){

  inv <- function(x) 1/x

  j <- ncol(y)

  if(length(link)==1 & j>1) link <- rep(link,j)
  if(length(family)==1 & j>1) family <- rep(family,j)

  ## convert the link argument into an actual function
  link_function <- 
  ifelse(link=="log", "exp", 
  ifelse(link=="inverse", "inv", 
  ifelse (link=="logit", "plogis", 
  ifelse (link=="probit", "pnorm", 
   link))))
  

  y_family <-  sapply(1:j,function(i){
    ## apply link function to y
  y_link <- get(link_function[i])(y[,i])
    ## sample from poisson or binomial 
    if(family[i]=="gaussian") y_link else 
    if(family[i]=="poisson") stats::rpois(length(y_link),y_link) else 
    if(family[i]=="binomial") stats::rbinom(length(y_link),1,y_link)
  })
  
  if(is.null(colnames(y))){
    colnames(y_family) <- if(j==1)"y" else paste0("y",1:j)
  }
  else{
    colnames(y_family) <- colnames(y)
  }

  return(y_family)
}


