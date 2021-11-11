index_factors <- function(data_structure, pedigree, parameters,...){
  
  if(is.null(data_structure)){
    NULL
  }else{
    ## for each column of the data structure, are any of the parameters list names associated with it, linked with a pedigree. If so, then get the row number in the pedigree, as that will match the indexing in the pedigree
    
    new_ds <- do.call(cbind,lapply(colnames(data_structure), function(i){
      # i = colnames(data_structure)[1]

      if(any(names(parameters)[sapply(parameters,function(x) x$group) %in% i] %in% names(pedigree))){ 
        
        list_names <- names(parameters)[sapply(parameters,function(x) x$group) %in% i]
        ped_link <- list_names[list_names %in% names(pedigree)]
        if(length(ped_link)>1){stop("Multiple pedigrees linked to one grouping factor")}
        match(data_structure[,i],pedigree[[ped_link]][,1])
      }else{ 
        as.numeric(factor(data_structure[,i]))
      }
    }))
    
    # apply(data_structure,2,function(x) as.numeric(factor(x)))
    colnames(new_ds) <- colnames(data_structure)
    return(new_ds)

  }
}

index_ped <- function(pedigree, unknown=NA){
  
  new_ped <- data.frame(
    1:nrow(pedigree), 
    ifelse(is.na(pedigree[,2]),unknown,match(pedigree[,2], pedigree[,1])), 
    ifelse(is.na(pedigree[,3]),unknown,match(pedigree[,3], pedigree[,1]))
  )
  colnames(new_ped) <- colnames(pedigree)[1:3]

  return(new_ped) 
}


cov_str_list <- function(parameters, data_structure, pedigree, phylogeny, cov_str,...){

  ped_check <- lapply(c("pedigree","phylogeny","cov_str"), function(j){
    cs <- get(j)
    if(!is.null(cs) && (!is.list(cs) | is.data.frame(cs))) stop(j, " needs to be a list", call.=FALSE) 
  })

  ped_names <- c(names(pedigree),names(phylogeny),names(cov_str))
  if(any(duplicated(ped_names))) stop("Cannot have multiple covariance structures linking to the same item in the parameter list", call.=FALSE)
  if(any(!ped_names %in% names(parameters))) stop("Some names in pedigree/phylogeny/cov_str are not in parameters", call.=FALSE)

  ped_chol <- lapply(pedigree, function(x) Matrix::chol(nadiv::makeA(x)))
  phylo_chol <- lapply(phylogeny, function(x) as(chol(ape::vcv(x), corr = TRUE), "dgCMatrix"))
  cor_chol <- lapply(cov_str, function(x) as(chol(x), "dgCMatrix"))

  chol_str<-c(ped_chol,phylo_chol,cor_chol)

  names_check <- lapply(ped_names,function(i){
  # data_structure[,i]
    if(!all(unique(rownames(chol_str[[i]])) %in% unique(data_structure[,parameters[[i]]$group]))) stop(paste("all IDs in the pedigree/phylogeny/cov_str linked with", i, "are not in the data_structure"), call.=FALSE)
    if(!all(unique(data_structure[,parameters[[i]]$group]) %in% unique(rownames(chol_str[[i]])))) stop(paste("all IDs in data_structure are not in the pedigree/phylogeny/cov_str linked with", i), call.=FALSE)      
  })
  
  add_list<-names(parameters)[!names(parameters) %in% names(chol_str)]
  for(i in add_list){
    chol_str[[i]] <- Matrix::Diagonal(parameters[[i]][["n_level"]])
  }
  return( chol_str)
}




sim_predictors <- function(parameters, data_structure, pedigree, cov_str, ...){
  
  ## index data_structure
  str_index <- index_factors(data_structure=data_structure,pedigree=pedigree,parameters=parameters)
  ped_index <- lapply(pedigree,index_ped)

  traits <- do.call(cbind, lapply( names(parameters), function(i){  

# i<-"animal"
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

      x <- as(Matrix::crossprod(cov_str[[i]],matrix(stats::rnorm( n*k,  0, 1), n, k)) %*% chol(p$vcov[!interactions,!interactions])   + matrix(p$mean[!interactions], n, k, byrow=TRUE),"matrix")

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
    # y <- eval(parse(text=model), envir = c(as.data.frame(y_predictors),as.list(extra_param)))
    
    model2 <- paste(model,";\n return(data.frame(mget(ls()[!ls() %in% c(colnames(y_predictors),names(extra_param))])))")
    y <- eval(parse(text=model2), envir = c(as.data.frame(y_predictors),as.list(extra_param)))

    if(is.vector(y)) y <- matrix(y)
  }
  
  return(y)
}

generate_y_list <- function(parameters, data_structure, predictors, pedigree, model,...){

    ## index data_structure
  str_index <- index_factors(data_structure=data_structure,pedigree=pedigree,parameters=parameters)
  
  ## put all betas together
  betas <- do.call(rbind,lapply(parameters,function(x) x$beta))
  #betas <- rbind(do.call(rbind,lapply(parameters,function(x) x$beta)), extra_betas)

  y_pred_names <- c(colnames(predictors[[1]]), paste0(colnames(predictors[[1]]),"_raw"), if(!is.null(str_index)){paste0(colnames(str_index),"_ID")})

  ## extract and name extra parameters
  if(!is.null(model)){

    param_names <- c("names", "group", "mean", "vcov", "vcorr", "beta", "n_response", "fixed", "covariate", "n_level")
    extra_param <- unlist(sapply(parameters, function(x){ x[!names(x) %in% param_names] }))
        
    if(!is.null(extra_param)){
      names(extra_param) <- unlist(sapply(parameters, function(x) names(x)[!names(x) %in% param_names]
        ))
      ## check extra param names dont clash with y_trait names
      if(any(names(extra_param) %in% colnames(y_pred_names))) stop("You cannot name extra parameters the same as any variables")
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

