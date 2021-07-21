
n_phenotypes <- function(parameters){
  j <- sapply(parameters, function(x) ncol(x$beta))
  if(length(unique(j))!= 1) stop("The number of phenotypes (columns in beta) are not consistent across hierarchical levels in the parameter list", call.=FALSE)
  return(unique(j))
}




## I've used loops rather than apply functions in here because then the original parameter list can then be added to rather than new lists made - this will be slightly slower but very negligible given their size
fill_parameters <- function(parameters,data_structure){

  # Check whether list is given
  if(!is.list(parameters)) stop("parameters are not provided as a list", call.=FALSE)

  	# 


  

  # Check whether group specified -If not, then give name in list
  # Check whether length(group)==1 - If not, give warning and use only first group

  #i <- names(parameters)[1]
  for (i in names(parameters)){
    group <-parameters[[i]]$group
    if(is.null(group)) group <- i
    if(length(group)>1){
      warning("More than one group provided for ", i, ". First group being used.")
      group <- group[1]
    } 
    parameters[[i]]$group <- group
  }

  group_names <- sapply(parameters,function(x) x$group)

  ## check data_structure
  if(is.null(data_structure)){
    if(any(!group_names %in% c("observation","residual"))) stop("data_structure must be specified if there are more groups than 'observation' and 'residual' in parameter list", call.=FALSE)
  }else{
    if(!(is.matrix(data_structure)|is.data.frame(data_structure))) stop("data_structure is not a matrix or data.frame", call.=FALSE)   
  } 

  # User has to specify a "residual" level
  if(! "residual" %in% group_names) stop("One of the parameters groups must be 'residual'", call.=FALSE)
 
  # Check whether all groups match ones in data structure - If not, give error
  if(any(!group_names %in% c(colnames(data_structure),"observation","residual"))) stop("Group names in parameter list do not match group names in data_structure", call.=FALSE)

  # Check no group is called observation or residual - If not, give error
  if(any(colnames(data_structure) %in% c("observation","residual"))) stop("'observation' and 'residual' are reserved names for grouping factors. Please rename grouping factors in data_structure", call.=FALSE)

  #i <- names(parameters)[1]
  for (i in names(parameters)){
    # p <- parameters[[i]]
    
    # If cov is not a matrix, make it one. Need to do this before working out k, as code below requires a matrix
    # if its a matrix check its square and symmetric
    # if its a vector, make its the diagonal of a square matrix
    # if neither give error
    if(!is.null(parameters[[i]][["cov"]])){
      if(is.matrix(parameters[[i]][["cov"]])){
        if(nrow(parameters[[i]][["cov"]])!=ncol(parameters[[i]][["cov"]])) stop("need square cov matrix for ",i, call.=FALSE)
        if(!isSymmetric(parameters[[i]][["cov"]])) stop("cov matrix should be symmetric for ",i, call.=FALSE)
          #any(x[lower.tri(x)] != x[upper.tri(x)])
        if(any(eigen(parameters[[i]][["cov"]])$values<0))stop("cov matrix should be positive definite for ",i, call.=FALSE)
      }else if(is.vector(parameters[[i]][["cov"]])){
        parameters[[i]][["cov"]] <- if(length(parameters[[i]][["cov"]])==1) as.matrix(parameters[[i]][["cov"]]) else diag(parameters[[i]][["cov"]])
      }else{
        stop("cov must be a symmetric square matrix or a vector", call.=FALSE)
      }
    }
  
    # If beta is not a matrix, make it one. good for working out k and for simulations, as code below requires a matrix
    if(!is.null(parameters[[i]]$beta)){
      if(is.vector(parameters[[i]]$beta)){
        parameters[[i]]$beta <- matrix(parameters[[i]]$beta)
      }else if(!is.matrix(parameters[[i]]$beta)){stop("'beta' in ", i, " should be a vector or matrix", call.=FALSE)
      }
      if(is.null(parameters[[i]]$n_response)){ 
        parameters[[i]]$n_response <- ncol(parameters[[i]]$beta)
      }else if(parameters[[i]]$n_response != ncol(parameters[[i]]$beta)){ 
        stop("number of columns in beta is not the same as n_response for ",i, call.=FALSE)
      }
    }else{ 
      if(is.null(parameters[[i]]$n_response)){ 
        parameters[[i]]$n_response <- 1
      }else if(parameters[[i]]$n_response>1){ parameters[[i]]$beta <- diag(parameters[[i]]$n_response)
      }
    } 

    # Work out number of variables at that level (k)
    # Check that size (k) of names, mean, cov, sd and var match - if not give error
    lengths <- c(length(parameters[[i]]$names),
    	length(parameters[[i]]$mean),
    	ncol(parameters[[i]][["cov"]]),
    	nrow(parameters[[i]]$beta) ## possibly change this if allowing matrix of sds for multivariate
    )
    k <- unique(lengths[lengths>0])
    if(length(k) != 1) stop("The number of parameters given for ", i, " are not consistent", call.=FALSE)
    

    # Check whether names specified
    # If not, generate names (length k)
    if(is.null(parameters[[i]]$names)){
      # if(k==1) parameters[[i]]$names <- i
      # if(k>1) 
      parameters[[i]]$names <- paste0(i,"_effect",if(k>1){1:k})
    }else if(!is.vector(parameters[[i]]$names)){
      stop("'names' should be a vector for ", i, call.=FALSE)
    }
    # check everything is not just interaction terms
    if(!any(!grepl(":",parameters[[i]]$names))){
     stop("'names' only include interaction terms for ", i, call.=FALSE) 
    }
    ## check that all main effects are also specified along with interactions
    interactions <- grepl(":",parameters[[i]]$names)
    if(!all(c(strsplit(parameters[[i]]$names[interactions],":"), recursive=TRUE) %in% parameters[[i]]$names[!interactions] )){
      stop("'names' doesn't include all variables included in interactions for ", i, call.=FALSE) 
    }

    ## Check whether number of levels is specified
    # - if no take from data structure 
    # - if yes check it matches data structure  
    
    if(is.null(parameters[[i]]$n_level)){
      if(is.null(data_structure)){ 
        stop("If data_structure is not specified n_level must be specified in parameter list", call.=FALSE)
      }else if(parameters[[i]]$group %in% c("observation","residual")) {
          parameters[[i]]$n_level <- nrow(data_structure)
      }else{
          parameters[[i]]$n_level <- length(unique(data_structure[,parameters[[i]]$group]))
      }
    }else if(!is.null(data_structure) && parameters[[i]]$n_level != length(unique(data_structure[,parameters[[i]]$group]))){
      stop("If specified n_level must match the number of levels for a given grouping factor", call.=FALSE)
    }


    ## fixed
    if(is.null(parameters[[i]]$fixed)) parameters[[i]]$fixed <- FALSE
    
    if(parameters[[i]]$fixed & parameters[[i]]$n_level != k) stop("If fixed=TRUE, number of parameters should match the number of levels in grouping factor", call.=FALSE)
    
    if(parameters[[i]]$fixed & is.null(parameters[[i]]$beta)) stop("If fixed =TRUE, beta also needs to be specified", call.=FALSE)

    # Check whether covariate is specified
    if(is.null(parameters[[i]]$covariate)) parameters[[i]]$covariate <- FALSE
  
    if(parameters[[i]]$covariate & (!is.null(parameters[[i]]$mean) || !is.null(parameters[[i]][["cov"]]))) warning("Covariate=TRUE for ",i,", so mean and cov are ignored", call.=FALSE)
    
    if(parameters[[i]]$covariate & is.null(parameters[[i]]$beta)) stop("If covariate =TRUE, beta also needs to be specified", call.=FALSE)

    if(parameters[[i]]$covariate & is.null(parameters[[i]]$fixed)) stop("covariate =TRUE and fixed=TRUE for ", i, call.=FALSE)


    # Check whether mean specified
    # If not, rep(0,k)
    if(is.null(parameters[[i]]$mean)){
      parameters[[i]]$mean <- rep(0,k)
    }else if(!is.vector(parameters[[i]]$mean)){
      stop("'mean' should be a vector", call.=FALSE)
    }
    
    # Check whether cov specified
    # If not, diag(k)
    if(is.null(parameters[[i]][["cov"]])) parameters[[i]][["cov"]] <- diag(k)
    
    # Check whether beta specified
    if(is.null(parameters[[i]]$beta)){
      parameters[[i]]$beta <- matrix(1,k,parameters[[i]]$n_response)
    }




  }

  ##check whether all betas have same dimension
  j <- n_phenotypes(parameters)

  ##Check extra parameters
  param_names <- c("names", "group", "mean", "cov", "beta", "n_level", "fixed", "n_response", "covariate")

  e_p <- unlist(sapply(parameters, function(x){
    names(x)[!names(x) %in% param_names]
    }))

  if(length(e_p)>0){
    ## check is extra parameters are vectors
    e_p_vector <- !unlist(sapply(parameters, function(x){
      sapply(x[!names(x) %in% param_names],is.vector)
      }))
    if(any(e_p_vector)) stop("Additional parameters given to parameters lists must be vectors, this is not the case for ",names(e_p_vector)[e_p_vector], call.=FALSE)
  
    ## check length of all extra parameters is 1
    e_p_length <- unlist(sapply(parameters, function(x){
      sapply(x[!names(x) %in% param_names],length)
      }))
    if(any(e_p_length>1)) stop("Additional parameters given to parameters lists must be length 1, this is not the case for ",names(e_p_length)[e_p_length>1], call.=FALSE)
  }

  ## Check whether all names in data_structure and parameters contain only words, digits, : and _
  pred_names <- do.call(c, lapply(parameters,function(x) x$names))
  if(any(duplicated(pred_names))) stop("Predictor names must be unique", call.=FALSE)
  
  all_names <- c(e_p, pred_names, colnames(data_structure))

  if(!all(grepl("^[A-z0-9_:]*$",all_names))) stop("Names in data structure and in parameters must be alphanumeric, '_' or ':'", call.=FALSE)

  ### check no names are repeated!!
  if(any(duplicated(e_p)) || any(e_p %in% c(pred_names, colnames(data_structure)))) stop("Additional parameters names must be unique", call.=FALSE)

  ### check all names have at least 1 character!!
  if(any(nchar(c(pred_names, e_p, colnames(data_structure)))==0 )) stop("Specified names must have nchar>0", call.=FALSE)

	return(parameters)

}

