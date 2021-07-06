
n_phenotypes <- function(parameters){
  j <- sapply(parameters, function(x) ncol(x$beta))
  if(length(unique(j))!= 1) stop("The number of phenotypes (columns in beta) are not consistent across hierarchical levels in the parameter list", call.=FALSE)
  return(unique(j))
}




## I've used loops rather than apply functions in here because then the original parameter list can then be added to rather than new lists made - this will be slightly slower but very negligible given their size
fill_parameters <- function(parameters,data_structure){

  # Check whether list is given
  if(!is.list(parameters)) stop("parameters are not provided as a list", call.=FALSE)

  	# if(exists("data_structure"))

  ## check data_structure
  if(!(is.matrix(data_structure)|is.data.frame(data_structure))) stop("data_structure is not a matrix or data.frame", call.=FALSE)

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

  # User has to specify a "residual" level
  if(! "residual" %in% sapply(parameters,function(x) x$group)) stop("One of the parameters groups must be 'residual'", call.=FALSE)
 
  # Check whether all groups match ones in data structure - If not, give error
  if(any(!sapply(parameters,function(x) x$group) %in% c(colnames(data_structure),"residual"))) stop("Group names in parameter list do not match group names in data_structure", call.=FALSE)

  #i <- names(parameters)[1]
  for (i in names(parameters)){
    # p <- parameters[[i]]
    # If cov is not a matrix, make it one. Need to do this before working out k, as code below requires a matrix
    # if its a matrix check its square and symmetric
    # if its a vector, make its the diagonal of a square matrix
    # if neither give error
    if(!is.null(parameters[[i]]$cov)){
      if(is.matrix(parameters[[i]]$cov)){
        if(nrow(parameters[[i]]$cov)!=ncol(parameters[[i]]$cov)) stop("need square cov matrix for ",i, call.=FALSE)
        if(!isSymmetric(parameters[[i]]$cov)) stop("cov matrix should be symmetric for ",i, call.=FALSE)
          #any(x[lower.tri(x)] != x[upper.tri(x)])
        if(any(eigen(parameters[[i]]$cov)$values<0))stop("cov matrix should be positive definite for ",i, call.=FALSE)
      }else if(is.vector(parameters[[i]]$cov)){
        parameters[[i]]$cov <- if(length(parameters[[i]]$cov)==1) as.matrix(parameters[[i]]$cov) else diag(parameters[[i]]$cov)
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
    	ncol(parameters[[i]]$cov),
    	nrow(parameters[[i]]$beta) ## possibly change this if allowing matrix of sds for multivariate
    )
    k <- unique(lengths[lengths>0])
    if(length(k) != 1) stop("The number of parameters given for ", i, " are not consistent", call.=FALSE)
    

    # Check whether names specified
    # If not, generate names (length k)
    if(is.null(parameters[[i]]$names)){
      if(k==1) parameters[[i]]$names <- i
      if(k>1) parameters[[i]]$names <- paste(i,1:k,sep="_")
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
      if(parameters[[i]]$group=="residual") {
          parameters[[i]]$n_level <- nrow(data_structure)
      } else {
      parameters[[i]]$n_level <- length(unique(data_structure[,parameters[[i]]$group]))
      }
    } else {
      
    }

    ## fixed
    if(is.null(parameters[[i]]$fixed)) parameters[[i]]$fixed <- FALSE
    
    if(parameters[[i]]$fixed & parameters[[i]]$n_level != k) stop("If fixed=TRUE, number of parameters should match the number of levels in grouping factor", call.=FALSE)
    
    if(parameters[[i]]$fixed & is.null(parameters[[i]]$beta)) stop("If fixed =TRUE, beta also needs to be specified", call.=FALSE)

    # Check whether mean specified
    # If not, rep(0,k)
    if(is.null(parameters[[i]]$mean)){
      parameters[[i]]$mean <- rep(0,k)
    }else if(!is.vector(parameters[[i]]$mean)){
      stop("'mean' should be a vector", call.=FALSE)
    }
    
    # Check whether cov specified
    # If not, diag(k)
    if(is.null(parameters[[i]]$cov)) parameters[[i]]$cov <- diag(k)
    
    # Check whether beta and n_response specified
    if(is.null(parameters[[i]]$beta)){
      parameters[[i]]$beta <- matrix(1,k,parameters[[i]]$n_response)
    }
  
  }

  ##check whether all betas have same dimension
  j <- n_phenotypes(parameters)

  ##Check extra parameters
  param_names <- c("names", "group", "mean", "cov", "beta", "n_level", "fixed", "n_response")

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
  all_names <- c(e_p, do.call(c, lapply(parameters,function(x) x$names)), colnames(data_structure))

  if(!all(grepl("^[A-z0-9_:]*$",all_names))) stop("Names in data structure and in parameters must be alphanumeric, '_' or ':'", call.=FALSE)

  ### check no names are repeated!!
  if(any(duplicated(e_p)) || any(e_p %in% c(do.call(c, lapply(parameters,function(x) x$names)), colnames(data_structure)))) stop("Additional parameters names must be unique")
	return(parameters)





}



### possibly make it so that if data_structure is not specified then make one with completely crossed random effects?
### then you would need to specify the number of levels