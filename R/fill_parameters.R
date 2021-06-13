
## I've used loops rather than apply functions in here because then the original parameter list can then be added to rather than new lists made - this will be slightly slower but very negligible given their size
fill_parameters <- function(parameters,data_structure){

  # Check whether list is given
  if(!is.list(parameters)) stop("parameters are not provided as a list")

  	# if(exists("data_structure"))

  ## check data_structure
  if(!(is.matrix(data_structure)|is.data.frame(data_structure))) stop("data_structure is not a matrix or data.frame")

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
  if(! "residual" %in% sapply(parameters,function(x) x$group)) stop("One of the parameters groups must be 'residual'")
 
  # Check whether all groups match ones in data structure - If not, give error
  if(any(!sapply(parameters,function(x) x$group) %in% c(colnames(data_structure),"residual"))) stop("Group names in parameter list do not match group names in data_structure")

  #i <- names(parameters)[1]
  for (i in names(parameters)){
    
    # If cov is not a matrix, make it one. Need to do this before working out k, as code below requires a matrix
    # if its a matrix check its square and symmetric
    # if its a vector, make its the diagonal of a square matrix
    # if neither give error
    if(!is.null(parameters[[i]]$cov)){
      if(is.matrix(parameters[[i]]$cov)){
        if(nrow(parameters[[i]]$cov)!=ncol(parameters[[i]]$cov)) stop("need square cov matrix for ",i)
        if(!isSymmetric(parameters[[i]]$cov)) stop("cov matrix should be symmetric for ",i)
          #any(x[lower.tri(x)] != x[upper.tri(x)])
      }else if(is.vector(parameters[[i]]$cov)){
        parameters[[i]]$cov <- if(length(parameters[[i]]$cov)==1) as.matrix(parameters[[i]]$cov) else diag(parameters[[i]]$cov)
      }else{
        stop("cov must be symmetric square matrix or vector")
      }
    }
  
    # Work out number of variables at that level (k)
    # Check that size (k) of names, mean, cov, sd and var match - if not give error
    beta_k <- if(is.vector(parameters[[i]]$beta)){length(parameters[[i]]$beta)}else if(is.matrix(parameters[[i]]$beta)){ncol(parameters[[i]]$beta)}
    lengths <- c(length(parameters[[i]]$names),
    	length(parameters[[i]]$mean),
    	ncol(parameters[[i]]$cov),
    	beta_k ## possibly change this if allowing matrix of sds for multivariate
    )
    k <- unique(lengths[lengths>0])
    if(length(k) != 1) stop("The number of parameters given for ", i, " are not consistent")
    
    
    # Check whether names specified
    # If not, generate names (length k)
    if(is.null(parameters[[i]]$names)){
      if(k==1) parameters[[i]]$names <- i
      if(k>1) parameters[[i]]$names <- paste(i,1:k,sep="_")
    }else if(!is.vector(parameters[[i]]$names)){
      stop("'names' should be a vector")
    }

    # Check whether mean specified
    # If not, rep(0,k)
    if(is.null(parameters[[i]]$mean)){
      parameters[[i]]$mean <- rep(0,k)
    }else if(!is.vector(parameters[[i]]$mean)){
      stop("'mean' should be a vector")
    }
    
    # Check whether cov specified
    # If not, diag(k)
    if(is.null(parameters[[i]]$cov)) parameters[[i]]$cov <- diag(k)

    # Check whether beta specified
    # If not, rep(1,k)
    if(is.null(parameters[[i]]$beta)){
      parameters[[i]]$beta <- rep(1,k)
    }else if(!(is.vector(parameters[[i]]$beta) || is.matrix(parameters[[i]]$beta))){
      stop("'beta' should be a vector or matrix")
    }

    ## Check whether number of levels is specified
    # - if no take from data structure 
    # - if yes check it matches data structure 	
    if(is.null(parameters[[i]]$n_level)){
	    if(i=="residual") {
          parameters[[i]]$n_level <- nrow(data_structure)
      } else {
		  parameters[[i]]$n_level <- length(unique(data_structure[,parameters[[i]]$group]))
      }
	  } else {
      
	  }

  
  }

  ## Check whether all names in data_structure and parameters contain only words, digits and _
  if(!all(grepl("^\\w+$",c(do.call(c,lapply(parameters,function(x) x$names)), colnames(data_structure))))) stop("Names in data structure and in parameters must be letters, numbers or _")

  ##Check extra parameters
  param_names <- c("names", "group", "mean", "cov", "beta", "n_level")

  e_p <- unlist(sapply(parameters, function(x){
    names(x)[!names(x) %in% param_names]
    }))

  if(length(e_p)>0){
    ## check is extra parameters are vectors
    e_p_vector <- !unlist(sapply(parameters, function(x){
      sapply(x[!names(x) %in% param_names],is.vector)
      }))
    if(any(e_p_vector)) stop("Additional parameters given to parameters lists must be vectors, this is not the case for ",names(e_p_vector)[e_p_vector])
  
    ## check length of all extra parameters is 1
    e_p_length <- unlist(sapply(parameters, function(x){
      sapply(x[!names(x) %in% param_names],length)
      }))
    if(any(e_p_length>1)) stop("Additional parameters given to parameters lists must be length 1, this is not the case for ",names(e_p_length)[e_p_length>1])
  }

	return(parameters)

}



### possibly make it so that if data_structure is not specified then make one with completely crossed random effects?
### then you would need to specify the number of levels