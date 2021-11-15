
#' @title simulate_population
#' @description Simulate population level data
#' @param parameters A list of parameters for each hierarchical level. See details.
#' @param data_structure A matrix or dataframe with a named column for each grouping factor, including the levels
#' @param model Optional. 
#' @param family A description of the error distribution. Default "gaussian".
#' @param link A description of the link function distribution. Default "identity".
#' @param pedigree A list of pedigrees for each hierarchical level. Each pedigree must be matrix or data.frame, that is at least 3 columns, which correspond to ID, dam and sire.
#' @param pedigree_type A list describing what kind of genetic variance is to be simulated from each pedigree. Default is 'additive', other options are 'dominance' and 'epistatic'. Makes use of relationship matrices created by the nadiv package.
#' @param phylogeny A list of phylogenies for each hierarchical level. Each pedigree should be phylo class.
#' @param phylogeny_type A list describing what mode of evolution should be simulated from each phylogeny. Options are 'brownian'(default) or 'OU'. 
#' @param cov_str A list of covariance structures for each hierarchical level. 
#' @param N Sample size when data_structure is not specified
#' @param N_pop Number of populations. Default = 1
#' @details Parameter list ... 
#' @return 
#' @examples
#' 
#' @export
#' @import nadiv
#' @import ape
#' @import Matrix
simulate_population <- function(parameters, data_structure, model, family="gaussian", link="identity", pedigree, pedigree_type, phylogeny, phylogeny_type, cov_str, N, N_pop=1, known_predictors, extra_betas){

  if(!all(link %in% c("identity", "log", "inverse", "sqrt", "logit", "probit"))) stop("Link must be 'identity', 'log', 'inverse', 'sqrt', 'logit', 'probit'")
  if(!all(family %in% c("gaussian", "poisson", "binomial"))) stop("Family must be 'gaussian', 'poisson', 'binomial'")
  
  if(missing("N") & missing("data_structure")){
    stop("Either N or data_structure need to be specified")
  }else if(missing("N")){
    N <- nrow(data_structure)
  }else if(!missing("N") & !missing("data_structure")){
    if(nrow(data_structure)!=N) stop("N and nrow(data_structure) are not equal. Only one needs to be specified.")
  }
  
  # if(!missing("known_predictors")& !missing("data_structure")){
  #   if(nrow(data_structure)!=nrow(known_predictors)) stop("data_structure and known_predictors need to be the same length.")
  # }

  ## gets the arguments into a list that is added to for the output
  output <- lapply(as.list(environment()), function(x) if (length(x)==1 && x=="") NULL else x)

#####################  
###---Fill in parameter lists 
##################### 

  output$parameters <- do.call(fill_parameters, output)

  j <- n_phenotypes(output$parameters)

  if(j > 1 & !missing("model")) stop("Currently cannot specify multiple responses and a model formula")

  if(!(length(link)==j || length(link)==1)){
    stop("Link must either be length 1 or same length as the number of parameters")
  }
  if(!(length(family)==j || length(family)==1)){
    stop("Family must either be length 1 or same length as the number of parameters")
  }

#####################  
###---cov structures
#####################  


  ## check pedigree levels match data structure levels
  ## make function - that can check ped,phylo and covs

  output$cov_str <- do.call(cov_str_list, output)
  ## make cov_str with everything, then return it back to cov_str after predictors


  ## MCAR, sample a given number 
  ## MAR, sample a given number based of probability given by a predictor
  ## MNAR, sample a given number based of probability given by that response


#####################  
###---PREDICTORS 
#####################  

  output$predictors <- lapply(1:N_pop, function(x) do.call(sim_predictors, output))
  # output$predictors <- lapply(1:N_pop, function(x) cbind(do.call(sim_predictors, output), known_predictors))
  ## returns list of predictor matrices

  # output$cov_str <- cov_str

#####################  
###---GENERATE Y
##################### 

  # y <- do.call(generate_y, output)
  y <- do.call(generate_y_list, output)

#####################  
###---TRANSFORM Y 
##################### 

  output$y <- lapply(y, function(x) transform_dist(x, family, link))
  
  class(output) <- 'squid'
  return(output)
}

## problem that by default the predictors and the level IDs will have the same names
## - maybe append "_effects"


#' @title print.squid
#' @description Print method for class 'squid'
#' @param x an R object of class 'squid'
#' @param ... further arguments passed to or from other methods.
#' @export
print.squid <- function(x, ...){
  cat("Data simulated using squid \n
              /\\             
             /  \\            
            / /\\ \\           
            \\/  \\/            
            /    \\           
            |    |          
            |    |          
      0     |    |      0     
     /      \\____/      \\    
    {     __/(  )\\__     }   
     \\___/__\\_\\/_/__\\___/    
      / / / /    \\ \\ \\ \\     
     / / / {      } \\ \\ \\    
    { { /   \\    /   \\ } }   
    }  \\     0  0     /  {   
 0_/  { \\_0        0_/ }  \\_0
       \\              /      
        }            {       
       /              \\      
      0                0      
  
    ")
}


#' @title summary.squid
#' @description summary method for class 'squid'
#' @param object an R object of class 'squid'
#' @param ... further arguments passed to or from other methods.
#' @export
summary.squid <- function(object, ...){
  
}

#' @title get_population_data
#' @description Extracts population level data from a squid object
#' @param x an R object of class 'squid'
#' @param list Logical - whether to return data as a list or data_table (FALSE; default).
#' @param ... further arguments passed to or from other methods.
#' @export
get_population_data <- function(x,list=FALSE,...){

  # data.table(cbind(x$y,x$predictors,x$data_structure))

  pop_list <- lapply(1:x$N_pop,function(i) data.table(cbind(x$y[[i]],x$predictors[[i]],x$data_structure,squid_pop=i)))

  if(list){
    return(pop_list)
  }else{
    do.call(rbind,pop_list)
  }

}


#' @title get_parameters
#' @description Extracts population level data from a squid object
#' @param x an R object of class 'squid'
#' @export

get_parameters <- function(x){
  param<- lapply(x$parameters,function(y){

    means <- y$mean
    names(means) <- paste0(y$names,"_mean")

    vars <- diag(y$vcov)
    names(vars) <- paste0(y$names,"_var")

    if(ncol(y$vcov)>1){ 
      covs <- y$vcov[lower.tri(y$vcov)]
      name_ind<-which(lower.tri(y$vcov), arr.ind=TRUE)
      names(covs) <- paste0(y$names[name_ind[,2]],":",y$names[name_ind[,1]],"_cov")
    }else{
      covs<-NULL
    }

    betas <- as.vector(y$beta)
    names(betas) <- if(y$n_response==1){ 
      paste0(y$names,"_beta") 
    }else{ 
      paste0(rep(y$names,y$n_response),"_beta_y",rep(1:y$n_response, each=nrow(y$beta) )) 
    }

    c(means,vars,covs,betas)  
  })
  names(param)<-NULL
  c(param, recursive=TRUE)

  ## extra_parameters
}