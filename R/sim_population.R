
#' @title sim_population
#' @description Simulate population level data
#' @param parameters A list of parameters for each hierarchical level. See details.
#' @param data_structure A matrix or dataframe with a named column for each grouping factor, inuding the levels
#' @param model Optional. 
#' @param family A description of the error distribution. Default "gaussian".
#' @param link A description of the link function distribution. Default "identity".
#' @param pedigree A list of pedigrees for each hierarchical level
#' @details Parameter list ... 
#' @return 
#' @examples
#' 
#' @export
#' @import MCMCglmm
sim_population <- function(parameters, data_structure, model, family="gaussian", link="identity", pedigree,N, N_pop=1){

  if(!all(link %in% c("identity", "log", "inverse", "sqrt", "logit", "probit"))) stop("Link must be 'identity', 'log', 'inverse', 'sqrt', 'logit', 'probit'")
  if(!all(family %in% c("gaussian", "poisson", "binomial"))) stop("Family must be 'gaussian', 'poisson', 'binomial'")
  
  if(missing("N") & missing("data_structure")){
    stop("Either N or data_structure need to be specified")
  }else if(missing("N")){
    N <- nrow(data_structure)
  }else if(!missing("N") & !missing("data_structure")){
    if(nrow(data_structure)!=N) stop("N and nrow(data_structure) are not equal. Only one needs to be specified.")
  }
  

  ## gets the arguments into a list that is added to for the output
  output <- lapply(as.list(environment()), function(x) if (length(x)==1 && x=="") NULL else x)

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
###---PEDIGREE  
#####################  

  ## check pedigree is list, make one if not
  if(missing(pedigree)){
    output$pedigree <- pedigree <-list()
  }else{
    if(!is.list(pedigree) | is.data.frame(pedigree)) stop("pedigree needs to be a list")
  }

  ## check pedigree levels match data structure levels
  ped_check <- lapply(names(pedigree),function(i){
  # data_structure[,i]
    if(!all(unique(pedigree[[i]][,1]) %in% unique(data_structure[,output$parameters[[i]]$group]))) stop(paste("all individuals in the pedigree linked with", i, "are not in the data_structure"))
    if(!all(unique(data_structure[,output$parameters[[i]]$group]) %in% unique(pedigree[[i]][,1]))) stop(paste("all individuals in data_structure are not in the pedigree linked with", i))      
  })


## within sim_predictors, use factorise_ped, make sure it matches. maybe somehow factorise them together?
  

  # unique(data_structure[,param[[i]]$group])
  # unique(pedigree[[i]][,1])


#####################  
###---PREDICTORS 
#####################  


  #output$predictors <- do.call(sim_predictors, output)
  output$predictors <- lapply(1:N_pop, function(x) do.call(sim_predictors, output))
  ## returns list of predictor matrices

#####################  
###---GENERATE Y
##################### 

  # y <- do.call(generate_y, output)
  y <- do.call(generate_y_list, output)

#####################  
###---TRANSFORM Y 
##################### 

  output$y <- lapply(y, function(x) transform_dist(x, family, link))

  # in output predictors, if name matches something in data_structure, then append "_effects"

## might be ble to take this out as it should happen anymore
  # matching_names <- colnames(predictors) %in% colnames(data_structure)
  # colnames(predictors)[matching_names] <- paste0(colnames(predictors)[matching_names],"_effects")
  

  # out <- as.data.frame(cbind(y_family,predictors,data_structure))
  
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

#' @title pop_data
#' @description Extracts population level data from a squid object
#' @param x an R object of class 'squid'
#' @param list Logical - whether to return data as a list or data_table (FALSE; default).
#' @param ... further arguments passed to or from other methods.
#' @export
pop_data <- function(x,list=FALSE,...){

  # data.table(cbind(x$y,x$predictors,x$data_structure))

  pop_list <- lapply(1:x$N_pop,function(i) data.table(cbind(x$y[[i]],x$predictors[[i]],x$data_structure,squid_pop=i)))

  if(list){
    return(pop_list)
  }else{
    do.call(rbind,pop_list)
  }

}


