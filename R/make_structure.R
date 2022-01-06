is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  
	if(is.numeric(x)){
		return(abs(x - round(x)) < tol)
	}else{
		return(FALSE)
	}
	return(FALSE)
}

## function to get sample size out of structure
extract_N <- function(x){
 N <- as.numeric(sub("\\w+\\((\\d+)\\)","\\1",x))
 # if(length(N)>1) for(i in 1:(length(N)-1)) if(!is.wholenumber(N[i+1]/N[i])) stop("Nested factors need to be completely balanced ", call.=FALSE) ##better error message needed
 return(N)
}

## function to get grouping level names out of structure
extract_name <- function(x) sub("(\\w+)\\(\\d+\\)","\\1",x)


## generate levels with simple nested design
# generate_levels <- function(x){
# 	levels <- matrix(NA,x[length(x)],length(x))
# 	for(i in (length(x)):1) levels[,i]<- rep(1:x[i],each=x[length(x)]/x[i])
# 	return(levels)
# } 
generate_levels <- function(x){
	levels <- matrix(NA,prod(x),length(x))
	for(i in (length(x)):1) levels[,i]<- rep(1:prod(x[i:1]),each=prod(x)/prod(x[i:1]))
	return(levels)
} 

cross_levels <-function(x){
	crossed_lower_levels <- expand.grid(lapply(x,function(i) i[,ncol(i)])[length(x):1])[,length(x):1,drop=FALSE]    
	# crossed_lower_levels <- expand.grid(lapply(x,function(i) i[,ncol(i)]))  
    do.call(cbind,lapply( 1:ncol(crossed_lower_levels), function(j) x[[j]][crossed_lower_levels[,j],] ))
}

add_interactions <- function(all_levels, int){	
	int_str <-do.call(cbind,lapply(strsplit(int, ":"), function(x){
			z<- apply(all_levels[,x],2,function(y) {
			  char_dif <- max(nchar(y)) - nchar(y)
			  paste0(sapply(1:length(char_dif), function(i) paste0(rep(0,each=char_dif[i]),collapse="")),y)
	    })
			as.numeric(as.factor(apply(z, 1, paste, collapse="")))
		}))
	colnames(int_str)<-int
	cbind(all_levels,int_str)

}


#' Make nested and crossed balanced hierarchical structures
#'
#' @param structure A formula specifying the structure and sample sizes at each level. See details.
#' @param repeat_obs Number of repeated observations at the lowest level
#' @param ... Further arguments passed to or from other methods.
#' @details Factors are input as a text string. The name of each factor is followed by the number of levels in that factor in brackets e.g. "individual(100)". Nested factors can be specified using "/", e.g. "population(2)/individual(2)", the lower levels being specified after the higher levels, and the sample sizes of the lower levels 
#' @return 
#' @examples
#' 
#' @export


make_structure <- function(structure, repeat_obs=1,...){

	## strip white space from structure
	structure <- gsub("\\s","",structure)

    if(!grepl("^[A-z0-9_:/()+]*$",structure)) stop("Characters in structure must be alphanumeric, '_', '/', '+', '(', ')' or ':'", call.=FALSE)

	## separate into crossed and nested components
	components <- strsplit(structure, "\\+")[[1]]
	comp2 <- 	components[!grepl(":",components)]
	int <- 	components[grep(":",components)]

	## split nested grouping levels
	comp_list <- strsplit(comp2, "\\/")

	## separate names and sample sizes
	comp_list_N <- lapply(comp_list,extract_N)
	comp_names <- do.call(c,lapply(comp_list,extract_name))

	## apply generate levels function to all components	
	comp_levels <- lapply(comp_list_N,generate_levels)

	## do crossing
	all_levels <- cross_levels(comp_levels)

	## give names to matrix
	colnames(all_levels) <- comp_names

	## add in interaction columns
	all_levels_int<- if(length(int)>0){
		add_interactions(all_levels, int)
	}else{
		all_levels
	}

	## repeat levels for number of time steps
	repeat_levels <- all_levels_int[rep(1:nrow(all_levels_int),each=repeat_obs),,drop=FALSE] 

	return(as.data.frame(repeat_levels))
}

## potential error messages
## can only be 
## - ) needs to be followed by +, / or end