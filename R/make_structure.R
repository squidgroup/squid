is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){  
	if(is.numeric(x)){
		return(abs(x - round(x)) < tol)
	}else{
		return(FALSE)
	}
	return(FALSE)
}

## function to get sample size out of structure
extract_N <- function(x) as.numeric(sub("\\w+\\((\\d+)\\)","\\1",x))

## function to get grouping level names out of structure
extract_name <- function(x) sub("(\\w+)\\(\\d+\\)","\\1",x)

## function to generate grouping levels
generate_levels <- function(x, N, balanced=TRUE, randomise=TRUE){

	Ns <- c(x,N)

	levels <- matrix(NA,N,length(x))

	if(balanced){
		for(i in (length(x)):1){
			levels[,i]<- rep(1:Ns[i],each=N/Ns[i])
		}
	}else{
		## start with lowest level, sample all levels once then, sample with replacement to get uniform variation in sample
		## do the same with the next level based on the number of the lower levels rather than the total sample size
		## if samples completely randomly, some levels will have zero and so the number of levels won't be consistent with that specified. To avoid this we need to sample each one ones, and then sample all again with replacement
		for(i in length(x):1){
			if(i==length(x)){
				levels[,i] <- sort(c(1:Ns[i],sample(1:Ns[i],N-Ns[i], replace=TRUE)))
			}else{
				a <- table(levels[,i+1])
				b <- sort(c(1:Ns[i],sample(1:Ns[i],Ns[i+1]-Ns[i], replace=TRUE)))
				levels[,i] <- do.call(c,lapply(1:Ns[i+1],function(j) rep(b[j],a[j])))
			}
		}
	}

	# randomise the order - this helps with creating crossed random effects
	if(randomise) levels <- levels[sample(1:N), ]
	return(levels)
} 

## for tests for package - make a function to test whether its nested or not 
## - any(colSums(table(x,y)>0)>0)




## balanced - is the number of observations within each grouping level the same?
## - does not ensure that they are balanced across grouping levels
## need to add in warnings if chose balanced and the numbers don't match
make_structure <- function(structure, N, balanced=TRUE){
	## strip white space from structure
	structure <- gsub("\\s","",structure)

	## separate into crossed and nested components
	components <- strsplit(structure, "\\+")[[1]]

	## split nested grouping levels
	comp_list <- strsplit(components, "\\/")

	## separate names and sample sizes
	comp_list_N <- lapply(comp_list,extract_N)
	comp_names <- do.call(c,lapply(comp_list,extract_name))

	## apply generate levels function to all components	
	## randomise the order to make it crossed with the other random effects
	all_levels <- do.call(cbind,lapply(comp_list_N,generate_levels,N=N, balanced=balanced, randomise=TRUE))

	## give names to matrix
	colnames(all_levels) <- comp_names

	return(all_levels)
}




##problems:
## for example nest can be nested within mother and year, but mother and year are crossed - difficult to make a syntax that can create this, so I think we provide a simple function but also allow users to put in their own structure

### a fundamental problem is that the data generation process, especially in terms of data structure, might not be the same as that which we use to model the data. But here we are simulating data according to a model. Not using the phenoptype in the data structure simulation will mean that we cant simulate some of the biological properties of different data structures.

### two grouping levels that are nested within the same grouping level, but crossed with each other like nest of rearing and nest of origin are nested in year but crossed with each other
# something like  sex[2] + year[20]/(nest[25] + individual[250])

