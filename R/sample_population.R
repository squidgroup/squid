
## function which tells whether consecutive columns are nested, assuming that the first one is the highest level
is.nested <-function(x) sapply(2:ncol(x), function(i) nrow(unique(x[,c(i-1,i)])) == length(unique(x[,i])) )

## function
## param 
sample_nested <- function(data_structure, param, plot=FALSE){
	
	groups <- colnames(param)
	
	## check naming
	if(!all(groups %in% c(colnames(data_structure),"observation"))){
		stop("Names in sample order need to be in data_structure")
	}
	
	## if there isn't an observation level specified make one
	if("observation" %in% groups & !"observation" %in% colnames(data_structure)){
	 data_structure[,"observation"] <- 1:nrow(data_structure)
	}

	## test if nested
	if(!all(is.nested(data_structure[,groups]))){
		stop("Grouping factors are not nested in the order provided")
	}

	# check that all levels of factor have at least the maximum samples

		# for(i in colnames(data_structure)){
		# 	if(min(table(data_structure[,i]) ) < max(simple[[i]])) stop("not all levels of ",i, " have the maximum number of samples")
		# }

	# for (i in colnames(param)){
	# 	if(length(unique(data_structure[,i])) < max(param[,i])) stop("not all levels of ",i, " have the maximum number of samples")
	# }

		

	# for each parameter set (row in param)
	apply(param,1, function(j){
		
		## get the right number of levels for the highest group
		index<-which(data_structure[,groups[1]] %in% sample(unique(data_structure[,groups[1]]), j[1], replace=FALSE))

		##then cascade through nested groups to get right number of levels within each
		if(ncol(param)>1){
			for(i in 2:ncol(param)){
				index_new <- sort(which(data_structure[index,groups[i]] %in% c(tapply(data_structure[index,groups[i]],data_structure[index,groups[i-1]], function(x) sample(unique(x),j[i], replace=FALSE)), recursive=TRUE)))
				index <- index[index_new]
			}	
		}

		index
	}, simplify=FALSE)

		# At each level, user can specify proportion or integer of samples at a given level if proportion then round to nearest integer?
		# Apply separately to each response?

# possible alternative data entry
# simple = c("individual(50)/observation(10)","individual(100)/observation(5)")
# structure=simple
# structure <- gsub("\\s","",structure)
# comp_list <- strsplit(structure, "\\/")
# comp_list_N <- lapply(comp_list,extract_N)
# comp_names <- lapply(comp_list,extract_name)

}


sample_missing <- function(pop_data, param, plot=FALSE){
	## check that missingness predictors are are in y or predictors (error message will be given if they;re not, but might be worth making more informative one)
	## by default center and scale predictors
	## option to plot missingness function
		
	lapply(1:pop_data$N_pop, function(i){	

		## put together y and predictors and scale
		## scaling means that all coefficients are comparable
		dat <- as.data.frame(apply(cbind(pop_data$y[[i]],pop_data$predictors[[i]]),2,scale) )
	
			l <- sapply(param, function(x) {
				y <- eval(parse(text=x),envir = dat)
			  if(length(y)==1) y <- rep(y, nrow(dat))
			  return(y)
			})
			e <- stats::plogis(l)
			o <- apply(e,2,function(x)as.logical(stats::rbinom(length(x),1,x)))
			apply(o,2,which)
	})
	# could make total N constant ;using the probabilities e with sample

	# plot(dat$env,o)
	# plot(dat$ind,o)
	# plot(l,o)

	# x <- seq(-4,4,0.1)
	# plot(x,stats::plogis(x*0.5), type="l",ylim=c(0,1))
	# plot(x,stats::rbinom(length(x),1,stats::plogis(x*0.5)),ylim=c(0,1))

}




sample_temporal <- function(data_structure, time, group, variance, N, plot=FALSE){
	# Which grouping factor is time
	# Which grouping factor is temporally dependent
	# Sampling parameters - between group variance in sampling across time
	# list(time = c(day, month), group = c(ind, pop), variance = c(0.5, 0,6))

	all_levels <- unique(data_structure[,group])
  N_levels <- length(all_levels)

  Tsamp <- length(unique(data_structure[,time]))
  # Tsamp - number of time points

  Tmin <- min(unique(data_structure[,time]))
  ## work out within group and between group range
  TsampB <- round(Tsamp*variance,0)
  TsampW <- Tsamp-TsampB
  ## 
  if(! TsampW >= N) stop("Number of time steps not enough to implement this varaince")


  if(plot){
    plot(NA, xlim=c(1,Tsamp), ylim=c(1,N_levels))
    graphics::abline( h=1:N_levels, col="grey")
  }

  indices <- sort(c(lapply(1:N_levels, function(x){ 
        Tx <- sort(sample(1:TsampW,N, replace=FALSE)) + sample(1:TsampB,1) + Tmin -1
        if(plot) graphics::points(Tx,rep(x,N), pch=19, col=x)
        Tx
        all_levels[x]
        index1 <- which(data_structure[,group]==all_levels[x])
        index1[which(data_structure[index1,time] %in% Tx)]
        
      }), recursive=TRUE))
  if(plot)graphics::points(individual~day,data_structure[indices,])

  indices
}




#' @title sample_population
#' @description Sample population level data
#' @param x A squid object, created using simulate_population().
#' @param type Type of sampling, needs to be one of 'nested', 'missing' or temporal. See details.
#' @param param A set of parameters, specific to the sampling type. See details.
#' @param plot Logical. Should illustrative plots be made - defaults to FALSE.
#' @details ...
#' @return 
#' @examples
#' 
#' @export
sample_population <- function(x, type, param, plot=FALSE){
	
	if(class(x) != "squid"){
		stop("x needs to be class 'squid'")
	}

	if(type=="nested"){
		if(!is.matrix(param)){
		  stop("param needs to be a matrix for type='nested'")
		}
		indices <- lapply(1:x$N_pop,function(y) sample_nested(x$data_structure, param, plot))
	
	}else if(type=="missing"){
		if(!is.vector(param)){
		  stop("param needs to be a vector for type='missing'")
		}
		indices <- sample_missing(x, param, plot)
	
	}else if(type=="temporal"){
		if(!is.list(param)){
		  stop("param needs to be a list for type='temporal'")
		}
		if(!all(sapply(param,is.vector))){
		  stop("All elements of param list must be vectors for type='temporal'")
		}
		vec_length <- sapply(param,length)
		if(!all(vec_length%in%c(1,max(vec_length)))){
		  stop("vectors in param list must be same length or length 1, for type='temporal'")
		}
		param <- lapply(param,function(y) if(length(y)==1) {rep(y,max(vec_length))} else {y})
		indices <- lapply(1:x$N_pop,function(y) { # for each population
			lapply(1:max(vec_length), function(i) { # for each set of parameters
				do.call(sample_temporal, c(list(data_structure=x$data_structure),lapply(param,function(z) z[i]), plot=FALSE)) # 
			})
		})
	}else {
		stop("type must be 'nested', 'missing' or 'temporal'")
	}

	pop_data$sample_param <- list(type=type, param=param)

	pop_data$samples <- indices
	pop_data
	## apply to each population 
	## incorporate into squid object

## plotting - maybe plot only first population, but all parameter combinations

}



#' @title get_sample_data
#' @description Extracts sampled data from a squid object
#' @param x an R object of class 'squid'
#' @param sample_set Integer - which sample set to return. Defaults to 1
#' @param list Logical - whether to return data as a list or data_table (FALSE; default).
#' @param ... further arguments passed to or from other methods.
#' @export
get_sample_data <- function(x, sample_set=1, list=FALSE,...){
  
  pop_list <- lapply(1:x$N_pop,function(i) {
  	data.table(cbind(x$y[[i]],x$predictors[[i]],x$data_structure,squid_pop=i)[x$samples[[i]][[sample_set]],])
  })
  
  if(list){
    return(pop_list)
  }else{
    do.call(rbind,pop_list)
  }
}

## think it would be a good idea to provide functionality that allowed a user to chose different variables to have missing data
