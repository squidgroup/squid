# rm(list=ls())


# devtools::load_all("~/github/squid/R")

pop_data <- simulate_population(
  data_structure=make_structure("nest(10)/individual(20)",repeat_obs=20),
  parameters = list(
    individual = list( 
      vcov = 0.1
    ),
    observation= list(
      names = c("environment"),
      beta =c(0.5)
    ), 
    residual = list(
      vcov = 0.8
    )
  )
)



sample_group <- function(x,n){
	if(min(table(x)) < n) stop("not all levels have the maximum number of samples", call. = FALSE)
	sort(c(lapply(unique(x), function(y) sample(which(x==y),n, replace=FALSE) ), recursive=TRUE))
}


	data_structure <- pop_data$data_structure
# param = list( N = cbind(individual=c(50, 100),observation=c(10, 5)), order = c("individual","observation"))

param = cbind(nest=c(2,10),individual=c(10, 15),observation=c(10, 5))
## columns are named, and order with highest hierarchical level first (order in which sampling is done)

# simple = c("individual(50)/observation(10)","individual(100)/observation(5)")
# structure=simple
# structure <- gsub("\\s","",structure)
# comp_list <- strsplit(structure, "\\/")
# comp_list_N <- lapply(comp_list,extract_N)
# comp_names <- lapply(comp_list,extract_name)

nested_sampling <- function(data_structure, param){
				# check that all levels of factor have at least the maximum samples
	
	if(!all(colnames(param) %in% c(colnames(data_structure),"observation"))) stop("Names in sample order need to be in data_structure")
	if("observation" %in% colnames(param) & !"observation" %in% colnames(data_structure)) data_structure[,"observation"] <- 1:nrow(data_structure)

		# for(i in colnames(data_structure)){
		# 	if(min(table(data_structure[,i]) ) < max(simple[[i]])) stop("not all levels of ",i, " have the maximum number of samples")
		# }

	# for (i in colnames(param)){
	# 	if(length(unique(data_structure[,i])) < max(param[,i])) stop("not all levels of ",i, " have the maximum number of samples")
	# }

		

apply(param,1, function(x){

})

## observation level
groups <- colnames(param)
j <- 2
# i=2
for(i in 1:ncol(param)){
	if(i==1){
		index1<-which(data_structure[,groups[i]] %in% sample(unique(data_structure[,groups[i]]), param[,groups[i]][j], replace=FALSE))
	}else{
		index_new <- sample_group(data_structure[index,groups[i-1]],param[,groups[i]][j])
		index <- index[index_new]
	}
}

sample_group <- function(x,n) which(x %in% sample(unique(x), n, replace=FALSE))

index1 <- sample_group(data_structure[,groups[1]],param[,groups[1]][j])
dat2 <- data_structure[index1,]


sample(unique(x)


split(dat2,dat2)
lapply(split(data_structure[index1,],data_structure[index1,groups[i]]), )



index<-which(data_structure[,groups[1]] %in% sample(unique(data_structure[,groups[1]]), param[,groups[1]][j], replace=FALSE))

if(ncol(param)>1)
for(i in 2:ncol(param)){
		index_new <- sort(which(data_structure[index,groups[i]] %in%c(tapply(data_structure[index,groups[i]],data_structure[index,groups[i-1]],function(x) sample(unique(x),param[,groups[i]][j], replace=FALSE)), recursive=TRUE)))
		index <- index[index_new]
	}
}



# length(unique(data_structure[,"individual"]))
# unique(table(data_structure[,"individual"]))

# length(table(data_structure[,"nest"]))
# unique(table(data_structure[,"nest"]))

# aggregate(observation~nest+individual,data_structure,length)
# aggregate(observation~nest+individual,data_structure,length)

# unique(table(data_structure[,"individual"]))
# unique(table(data_structure[,"nest"]))/unique(table(data_structure[,"individual"]))

length(unique(data_structure[index,"nest"]))
length(unique(data_structure[index,"individual"]))
unique(table(data_structure[index,"individual"]))

data_structure[index,]

index<-which(data_structure[[groups[i]]] %in% sample(unique(data_structure[[groups[i]]]), param[[groups[i]]][1], replace=FALSE))
i2 <- sample_group(data_structure[index,groups[i]],param[[groups[i+1]]][1])
table(data_structure[i2,])


index<-which(data_structure[[groups[i]]] %in% sample(unique(data_structure[[groups[i]]]), param[[groups[i]]][2], replace=FALSE))
i2<-sample_group(data_structure[index,groups[i]],param[[groups[i+1]]][2])
table(data_structure[i2,])



		# At each level, user can specify proportion or integer of samples at a given level if proportion then round to nearest integer?
		# Apply separately to each response?

}

complex_sampling <- function(pop_data, param){
			## check that missingness predictors are are in y or predictors
		## by default center and scale predictors

		## option to plot missingness function
			
			## put together y and predictors and scale
			dat<- as.data.frame(apply(cbind(pop_data$y[[1]],squid_data$predictors[[1]]),2,scale) )

			# FUN= c(0, "0.5*environment", "0.25*y")
			l <- sapply(param, function(x) {
				y <- eval(parse(text=x),envir = dat)
			  if(length(y)==1) y <- rep(y, nrow(dat))
			  return(y)
			})
			## scaling means that all coefficients are comparable
			e <- plogis(l)
			o <- apply(e,2,function(x)as.logical(rbinom(length(x),1,x)))
			apply(o,2,which)
			# could make total N constant ;using the probabilities e with sample

			# plot(dat$env,o)
			# plot(dat$ind,o)
			# plot(l,o)


		# x <- seq(-4,4,0.1)
		# plot(x,plogis(x*0.5), type="l",ylim=c(0,1))
		# plot(x,rbinom(length(x),1,plogis(x*0.5)),ylim=c(0,1))

		# Logistic function - probability of being sampled
		# MNAR Y = beta_1 * body size
		# MNAR Y = beta_1 * (temp +residual)
		# MAR Y = beta_2*temp
}


temporal_sampling <- function(pop_data, param){
# Which grouping factor is time
		# Which grouping factor is temporally dependent
		# Sampling parameters - between group variance in sampling across time
		# list(time = c(day, month), group = c(ind, pop), variance = c(0.5, 0,6))

}



sample_population <- function(pop_data, type, param, plot=FALSE){
	

	data_structure <- pop_data$data_structure


	if(type=="nested"){
		indices <- nested_sampling(pop_data$data_structure, param)
	}else if(type=="complex"){
		indices <- complex_sampling(pop_data, param)
	}else if(type=="temporal"){
		indices <- temporal_sampling(pop_data, param)
	}else stop("type must be 'nested', 'complex' or 'temporal'")

	## incorporate into squid object
}




get_sample_data <- function(){

}