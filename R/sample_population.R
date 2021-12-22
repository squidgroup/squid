rm(list=ls())


devtools::load_all("~/github/squid/R")

pop_data <- simulate_population(
  data_structure=make_structure("individual(200)",repeat_obs=20),
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



simple_sampling <- function(pop_data, FUN){
				# check that all levels of factor have at least the maximum samples

	data_structure <- pop_data$data_structure


simple = c("individual(50)/observation(10)","individual(100)/observation(5)")
structure=simple
structure <- gsub("\\s","",structure)
comp_list <- strsplit(structure, "\\/")
comp_list_N <- lapply(comp_list,extract_N)
comp_names <- lapply(comp_list,extract_name)
	

FUN = list(individual= c(50, 100), observation=c(10, 5), sample_order = c("individual","observation"))
# simple = list(individual= c(15, 10, 5))



## observation level
groups <- FUN[["sample_order"]]
i <- 1

index<-which(data_structure[[groups[i]]] %in% sample(unique(data_structure[[groups[i]]]), FUN[[groups[i]]][1], replace=FALSE))
i2 <- sample_group(data_structure[index,groups[i]],FUN[[groups[i+1]]][1])
table(data_structure[i2,])


index<-which(data_structure[[groups[i]]] %in% sample(unique(data_structure[[groups[i]]]), FUN[[groups[i]]][2], replace=FALSE))
i2<-sample_group(data_structure[index,groups[i]],FUN[[groups[i+1]]][2])
table(data_structure[i2,])

		# for(i in colnames(data_structure)){
		# 	if(min(table(data_structure[,i]) ) < max(simple[[i]])) stop("not all levels of ",i, " have the maximum number of samples")
		# }
		
		# 

		data_structure[sample_group(data_structure$individual),]

		# sample_population( simple = list(ind= c(50, 10), pop=c(2, 10), sample_order = “pop + ind”) )
		# “pop(2) + ind(50)”
		# Completely crossed - doesnt matter the order of sampling?
		# Only Nested - does matter the order

		# Ind, 50, 100
		# Pop, 10, 5

		# At each level, user can specify proportion or integer of samples at a given level, either fixed or variable, if proportion then round to nearest integer, if variable use poisson
		# Apply separately to each response.

}

complex = c("0.5 * body size")


complex_sampling <- function(pop_data, FUN){
			## check that missingness predictors are are in y or predictors
		## by default center and scale predictors

		## option to plot missingness function
			
			## put together y and predictors and scale
			dat<- as.data.frame(apply(cbind(pop_data$y[[1]],squid_data$predictors[[1]]),2,scale) )

			# FUN= c(0, "0.5*environment", "0.25*y")
			l <- sapply(FUN, function(x) {
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




sample_population <- function(pop_data, simple, complex, temporal, plot=FALSE){
	

	data_structure <- pop_data$data_structure


	if(!is.null(simple)){
		indices <- simple_sampling(pop_data, simple)
	}
	if(!is.null(complex)){
		indices <- complex_sampling(pop_data, complex)
	}
	if(!is.null(temporal)){
		# Which grouping factor is time
		# Which grouping factor is temporally dependent
		# Sampling parameters - between group variance in sampling across time
		# list(time = c(day, month), group = c(ind, pop), variance = c(0.5, 0,6))

	}
}




get_sample_data <- function(){

}