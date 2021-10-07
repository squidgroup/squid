#make_world

## what do we output - how do we incorporate alive individuals that don't breed or multiple measurements
## output pedigree and data structure?
rm(list=ls())
options(stringsAsFactors=FALSE)

years <- 5
n_females <- 50
P_breed <- 1
fecundity <- 3
# 
polgamy_rate <- 0
polgyny_rate <- 0
P_fert <- 1
juv_surv <- 0.1
adult_surv <- 0.5
migration <- NULL
constant_pop <- FALSE 


# start_females <- paste0("d",1:n_females)
# start_males <- paste0("s",1:n_females)




# dat[[1]] <- start_dat <- data.frame(ID = c(paste0("d",1:n_females),paste0("s",1:n_females)), sex = rep(c("F","M"),each=n_females), age=rgeom(n_females*2,adult_surv)+1)
## think about using rgeom here = should it be different for year 1
## do we start with age unknown as that would be realistic to the sampling?
# plot(table(rgeom(1000,0.5)+1))
# dgeom(0:10,0.5)


pedigree <- data.frame(
	animal = paste0("0_",1:(n_females*2)),
	dam = NA,
	sire = NA,
	sex = rep(c("F","M"),each=n_females),
	cohort=NA
)

dat <- list()
dat[[1]] <-  data.frame(animal = pedigree$animal, sex = pedigree$sex, age=NA)
## think about using rgeom here = should it be different for year 1
## do we start with age unknown as that would be realistic to the sampling?
# plot(table(rgeom(1000,0.5)+1))
# dgeom(0:10,0.5)

# year=1
for(year in 1:years){
	## import individuals that are around as adults pre-breeding (dat[[year]])

	# probability of breeding just on females? assume that male breeding is dependent on females?
	females <- subset(dat[[year]],sex=="F")$animal
	breeding_females <- females[as.logical(rbinom(length(females),1,P_breed))]
	males <- subset(dat[[year]],sex=="M")$animal

	# monogamy
	n_pair <- min(length(breeding_females),length(males))

	# number of offspring per female - random mating, equal sex ratio
	fec <- rpois(n_pair,fecundity*P_fert)

	ped <- data.frame(animal=paste0(year,"_",1:sum(fec)),
		dam=rep(sample(breeding_females,n_pair),fec),
		sire=rep(sample(males,n_pair),fec), 
		sex=sample(c("M","F"),sum(fec),replace=TRUE),
		cohort=year)
	pedigree <- rbind(pedigree,ped)

	## create individual present in the next year
	next_year_ind <- if(constant_pop){
		# sample(ped[,1], juv_surv*n_females)
		# sample(ped[,2], adult_surv*n_females)
		# sample(ped[,3], adult_surv*n_females)
	}else{
		rbind(
			ped[as.logical(rbinom(nrow(ped),1,juv_surv)),c(1,4)],
			cbind(animal=females[as.logical(rbinom(length(females),1,adult_surv))], sex="F"),
			cbind(animal=males[as.logical(rbinom(length(males),1,adult_surv))], sex="M")
		)
	}
  next_year_ind$age<-(year+1) - pedigree[match(next_year_ind$animal,pedigree$animal),"cohort"]
  dat[[year+1]] <- next_year_ind
}
