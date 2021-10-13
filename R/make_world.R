#make_world

## what do we output - how do we incorporate alive individuals that don't breed or multiple measurements
## output pedigree and data structure?

make_wild_str <- function(
	years = 5,
	n_females = 50,
	P_breed = 1,
	fecundity = 4, #number of oocytes,
	# 
	polgamy_rate = 0,
	polgyny_rate = 0,
	P_fert = 1, # probably that oocytes are fertilised,
	juv_surv = 0.25,
	adult_surv = 0.5,
	migration = 0,
	constant_pop = FALSE ,
	known_age_structure = FALSE){

options(stringsAsFactors=FALSE)

	det_growth_rate <- (juv_surv * fecundity * P_fert)/2 + adult_surv + migration 

	v_as <- adult_surv * (1-adult_surv)
	v_js <- juv_surv * (1-juv_surv)
	v_fec <- fecundity
	v_rec <- fecundity^2*v_js + juv_surv^2*v_fec + v_js*v_fec

	stoch_growth_rate <- det_growth_rate - (v_as + v_rec)/(2*n_females)

	migration <- 1- stoch_growth_rate


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
		n_juv <- rpois(n_pair,fecundity*P_fert)

		ped <- data.frame(animal=paste0(year,"_",1:sum(n_juv)),
			dam=rep(sample(breeding_females,n_pair),n_juv),
			sire=rep(sample(males,n_pair),n_juv), 
			sex=sample(c("M","F"),sum(n_juv),replace=TRUE),
			cohort=year)
		pedigree <- rbind(pedigree,ped)

		## create individuals present in the next year
		next_year_ind <- if(constant_pop){
			rbind(
				ped[sample(1:nrow(ped), juv_surv*fecundity*n_females, replace=TRUE),c(1,4)],
				cbind(animal=sample(females, adult_surv*n_females, replace=TRUE), sex="F"),
				cbind(animal=sample(males, adult_surv*n_females, replace=TRUE), sex="M")
			)
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

	return(list(pedigree=pedigree,data_str=do.call(rbind,dat)))

}
