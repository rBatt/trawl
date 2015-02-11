
# ================
# = JAGS Options =
# ================
nChains <- 3
nIter <- 4E3
n0s <- 5E2
nThin <- 10 # max(1, floor((nIter - floor(nIter/2)) / 1000))

# =================
# = Load packages =
# =================
library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)
library(R2jags)
library(doParallel)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# ======================
# = Load MSOM Data set =
# ======================
load("./trawl/Data/MSOM/basic.dat.RData")
load("./trawl/Data/trawl2.RData") # needed so that I know where the original NA's are; not in format.trawl b/c this will need to be handled differently for the different richness models, so this is richness.basic specific


# =====================================
# = Set cores for parallel processing =
# =====================================
if(Sys.info()["sysname"]=="Windows"){
	nC <- floor(detectCores()*0.75)
	registerDoParallel(cores=nC)
}else if(Sys.info()["sysname"]=="Linux"){
	# registerDoParallel(cores=min(c(25,floor(detectCores()*0.75))))
	# registerDoParallel(floor(detectCores()*0.75))
	registerDoParallel(floor(detectCores()*0.90))
}else{
	registerDoParallel()
}


# ======================================
# = Change original data's NA's to 1's =
# ======================================
change.na.1 <- trawl2[is.na(wtcpue), list(s.reg=s.reg, year=year, stratum=stratum, K=K, spp=spp)]
setkey(basic.dat[[2]], s.reg, year)
md.num <- basic.dat[[2]][change.na.1[,list(s.reg,as.character(year))]][,as.numeric(num)]
for(i in 1:length(md.num)){
	t.c <- change.na.1[i,c(stratum=stratum,K=K,spp=spp)]
	tryCatch(
		{
			if(!is.na(basic.dat[[1]][[md.num[i]]][t.c[1],t.c[2],t.c[3]])){
				print("already fixed!")
			}else{
				basic.dat[[1]][[md.num[i]]][t.c[1],t.c[2],t.c[3]] <- 1
			}
		
		}, 
		error=function(cond){print(paste0(t.c,collapse=" "))} # sometimes the subset doesn't exist b/c that particular species from trawl2 was removed before the creation of basic.dat b/c it wasn't correctSpp, or it didn't have a common name, or is.species() was F, etc. For example, spp == "Gastropoda" or spp=="Trash species in catch" will not be included here. spp=="Ophichthus ocellatus" wasn't included because it doesn't have a common name (but see issue #26) 
	)
}


# =======================================
# = Function to remove unsampled strata =
# =======================================
prep.basic <- function(t.dat){	
	keep.strat <- apply(t.dat, 1, function(x)!all(is.na(x))) # which strata were never sampled?
	t.dat <- t.dat[keep.strat,,] # remove strata that were never sampled

	# If there are NA's 
	orig.na.TF <- apply(t.dat, c(1,3), function(x){all(is.na(x))})# sometimes there were NA's in the original data – this implies the species was observed, but perhaps effort wasn't recorded (/ by 0), or some other reason why wtcpue couldn't be recorded.
	if(any(orig.na.TF)){
		stop("A species was NA for all reps in a stratum. Please check the following: 1) replace original NA's with 1, 2) if other species have non-NA for this s.reg-year-stratum-K, then this species should have been changed to 0 in expand.data, 3) if all species are NA for all reps in this s.reg-year-stratum, then the stratum should have been dropped at the start of this function")
		# print("found one!")
		# orig.na.ind <- which(orig.na.TF, arr.ind=TRUE) # not easy to use orig.na.TF as subset b/c it doesn't reference all 3 dimensions
		# orig.na.ind.c <- c(orig.na[,1], rep(1,nrow(orig.na)), orig.na[,2]) # we'll just change the first column (K) to not-NA
		# orig.na.2.1 <- matrix(orig.na.ind.c, ncol=3, dimnames=list(rownames(orig.na),c("dim1","dim2","dim3")))
		# t.dat[orig.na.2.1] <- 1 # all non-zero positive values will be turned into 1 in the occurrence model
	}
	

	return(t.dat)

}


# ===================================
# = Prep Data for Bayesian Richness =
# ===================================
t.dat <- lapply(basic.dat[[1]], prep.basic)


# ==============================
# = Function to combine output =
# ==============================
# Need to combine lists of model output so that they are grouped by parameter/type, not by model run
# e.g., if regions 1-3 each have paremters A-D, would want output to be [[1]]A1,A2,A3 [[2]]B1,B2,B3 etc ... Not [[1]]A1,B1,C1,D1 [[2]]A2,B2,C2,D2 etc ...
# comb <- function(...){
# 	lapply(do.call(Map, c(list, list(...))), simplify2array)
# }


# =========================
# = Run Bayesian Richness =
# =========================
# Run first Bayesian richness (done separately b/c combine function in foreach needs starting output)
# first.out <- rich.basic(t.dat[[1]], nzeroes=n0s, nChains=nChains, nIter=nIter, nThin=nThin) # run the model for the first subset

# Run all other Bayesian richness in parallel
richness.basic.out <- foreach(i=(1:length(t.dat))) %dopar%{ # run all other subsets in parallel
	rich.basic(t.dat[[i]], nzeroes=n0s, nChains=nChains, nIter=nIter, nThin=nThin) # do analysis for this subset
}




# ===============
# = Save Output =
# ===============
save(richness.basic.out, file="./trawl/Results/Richness/richness.basic.out.RData")


# ======================================
# = Run and save last.out from screwup =
# ======================================
# last.out <- rich.basic(t.dat[[length(t.dat)]], nzeroes=n0s, nChains=nChains, nIter=nIter, nThin=nThin) # run the model for the first subset
# save(last.out, file="./trawl/Results/Richness/last.out.RData", compress="xz")


