
# ================
# = JAGS Options =
# ================
nChains <- 3
nIter <- 4E3
n0s <- 5E2
nThin <- 50 # max(1, floor((nIter - floor(nIter/2)) / 1000))

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
load("./trawl/Data/MSOM/cov.dat.RData")
load("./trawl/Data/MSOM/cov.dat.prec.RData")
load("./trawl/Data/trawl2.RData") # needed so that I know where the original NA's are


# =====================================
# = Set cores for parallel processing =
# =====================================
if(Sys.info()["sysname"]=="Windows"){
	nC <- floor(detectCores()*0.75)
	registerDoParallel(cores=nC)
}else if(Sys.info()["sysname"]=="Linux"){
	# registerDoParallel(cores=min(c(25,floor(detectCores()*0.75))))
	registerDoParallel(floor(detectCores()*0.60))
	# registerDoParallel(floor(detectCores()*0.90))
}else{
	registerDoParallel()
}


# ======================================
# = Change original data's NA's to 1's =
# ======================================
change.na.1 <- trawl2[is.na(wtcpue), list(s.reg=s.reg, year=year, stratum=stratum, K=K, spp=spp)]
setkey(cov.dat[[2]], s.reg, year)
md.num <- cov.dat[[2]][change.na.1[,list(s.reg,as.character(year))]][,as.numeric(num)]
for(i in 1:length(md.num)){
	t.c <- change.na.1[i,c(stratum=stratum,K=K,spp=spp)]
	tryCatch(
		{
			if(!is.na(cov.dat[[1]][[md.num[i]]][t.c[1],t.c[2],t.c[3]])){
				print("already fixed!")
			}else{
				cov.dat[[1]][[md.num[i]]][t.c[1],t.c[2],t.c[3]] <- 1
			}
		
		}, 
		error=function(cond){print(paste0(t.c,collapse=" "))} # sometimes the subset doesn't exist b/c that particular species from trawl2 was removed before the creation of cov.dat b/c it wasn't correctSpp, or it didn't have a common name, or is.species() was F, etc. For example, spp == "Gastropoda" or spp=="Trash species in catch" will not be included here. spp=="Ophichthus ocellatus" wasn't included because it doesn't have a common name (but see issue #26) 
	)
}


# =======================================
# = Function to remove unsampled strata =
# =======================================
prep.cov <- function(pc.dat, pc.cov1, pc.cov2, pc.cov1.prec, pc.cov2.prec){	
	keep.strat <- apply(pc.dat, 1, function(x)!all(is.na(x))) # which strata were never sampled?
	pc.dat2 <- pc.dat[keep.strat,,] # remove strata that were never sampled
	
	keep.strat.cov1 <- apply(pc.cov1, 1, function(x)!all(is.na(x))) # which strata don't have covariate 1?
	keep.strat.cov2 <- apply(pc.cov2, 1, function(x)!all(is.na(x))) # which strata don't have covariate 2?
	
	# sum(keep.strat)
	# sum(keep.strat.cov1)
	# sum(keep.strat.cov2)
	
	if(!all(keep.strat == keep.strat.cov1)){
		toFill <- is.na(pc.cov1) & !is.na(pc.dat)
		pc.cov1[toFill] <- mean(pc.cov1[pc.cov1.prec>=1E4], na.rm=TRUE) # the mean of the covariate values (subset to the values that weren't previously filled in!)
		pc.cov1.prec[toFill] <- pmax(1/1E4, 1/var(pc.cov1[!toFill & pc.cov1.prec>=1E4], na.rm=TRUE)) # precision as 1/variance, or if variance undefined, 1/1E4
		# any(is.na(pc.cov1.prec) & !is.na(pc.cov1))
		# any(is.na(pc.cov1.prec) & !is.na(pc.dat))
		# any(is.na(pc.cov1) & !is.na(pc.dat))
		
	}
	pc.cov1.2 <- apply(pc.cov1[keep.strat.cov1,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
	pc.cov1.prec.2 <- apply(pc.cov1.prec[keep.strat.cov1,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
	
	
	if(!all(keep.strat == keep.strat.cov2)){
		toFill <- is.na(pc.cov2) & !is.na(pc.dat)
		pc.cov2[toFill] <- mean(pc.cov1[pc.cov2.prec>=1E4], na.rm=TRUE) # the mean of the covariate values (subset to the values that weren't previously filled in!)
		pc.cov2.prec[toFill] <- pmax(1/1E4, 1/var(pc.cov2[!toFill & pc.cov1.prec>=1E4], na.rm=TRUE)) # precision as 1/variance, or if variance undefined, 1/1E4
	}
	pc.cov2.2 <- apply(pc.cov2[keep.strat.cov2,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
	pc.cov2.prec.2 <- apply(pc.cov2.prec[keep.strat.cov2,,], c(1,3), mean, na.rm=TRUE) # remove strata that were never sampled
	
	
		

	# If there are NA's 
	orig.na.TF <- apply(pc.dat2, c(1,3), function(x){all(is.na(x))}) # NA's can indicate presence but unknown biomass
	if(any(orig.na.TF)){
		stop("A species was NA for all reps in a stratum.")
	}
	

	return(list(pc.dat2, pc.cov1.2, pc.cov2.2, pc.cov1.prec.2, pc.cov2.prec.2))

}


# ===================================
# = Prep Data for Bayesian Richness =
# ===================================
# t.dat, t.cov1, and t.cov2 should all be the same length
prepd.cov.dat <- vector("list", length(cov.dat[[1]]))
prepd.cov1 <- vector("list", length(cov.dat[[3]][[1]]))
prepd.cov2 <- vector("list", length(cov.dat[[3]][[2]]))
prepd.cov1.prec <- vector("list", length(cov.dat.prec[[1]]))
prepd.cov2.prec <- vector("list", length(cov.dat.prec[[2]]))


for(i in 1:length(cov.dat[[1]])){
	t.prep <- prep.cov(
		pc.dat=cov.dat[[1]][[i]],
		pc.cov1=cov.dat[[3]][[1]][[i]], 
		pc.cov2=cov.dat[[3]][[2]][[i]],
		pc.cov1.prec=cov.dat.prec[[1]][[i]],
		pc.cov2.prec=cov.dat.prec[[2]][[i]]
	)
	
	prepd.cov.dat[[i]] <- t.prep[[1]]
	prepd.cov1[[i]] <- t.prep[[2]]
	prepd.cov2[[i]] <- t.prep[[3]]
	prepd.cov1.prec[[i]] <- t.prep[[4]]
	prepd.cov2.prec[[i]] <- t.prep[[5]]
	
}

save(prepd.cov.dat, prepd.cov1, prepd.cov2, prepd.cov1.prec, prepd.cov2.prec, file="./trawl/Data/MSOM/prepd.msom.cov.RData")


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
# first.out <- rich.cov(t.dat[[1]], nzeroes=n0s, nChains=nChains, nIter=nIter, nThin=nThin) # run the model for the first subset

# Run all other Bayesian richness in parallel
richness.cov.out <- foreach(i=(1:length(prepd.dat))) %dopar%{ # run all other subsets in parallel
	rich.cov(
		data=prepd.cov.dat[[i]], 
		covs=list(prepd.cov1[[i]],prepd.cov2[[i]]), 
		cov.precs=list(prepd.cov1.prec[[i]], prepd.cov2.prec[[i]]), 
		nzeroes=n0s, 
		nChains=nChains, 
		nIter=nIter, 
		nThin=nThin
	) # do analysis for this subset
}




# ===============
# = Save Output =
# ===============
save(richness.cov.out, file="./trawl/Results/Richness/richness.cov.out.RData")


# ======================================
# = Run and save last.out from screwup =
# ======================================
# last.out <- rich.cov(t.dat[[length(t.dat)]], nzeroes=n0s, nChains=nChains, nIter=nIter, nThin=nThin) # run the model for the first subset
# save(last.out, file="./trawl/Results/Richness/last.out.RData", compress="xz")


