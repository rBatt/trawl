
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
load("./trawl/Data/MSOM/prepd.msom.cov.RData")
# load("./trawl/Data/MSOM/cov.dat.RData")
# load("./trawl/Data/MSOM/cov.dat.prec.RData")
# load("./trawl/Data/trawl2.RData") # needed so that I know where the original NA's are


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


# =========================
# = Run Bayesian Richness =
# =========================


# Run all other Bayesian richness in parallel
richness.cov.out <- foreach(i=(1:length(prepd.dat))) %dopar%{ # run all other subsets in parallel
	rich.cov(
		data=prepd.cov.dat[[i]], 
		covs=list(prepd.cov1[[i]],prepd.cov2[[i]]), 
		cov.precs=list(prepd.cov1.prec[[i]], prepd.cov2.prec[[i]]), 
		nameID=paste(prepd.cov.names[i], collapse="_"),
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


