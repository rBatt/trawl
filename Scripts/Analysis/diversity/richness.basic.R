

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
load("./trawl/Data/msom.dat.RData")


# =====================================
# = Set cores for parallel processing =
# =====================================
if(Sys.info()["sysname"]=="Windows"){
	nC <- floor(detectCores()*0.75)
	registerDoParallel(cores=nC)
}else if(Sys.info()["sysname"]=="Linux"){
	registerDoParallel(cores=min(c(25,floor(detectCores()*0.75))))
}else{
	registerDoParallel()
}


# ==============================
# = Function to combine output =
# ==============================
# Need to combine lists of model output so that they are grouped by parameter/type, not by model run
# e.g., if regions 1-3 each have paremters A-D, would want output to be [[1]]A1,A2,A3 [[2]]B1,B2,B3 etc ... Not [[1]]A1,B1,C1,D1 [[2]]A2,B2,C2,D2 etc ...
comb <- function(...){
	lapply(do.call(Map, c(list, list(...))), simplify2array)
}


# =========================
# = Run Bayesian Richness =
# =========================
t.dat <- msom.dat[[1]][[1]] # subset to data for first run
keep.strat <- apply(t.dat, 1, function(x)!all(is.na(x))) # which strata were never sampled?
t.dat <- t.dat[keep.strat,,] # remove strata that were never sampled
first.out <- rich.basic(t.dat, nzeroes=20, nChains=2, nIter=5E2) # run the model for the first subset

richness.basic.out <- foreach(i=(2:(length(msom.dat[[1]])-1)), .combine=comb, .init=first.out, .multicombine=TRUE) %dopar%{ # run all other subsets in parallel
	t.dat <- msom.dat[[1]][[i]] # subset to current iteration
	keep.strat <- apply(t.dat, 1, function(x)!all(is.na(x))) # figure out which strata to toss out
	t.dat <- t.dat[keep.strat,,] # only keep observed strata
	rich.basic(t.dat, nzeroes=20, nChains=2, nIter=1E2) # do analysis for this subset
}


# testing[[10]][[1]] # is the normal bugs output for the first run
# testing[[10]][[2]] # is the normal bugs output for the second run
# testing[[10]][[3]] # is the normal bugs output for the third run




