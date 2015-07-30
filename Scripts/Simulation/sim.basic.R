
# =================
# = Load Packages =
# =================
# Data structure
library(raster)
library(data.table)

# Graphing
library(fields)

# Statistics
library(igraph)
library(R2jags)

# Computing
library(parallel)
library(doParallel)
library(foreach)

# Other
library(rbLib) # library(devtools); install_github("rBatt/rbLib")


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ======================
# = Load Sim Functions =
# ======================
sim.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/SimFunctions"
invisible(sapply(paste(sim.location, list.files(sim.location), sep="/"), source, .GlobalEnv))

data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# =====================================
# = Set cores for parallel processing =
# =====================================
if(Sys.info()["sysname"]=="Windows"){
	nC <- floor(detectCores()*0.75)
	registerDoParallel(cores=nC)
}else if(Sys.info()["sysname"]=="Linux"){
	# registerDoParallel(cores=min(c(25,floor(detectCores()*0.75))))
	registerDoParallel(floor(detectCores()*0.50))
	# registerDoParallel(floor(detectCores()*0.90))
}else{
	registerDoParallel()
}


# ================
# = Grid Options =
# ================
# Grid Size
grid.w <- 5 # Width
grid.h <- 7 # Height
grid.t <- 12 # Time


# ===================
# = Species Options =
# ===================
ns <- 100 # Number of Species


# ======================
# = Simulation Options =
# ======================
n.obs.reps <- 10 # number of time to observe the same true process (each observation is analyzed separately)
n.ss <- 9 # number of substrata (for observation)
n.ss.mu <- max(trunc((n.ss*grid.w*grid.h)/3), grid.w*grid.h) # total substrata observed
n.noID <- ns/2 # number of species to not be ID'd in first half of time series
base.chance <- runif(n=ns, 0.2, 0.8) # baseline detectability (before ID chance)
t.noID <- list()
for(i in 1:grid.t){
	if(i%%2 == 0){
		r1 <- floor(grid.t/2)
		r2 <- ceiling(grid.t/2)
		t.t.noID <- c(rep(1, r1), rep(0, r2))
	}else{
		r1 <- floor(grid.t/2)
		r2 <- ceiling(grid.t/2)
		t.t.noID <- c(rep(0, r1), rep(1, r2))
	}
	t.noID[[i]] <- t.t.noID
}



# ================
# = MSOM Options =
# ================
nChains <- 3
nIter <- 4E3
n0s <- 100
nThin <- 60 # max(1, floor((nIter - floor(nIter/2)) / 1000))


# =================================
# = Do Simulation of True Process =
# =================================
# Simulate environment
env <- sim.env(grid.w=grid.w, grid.h=grid.h, grid.t=grid.t, X.slope=0.75)

# Simulate Species
out <- sim.spp.proc(env, ns=ns, niche.bias=c(0.7,0.7))

# name output attributes for easy access
spp.bio <- attr(out, "spp.bio")
grid.X <- attr(out, "grid.X")
S.dens.X <- attr(out, "spp.densX")
dims <- attr(out, "dims")

# Get S, a list of bricks of length grid.t specifying species presence
S <- getS(out)


# =========================================
# = Observation Level and Format for MSOM =
# =========================================
# The loop is for re-observing the same true process multiple times
for(i in 1:n.obs.reps){
	if(i==1){
		out.obs <- obs.spp(out, n.ss, n.ss.mu, n.noID, base.chance, t.noID[[i]])
		formatted <- spp2msom(out.obs)
		new.simDat <- formatted$simDat 
		simCov <- formatted$simCov 
		simCov.NA <- formatted$simCov.NA 
		simCov.precs <- formatted$simCov.precs 
		simCov.precs.bad <- formatted$simCov.precs.bad 
		sim.cov.names <- formatted$sim.cov.names
		
		big.out.obs <- list(out.obs)
		
		names(new.simDat) <- paste(names(new.simDat),i, sep=".")
		big.simDat <- new.simDat
	}else{
		big.out.obs[[i]] <- obs.spp(out, n.ss, n.ss.mu, n.noID, base.chance, t.noID[[i]])
		new.simDat <- spp2msom(big.out.obs[[i]])$simDat
		names(new.simDat) <- paste(names(new.simDat),i, sep=".")
		big.simDat <- c(big.simDat, new.simDat)
	}
}


# =============================
# = Analyze simData with MSOM =
# =============================
# Run all other Bayesian richness in parallel
year <- as.numeric(gsub("year([0-9]{1,2})\\.[0-9]{1,2}","\\1",names(big.simDat)))
sim.rich.cov <- foreach(i=(1:length(big.simDat))) %dopar%{ # run all other subsets in parallel
	yi <- year[i]
	rich.cov(
		data=big.simDat[[i]], 
		covs=list(simCov.NA[[yi]],simCov[[yi]]), 
		cov.precs=list(simCov.precs.bad[[yi]], simCov.precs[[yi]]), 
		nameID=paste(sim.cov.names[yi,], collapse="_"),
		nzeroes=n0s, 
		nChains=nChains, 
		nIter=nIter, 
		nThin=nThin,
		save.out.dir="trawl/Results/Simulation/",
		save.fit.cov.dir="trawl/Results/Simulation/",
		Save=FALSE
	) # do analysis for this subset
}


# ===============
# = Save Output =
# ===============
save(sim.rich.cov, file="./trawl/Results/Simulation/sim.rich.cov.RData")
save.image(file="./trawl/Results/Simulation/sim.basic.RData")


# =================================
# = Run this script on amphiprion =
# =================================
# Make sure the 
# library(rbLib)
# oldwd <- getwd()
# setwd("~")
#
# # Mirror
# from <- "Documents/School&Work/pinskyPost/trawl/"
# to <- "ryanb@amphiprion.deenr.rutgers.edu:'Documents/School&Work/pinskyPost/trawl/'"
# mirror(from, to)
#
# # Push, Run, Pull
# path <- "./Documents/School&Work/pinskyPost/trawl/Scripts/Simulation/"
# scriptName <- "sim.basic.R"
# remoteName <- "ryanb@amphiprion.deenr.rutgers.edu"
# prp(path, scriptName, remoteName, verbose=TRUE)
#
# # Pull whole trawl
# pull("./Documents/School&Work/pinskyPost/trawl/", remoteName)
#
# setwd(oldwd)
