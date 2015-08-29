
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
	registerDoParallel(floor(detectCores()*0.8))
	# registerDoParallel(floor(detectCores()*0.90))
}else{
	registerDoParallel()
}


# ================
# = Grid Options =
# ================
# Grid Size
grid.w <- 9 # Width # 6
grid.h <- 9 # Height # 11
grid.t <- 4 # Time


# ===================
# = Species Options =
# ===================
ns <- 30 # Number of Species


# ======================
# = Simulation Options =
# ======================
n.obs.reps <- 4 # number of time to observe the same true process (each observation is analyzed separately)
n.ss <- 4 # number of substrata (for observation)
n.ss.mu <- trunc((n.ss*grid.w*grid.h)*(50/100)) #max(trunc((n.ss*grid.w*grid.h)/3*2), grid.w*grid.h) # total substrata observed
base.chance <- 1 #plogis(rnorm(ns)) #rbeta(ns,2,2) #runif(n=ns, 0.2, 0.8) # baseline detectability (before ID chance)

# Create chance to be identified if caught
# Can also be used to represent a generic
#  time-varying chance of being detected
obs.chance <- function(dim2=ns, dim1=grid.t, dim3=n.obs.reps, rand.gen=rnorm, chances, ...){
	# If chances is supplied, then each species will have the same chance in each year, and each replicate
	# species may differ tho
	
	rand.gen <- match.fun(rand.gen)
	dots <- list(...)
	# stopifnot(all.same(sapply(dots, length)))
	
	if(missing(chances)){
		
		if(length(dots)>0){
			chances <- matrix(mapply(rand.gen, MoreArgs=list(n=dim2), ...), nrow=dim2)
		}else{
			chances <- matrix(rand.gen(n=dim2,...))
		}
		
		v.rep <- function(...){rep(c(...),each=max(floor(dim1/ncol(chances)),1))}
		
		t.lvls <- unlist(apply(chances, 1, function(x)list(v.rep(x))),F,F)
		# t.noID <- aperm(simplify2array(lapply(t.lvls, roll.recycle, dim3, dim1)), c(2, 3, 1))
		# t.noID0 <- lapply(t.lvls, roll.recycle, dim3, dim1)
		# t.noID <- aperm(array(simplify2array(t.noID0), dim=c(dim3,dim1,dim2)), c(2, 3, 1))
		# t.noID
		
	}else{
		stopifnot(dim2%%length(chances)==0)
		chances <- rep(chances, each=dim2%/%length(chances))
		
		t.lvls <- lapply(chances, c)
		# t.noID <- aperm(simplify2array(lapply(t.lvls, roll.recycle, dim1, dim3)), c(1, 3, 2))
		# t.noID0 <- lapply(t.lvls, roll.recycle, dim3, dim1)
		# t.noID <- aperm(array(simplify2array(t.noID0), dim=c(dim3,dim1,dim2)), c(2, 3, 1))
		# t.noID
	}
	
	t.noID0 <- lapply(t.lvls, roll.recycle, dim3, dim1)
	t.noID <- aperm(array(simplify2array(t.noID0), dim=c(dim3,dim1,dim2)), c(2, 3, 1))
	t.noID	
}

# obs.chance 

# the main use of the function will be when chances is not supplied, 
# and the function performs the random number generation
# In a given year, all species will be drawn from the same distribution
# That distribution can change among years by supplying vectors to ... . Between replicates, 
# the order of which years correspond to which distribution changes

# dim2 is number of species; chances/ rng's always differ across dim2 (unless chances supplied with 1 unique)
# dim1 is time steps; chances will only differ across dim1 if length of arguments in ... are > 1
# dim3 is replicates; chances will be same across dim3, but dim1 shift by 1 index between each dim3
# rand.gen is a function for random number generation; its first argument must be \code{n}. Used to generate chances when chances is not specified.
# chances are predetermined probabilities that can optionally be provided for each species. length(chances) must be a multiple of dim2. If chances is supplied, rand.gen will not be used.
# ... arguments to be passed to rand.gen

# ===========================
# = Examples for obs.chance =
# ===========================

# species (dim2) are drawn from same distribution, but each is different
# no changes between years (dim1), thus no changes between replicates(dim3)
# plogis(obs.chance(dim2=6, dim3=2, dim1=7))

t.noID.mus <- c(-2,0,2,4)
t.noID.sd <- 2
t.noID <- plogis(obs.chance(dim2=ns, dim1=grid.t, dim3=n.obs.reps, mean=t.noID.mus, sd=t.noID.sd))


# ================
# = MSOM Options =
# ================
nChains <- 3
nIter <- 5E4
n0s <- 10
nSamples <- 500
nThin <- ((nIter/2)*nChains)/nSamples #40 # max(1, floor((nIter - floor(nIter/2)) / 1000))


# =================================
# = Do Simulation of True Process =
# =================================
# Simulate environment
# env <- sim.env(grid.w=grid.w, grid.h=grid.h, grid.t=grid.t, X.slope=0.75*(12/grid.t))
X.slope <- 0
env <- sim.env(grid.w=grid.w, grid.h=grid.h, grid.t=grid.t, X.slope=X.slope)

# Simulate Species
out <- sim.spp.proc(env, ns=ns, dynamic=FALSE)

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
		out.obs <- obs.spp(out, n.ss, n.ss.mu, base.chance=rep(1,ns), t.noID[,,i])
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
		big.out.obs[[i]] <- obs.spp(out, n.ss, n.ss.mu, base.chance=rep(1,ns), t.noID[,,i])
		new.simDat <- spp2msom(big.out.obs[[i]])$simDat
		names(new.simDat) <- paste(names(new.simDat),i, sep=".")
		big.simDat <- c(big.simDat, new.simDat)
	}
}


# =============================
# = Analyze simData with MSOM =
# =============================
# Run all other Bayesian richness in parallel
msom.start <- proc.time()
year <- as.numeric(gsub("year([0-9]{1,2})\\.[0-9]{1,2}","\\1",names(big.simDat)))
n.loops <- length(big.simDat)
sim.rich.cov <- foreach(i=(1:n.loops)) %dopar%{ # run all other subsets in parallel
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
msom.end <- proc.time()

# ===============================================================
# = Some Statistics about the Computational Demands of this Run =
# ===============================================================
msom.elapsed <- data.frame(t((msom.end - msom.start)[1:3]))

runtimeStats <- data.frame(
	datetime=Sys.time(), 
	msom.elapsed, 
	sim.rich.cov.size=as.numeric(object.size(sim.rich.cov)), 
	ns=ns, n0s=n0s, grid.w=grid.w, grid.h=grid.h, grid.t=grid.t, reps=n.obs.reps, n.ss=n.ss,
	nChains=nChains, nIter=nIter, nSamples=nSamples, 
	n.foreach.loops = n.loops,
	n.params.tracked=length(sim.rich.cov[[1]][[1]]), 
	n.nodes.tracked=sum(sapply(sim.rich.cov[[1]][[1]], function(x)prod(dim(x)))), 
	nCores=min(getDoParWorkers(), n.loops)
)

runtimeStats.file <- "./trawl/Scripts/Simulation/sim.basic.meta.runtimeStats.txt"
if(file.exists(runtimeStats.file)){
	write.table(runtimeStats, runtimeStats.file, append=TRUE, row.names=F, col.names=FALSE)
	
}else{
	write.table(runtimeStats, runtimeStats.file, append=F, col.names=TRUE, row.names=F)
	
}
# read.table(runtimeStats.file, header=TRUE, row.names=NULL)


# ===============
# = Save Output =
# ===============
saveFile_a <- "./trawl/Results/Simulation/sim.rich.cov.RData"
saveFile_b <- "./trawl/Results/Simulation/sim.basic.RData"
# saveFile_c <- "./trawl/Results/Simulation/sim.basic.small.RData"

save(sim.rich.cov, file=saveFile_a)
file.copy(from=saveFile_a, to=renameNow(saveFile_a), copy.date=TRUE)

save.image(file=saveFile_b)
file.copy(from=saveFile_b, to=renameNow(saveFile_b), copy.date=TRUE)

# sim.rich.cov <- lapply(sim.rich.cov, function(x)x$mean)
# save.image(file=saveFile_c)
# file.copy(from=saveFile_c, to=renameNow(saveFile_c), copy.date=TRUE)


# =================================
# = Run this script on amphiprion =
# =================================
# # Make sure the
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
# pull("./Documents/School&Work/pinskyPost/trawl/", "ryanb@amphiprion.deenr.rutgers.edu")
#
# setwd(oldwd)

