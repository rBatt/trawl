
# =================
# = Load Packages =
# =================
library(raster)
library(fields)
library(igraph)
library(R2jags)
library(data.table)


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
# Number of Species
ns <- 2E1


# =================================
# = Do Simulation of True Process =
# =================================
# Simulate environment
env <- sim.env(grid.w=grid.w, grid.h=grid.h, grid.t=grid.t, X.slope=0.75)

# Simulate Species
out <- sim.spp.proc(env, ns=ns, c(0.7,0.7))

# name output attributes for easy access
spp.bio <- attr(out, "spp.bio")
grid.X <- attr(out, "grid.X")
S.dens.X <- attr(out, "spp.densX")
dims <- attr(out, "dims")

# Get S, a list of bricks of length grid.t specifying species presence
S <- getS(out)



# ================================
# = Simulate Observation of True =
# ================================
out.obs <- obs.spp(out)


# ==========================================
# = Format Simulation for Analysis by MSOM =
# ==========================================
n.substrat <- 9
n.strat <- nrow(out.obs)
ns <- attr(out.obs, "dims")["ns"]
grid.t <- attr(out.obs, "dims")["grid.t"]
# 1) Label and sort rows of obs.arr according to stratum-substratum organization
	# Indicate which rows belong to which strata
	stratID0 <- 1:n.strat # stratum ID's (order would fill a matrix b)
	stratID <- c(matrix(stratID0, nrow=attr(out.obs, "dims")[1], byrow=TRUE)) # reorders to raster convention
	keyK.rast0 <- gen.grid(stratID, c(attr(out.obs, "dims")[1:2],1), byrow=F) # create a grid
	keyK.rast <- values(disaggregate(keyK.rast0, fact=sqrt(n.substrat), method=""))
	
	# Sort (rows of obs.arr) by stratum ID; in way that is convenient for filling output array
	obs.arr.sort <- attr(out.obs, "obs.arr")[order(keyK.rast),,]
	
# 2) Exploit reorganization of obs.arr to fill output array
	# Create output array (which is input array for MSOM analysis)
	# Dimension 1 of sorted obs.arr (location) fills dims 1&2 out output array (stratum, K)
	# Dimension 2 of sorted obs.arr (spp) fills dim 3 of output array (spp)
	# Dimension 3 of sorted obs.arr (year) corresponds to each element of list of output arrays
	fill.dims <- c(n.substrat, n.strat, ns) # can't fill byrow=T, so flip row/col and aperm() later
	a.o0 <- function(x){list(array(c(x), dim=fill.dims))} # function to fill/make arrays
	output.array0 <- unlist(apply(obs.arr.sort, 3, a.o0), F, F) # split 1st dim into 2, and make 3rd dim a list
	output.array <- lapply(output.array0, aperm, c(2,1,3)) # rearrange dim to row=stratum & col=K (unflip)
	
	# Checking
	# plot(subset(attr(out.obs, "obs")[[12]], 10)) # plot critter 10 in year 12
	# attr(out.obs, "obs.arr")[1:50,10,12]; obs.arr.sort[1:50,10,12] # first 3+ strata of critter 10 in year 12
	# output.array[[12]][1:4,,10] # first 4 strata of critter 10 in year 12

# 3) Name dimensions (dim) and indices within each dimension (dimnames)
	# list elements are years
	names(output.array) <- paste0("year",1:grid.t)

	# dim should have names c("stratum", "K", "spp")
	strat.names <- as.character(stratID0) # Dimension 1 ("stratum") has integer names
	K.names <- as.character(1:n.substrat) # Dimension 2 ("K") has integer names
	spp.names <- paste0("critter",1:ns) # Dimension 3 ("spp") has contrived character names (e.g., "critter1")
	name.arrays.in.list <- function(x){ # function to apply names within list
		dimnames(x) <- list("stratum"=c(strat.names), "K"=c(K.names), "spp"=c(spp.names))
		x
	}
	simDat <- lapply(output.array, name.arrays.in.list) # give names via lapplying naming function
	simCov <- unlist(apply(values(attr(out.obs, "grid.X")), 2, list),F,F)
	simCov.NA <- lapply(simCov, function(x){x[] <- NA; x})
	simCov.precs <- lapply(simCov, function(x){x[] <- 1E6; x})
	simCov.precs.bad <- lapply(simCov, function(x){x[] <- 1E-6; x})
	sim.cov.names <- data.frame(s.reg="sim",year=1:grid.t, num=1:length(simDat))
	
	
	


# =============================
# = Analyze simData with MSOM =
# =============================
# save.out.dir="trawl/Results/Richness/msomCov/msomCov.smry/"
# save.fit.cov.dir="trawl/Results/Richness/msomCov/msomCov.full/"

nChains <- 3
nIter <- 4E3
n0s <- 1E2
nThin <- 60 # max(1, floor((nIter - floor(nIter/2)) / 1000))

test <- rich.cov(
	data=simDat[[1]], 
	covs=list(simCov.NA[[1]],simCov[[1]]), 
	cov.precs=list(simCov.precs.bad[[1]],simCov.precs[[1]]), 
	nameID=paste(sim.cov.names[1,], collapse="_"),
	nzeroes=n0s, 
	nChains=nChains, 
	nIter=nIter, 
	nThin=nThin,
	save.out.dir="trawl/Results/Simulation",
	save.fit.cov.dir="trawl/Results/Simulation"
) # do analysis for this subset





# ==========
# = Graphs =
# ==========
# ===========================
# = Temperature Time Series =
# ===========================
# ---- Graph_Temp_Space_Time ----
# Plot temperature time series
temp.range <- range(values(grid.X))
smplt <- c(0.89,0.95, 0.2,0.8)
bgplt <- c(0.15,0.85,0.12,0.95)
axargs <- list(mgp=c(0.75,0.25,0), tcl=-0.15)
dev.new()
plot(env, zlim=c(temp.range[1], temp.range[2]), col=tim.colors(), smallplot=smplt, bigplot=bgplt, axis.args=axargs, nc=5,mgp=c(0.75,0.25,0), tcl=-0.15)


# =======================================
# = Species/ Community Richness Density =
# =======================================
# Plot Temperature Distributions for the Whole Community
store <- c()
sdt2 <- lapply(S.dens.X, FUN=function(x){x$y <- x$y/max(x$y); x})
for(d in 1:512){
	ti <- c()
	for(i in 1:length(sdt2)){
		ti[i] <- sdt2[[i]]$y[d]
	}
	store[d] <- mean(ti)
}
# dev.new(width=3.5, height=3.5)
# par(mar=c(2.15,2.15,0.1,0.1), ps=10, mgp=c(1.15,0.2,0), tcl=-0.2)
# plot(S.dens.X[[1]]$x, store, type="l", xlab="Temperature", ylab="Density (community average)")

dev.new(width=5.5, height=4.5)
par(mar=c(2.15,2.15,0.1,0.1), ps=10, mgp=c(1.15,0.2,0), tcl=-0.2)
plot(sdt2[[1]]$x, sdt2[[1]]$y, type="l", ylim=c(0,1), xlab="Environmental Variable", ylab="Relative Density", lwd=0.1)
for(i in 2:ns){
	lines(sdt2[[i]]$x, sdt2[[i]]$y, type="l", lwd=0.1)
}
lines(S.dens.X[[1]]$x, store, type="l", xlab="Temperature", ylab="Density (community average)", lwd=3, col="red")

# weighted.mean(S.dens.temps[[1]]$x, w=store)
# sum(store[S.dens.temps[[1]]$x>0])
# sum(store[S.dens.temps[[1]]$x<0])

# ===============================================
# = Initial Biomass and Presence of All Species =
# ===============================================
# Plot Initial Biomass of Each Species
spp.1 <- setValues(S[[1]], spp.bio[,,1]) # cell ordering is for raster
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.5,0.15,0))
dev.new(width=12, height=5)
plot(spp.1, maxnl=200, col=tim.colors(), zlim=range(values(spp.1), na.rm=TRUE),smallplot=smplt, bigplot=bgplt, axis.args=axargs, nr=8, nc=25, legend=FALSE, colNA="darkgray")


# =============================================
# = Final Biomass and Presence of All Species =
# =============================================
# Plot Initial Biomass of Each Species
spp.final <- setValues(S[[1]], spp.bio[,,grid.t]) # cell ordering is for raster
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.5,0.15,0))
dev.new(width=12, height=5)
plot(spp.final, maxnl=200, col=tim.colors(), zlim=range(values(spp.final), na.rm=TRUE),smallplot=smplt, bigplot=bgplt, axis.args=axargs, nr=8, nc=25, legend=FALSE, colNA="darkgray")


# =====================================
# = True Richness over Time and Space =
# =====================================
space.rich <- grid.X
for(i in 1:grid.t){
	t.bio <- spp.bio[,,i]
	t.rich <- apply(t.bio, 1, function(x, ...)sum(!is.na(x)))
	values(space.rich[[i]]) <- t.rich
	
}

smplt <- c(0.85,0.88, 0.2,0.8)
bgplt <- c(0.05,0.82,0.15,0.95)
axargs <- list(mgp=c(0.5,0.15,0))

zlim <- range(values(space.rich), na.rm=TRUE)
ylim <- c(1.085,0)
dev.new()
par(mfrow=auto.mfrow(grid.t))

mfg.mat <- matrix(1:grid.t, nrow=auto.mfrow(grid.t)[1], byrow=TRUE)
for(i in 1:grid.t){
	
	t.mfg <- which(mfg.mat==i, T)
	par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(t.mfg[1],t.mfg[2]))
	tp <- t(matrix(values(subset(space.rich, i)), nrow=grid.h, byrow=TRUE))
	image.plot(tp, zlim=zlim, ylim=ylim, axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
}

# plot(space.rich, col=tim.colors(), zlim=range(values(space.rich)), maxnl=50)


# ===========================
# = True Richness Over Time =
# ===========================
true.rich <- c()
for(i in 1:grid.t){
	t.pres <- out[,,i]
	true.rich[i] <- sum(apply(t.pres, 2, function(x, ...)any(!is.na(x))))
}
dev.new(width=3.5, height=3.5)
par(mar=c(2, 2, 0.1, 0.1), mgp=c(1.1, 0.15, 0), tcl=-0.15, ps=10)
plot(1:grid.t, true.rich, type="l", xlab="Time", ylab="Total Grid Richness")


# ===================================
# = Richness Accumulation over Time =
# ===================================
dev.new(width=3.5, height=3.5)
par(mar=c(1.75, 1.75, 0.1, 0.1), mgp=c(0.85, 0.15, 0), tcl=-0.15, ps=10)

spp.sample <- function(x, n){
	sub <- matrix(x[sample(nrow(x), n),], ncol=ncol(x))
	sum(apply(sub, 2, function(x)any(!is.na(x))))
}


rich.cols <- tim.colors(n=grid.t)

n.max <- min(dim(out)[1],200)
n.iter <- 5
min.effort <- 1


for(j in 1:grid.t){
	spp.t.m <- matrix(out[,,j], nrow=dim(out)[1], ncol=dims["ns"]) # cell ordering is for raster

	rich <- matrix(NA, nrow=n.max, ncol=n.iter)
	for(k in 1:n.iter){
		for(i in min.effort:n.max){
			rich[i,k] <- spp.sample(spp.t.m, n=i)
		}
	}
	
	rich.mu <- rowMeans(rich)
	
	if(j==1){
		plot(1:n.max, rich.mu, col=rich.cols[j], ylab="Avg Richness", xlab="# Cells Sampled", type="l", ylim=c(00,max(true.rich)))
	}else{
		lines(1:n.max, rich.mu, col=rich.cols[j])
	}
	
	
}


# =============================================
# = Results of Simulation over Time and Space =
# =============================================
# Plot up transition results
smplt <- c(0.85,0.88, 0.2,0.8)
bgplt <- c(0.05,0.82,0.15,0.95)
axargs <- list(mgp=c(0.5,0.15,0))

dev.new(width=5, height=2)

n.steps <- 6
spp.id <- sample(1:ns, 1)

par(mfrow=c(2, n.steps), oma=c(0.5,1,0,0))
year.index <- seq(1, grid.t, length.out=n.steps)
for(i in 1:n.steps){
	t.y <- year.index[i]
	
	par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(1,i))
	image.plot(t(matrix(values(subset(grid.X, t.y)), nrow=grid.h, byrow=TRUE)), zlim=range(values(grid.X), na.rm=TRUE), ylim=c(1.085,0), axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
	
	par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(2,i))
	image.plot(t(matrix(spp.bio[,spp.id,t.y], nrow=grid.h, byrow=TRUE)), zlim=range(spp.bio[,spp.id,], na.rm=TRUE), ylim=c(1.085,0), bg="gray", axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
}

