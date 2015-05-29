
# =================
# = Load Packages =
# =================
library(raster)
library(fields)
library(igraph)


# ======================
# = Load Sim Functions =
# ======================
sim.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/SimFunctions"
invisible(sapply(paste(sim.location, list.files(sim.location), sep="/"), source, .GlobalEnv))


# =============
# = Grid Size =
# =============
# Grid Size
grid.w <- 5 # Width
grid.h <- 7 # Height
grid.t <- 12 # Time

Nv <- (grid.w*grid.h) # Number of vertices if the Grid is a Graph (number of cells in 1 year of the grid)



# ===================
# = Grid Attributes =
# ===================
# Grid Lat
# Grid Lon
# Grid depth

# Grid temperature
temp.h <- seq(-10, 0, length.out=grid.h)
temp <- c()
for(i in 1:grid.w){
	temp <- c(temp, temp.h+rnorm(grid.h))
}


# =====================
# = Create Blank Grid =
# =====================
# Create Blank Grid
grid.blank <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)


# ========================================
# = Create Temperature Grid/ Time Series =
# ========================================
# Insert first year of temperature grid
grid.temp.1 <- gen.grid(temp, dims=c(grid.h, grid.w, 1), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

# Define temperature change in ºC per year
temp.slope <- 0.75

# Create time series of temperature grid
grid.temp <- grid.blank
grid.temp <- setValues(grid.temp, values(grid.temp.1), layer=1)

rg <- function(){ # function to add noise to the temperature process (kinda weird, b/c I apply the noise at each time step)
	rnorm(n=Nv, sd=0.1)
}
for(i in 2:grid.t){
	grid.temp <- setValues(grid.temp, values(subset(grid.temp, i-1))+rg()+temp.slope, layer=i)
}

# Plot temperature time series
temp.range <- range(values(grid.temp))
smplt <- c(0.89,0.95, 0.2,0.8)
bgplt <- c(0.15,0.85,0.12,0.95)
axargs <- list(mgp=c(0.75,0.25,0), tcl=-0.15)
plot(grid.temp, zlim=c(temp.range[1], temp.range[2]), col=tim.colors(), smallplot=smplt, bigplot=bgplt, axis.args=axargs, nc=5,mgp=c(0.75,0.25,0), tcl=-0.15)


# ==================
# = Create Species =
# ==================

# Number of Species
ns <- 200

# Create a brick of Species in space and time
S <- replicate(grid.t, gen.grid(dims=c(grid.h, grid.w, ns), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h), simplify=FALSE) # species information over space and time; different times are different elements of the list (top level), whereas different species are different layers in the brick. Note that in other bricks the different layers are different time steps, so be careful of that. It's done this way b/c time is going to be the top level of the process, so no more than 2 time steps need to be accessed at once, whereas 2 dimensions of space and all species might need to be accessed simultaneously.
R <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h) # Richness. Structured as other bricks (not S), b/c richness doesn't need species-specific information.

# Give the Species Temperature Preferences
rtemp <- function(n, alpha, beta, min, max){
	del <- max - min
	rbeta(n, alpha, beta)*del + min
}

min.t <- min(values(grid.temp))
max.t <- max(values(grid.temp))
mm <- replicate(ns, sort(runif(n=2, min=min.t, max=max.t)))
ab <- replicate(ns, runif(n=2, min=1, max=10))

S.obs.temps <- mapply(rtemp, alpha=ab[1,], beta=ab[2,], min=mm[1,], max=mm[2,], MoreArgs=list(n=500)) # Temperatures at which each species has been observed

S.dens.temps <- apply(S.obs.temps, 2, density, from=temp.range[1], to=temp.range[2], adjust=1) # density (prob) of each species at all possible temperatures


#
# test.t <- seq(-10, 10, length.out=500)
# dev.new(width=12, height=8)
# par(mfrow=c(7,10), mar=c(1,1,0,0))
# for(i in 1:70){
# 	test.ks <- p.persist(test.t, S.obs.temps[,i], method="ks")
# 	test.ds <- p.persist(test.t, S.obs.temps[,i], method="ds")
#
# 	plot(test.t, test.ks, type="l", ylim=c(0,1))
# 	lines(test.t, test.ds, col="red")
# }
# after seeing these plots, I'm a bit torn about which approach to use. Given that they aren't wildly different, KS test has an advantage b/c it is an established test, and the statistical interpretation of the p-value of this test is spot-on: the probability that a temperature was drawn from the same distribution as the temperature values previously-observed to be associated with the species. On the other hand, the reason the dsample() method gives higher "probabilities" within the previously-observed temperature range is b/c it's assumed to be 0 outside that range; which can be useful in its own right. However, even on the off-chance that the KS p-value allows a species to expand far outside its previous observed temperatures, this isn't all that unreasonable if temperature isn't given too much of a physiological interpretation and is simply viewed as a correlate of suitable habitat. I think that in this case the KS will win out b/c it'll be much easier to explain and defend.
# Also, I've later realized that dsample() is much faster than doing the mapply with ks.test. I can't think of a way to better program an implementation of ks.test without digging into its source code, because as-is, if x in ks.test is a vector, the probability returned is simply the probability of the x and y distributions being different.


# trying to find a way to get skew to go in either direction
# k0 <- seq(2, 50, length.out=50)
# lambda0 <- seq(0.1, 5, length.out=50)
# kl.grid <- expand.grid(k=k0, lambda=lambda0)
# k <- kl.grid[,"k"]
# lambda <- kl.grid[,"lambda"]
# mu <- lambda*gamma(1+1/k)
# sigma2 <- lambda^2*(gamma(1+2/k)-gamma(1+1/k)^2)
# skew <- (gamma(1+3/k)*lambda^3 - 3*mu*sigma2 - mu^3)/sqrt(sigma2)^3
# OK, you can get it with the weibull, but the weibull always has to be positive, so you can only get the left skew when the values are very small and positive


# Sdt.max <- max(sapply(S.dens.temps, function(x)max(x$y)))
# myGray <- rgb(t(col2rgb("black", alpha=TRUE)), alpha=75, maxColorValue=256)
# dev.new()
# plot(S.dens.temps[[1]], ylim=c(0,Sdt.max), col=myGray)
# for(i in 2:length(S.dens.temps)){lines(S.dens.temps[[i]], col=myGray)}

# Plot Temperature Distributions for the Whole Community
store <- c()
for(d in 1:512){
	ti <- c()
	for(i in 1:length(S.dens.temps)){
		ti[i] <- S.dens.temps[[i]]$y[d]
	}
	store[d] <- mean(ti)
}
dev.new(width=3.5, height=3.5)
par(mar=c(2.15,2.15,0.1,0.1), ps=10, mgp=c(1.15,0.2,0), tcl=-0.2)
plot(S.dens.temps[[1]]$x, store, type="l", xlab="Temperature", ylab="Density (community average)")


# =====================================
# = Initial Secies Population of Grid =
# =====================================

# Set up empty arrays to store species suitability, presence, and biomass
suit.pers <- array(NA, dim=c(Nv, ns, grid.t)) # environmental suitability to species persistance
spp.pres <- array(NA, dim=c(Nv, ns, grid.t)) # binary; species presence/ absence
spp.bio <- array(NA, dim=c(Nv, ns, grid.t)) # biomass (log units)

spp.bio.mu <- rnorm(n=ns)

c0 <- colonize(values(subset(grid.temp, 1)), S.dens.temps, spp.bio.mu) # initial colonization

suit.pers[,,1] <- c0[["suit.pers"]]
spp.pres[,,1] <- c0[["spp.pres"]]
spp.bio[,,1] <- c0[["spp.bio"]]


# ================================
# = Graphs of Initial Conditions =
# ================================
# Plot Initial Biomass of Each Species
spp.1 <- setValues(S[[1]], c0[["spp.bio"]]) # cell ordering is for raster
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.75,0.5,0))
dev.new(width=16, height=8)
plot(spp.1, maxnl=200, col=tim.colors(), zlim=range(values(spp.1), na.rm=TRUE),smallplot=smplt, bigplot=bgplt, axis.args=axargs, nr=8, nc=25, legend=FALSE, colNA="darkgray")

# Look at Initial Richness
spp.sample <- function(x, n){
	sub <- matrix(x[sample(nrow(x), n),], ncol=ncol(x))
	sum(apply(sub, 2, function(x)any(!is.na(x))))
}

spp.1.m <- matrix(spp.pres[,,1], nrow=Nv, ncol=ns) # cell ordering is for raster
rich <- c()
for(i in 1:min(Nv,200)){
	rich[i] <- spp.sample(spp.1.m, n=i)
}
dev.new()
plot(rich, type="o", ylim=c(0,ns))


R.1 <- stackApply(spp.1, indices=1, function(x, ...)sum(!is.na(x)))


# ====================
# = Spatial Dynamics =
# ====================
AR1.coef <- 0.8 # first order autoregressive coefficient for each cell; to be placed on the diagonal of the transition matrix for cells that previous had biomass and presently remain suitable
D.frac <- 0.5 # fraction of biomass to disperse from local cell to surroundings; think of it like reproduction, such that it doesn't result in a decrease in the biomass of the focal cell
M.frac <- 1 # proportion of biomass that will leave cell when it becomes unsuitable (this proportion is split evenly among neighbors, but not all neighbors will be suitable, so in many cases less than 100% of the biomass will find a new home)

perist.bonus <- 1 # Factor by which to adjust the probability that a species will fail to persist; decreasing this factors makes the species more likely to stick around, increasing it makes it less likely to stick around. When 1, no adjustment is made; when 0, the species will always stick around.


# Create indices to convert between matrix() and raster() conventions for naming/indexing cells
r2m <- order(c(r2m.cell(1:Nv, nrow=grid.h, ncol=grid.w))) # put this in brackets after a vector of cells ordered by the raster convention to put them in the order of the matrix convention
m2r <- order(r2m) # use this to reverse the above process


for(i in 2:grid.t){

	# Suitability of Persistance (p.persist)
	t.temp <- subset(grid.temp, i)
	c.t <- colonize(values(t.temp), S.dens.temps, spp.bio.mu)
	suit.pers[,,i] <- c.t[["suit.pers"]]

	for(s in 2:ns){
	
		# =========================
		# = Define Suitable Cells =
		# =========================
		spp.pres.t1 <- spp.pres[,s,i-1]
		pers.outcome <- c.t[["spp.pres"]][,s]
		# spp.pres.t.0 <- pers.outcome*spp.pres.t1
	
	
		# ===========================
		# = Create Adjacency Matrix =
		# ===========================
		# Dispersal Targets (create adjacency matrix)
		A <- matrix(0, nrow=Nv, ncol=Nv)
		adjac <- r2m.cell(adjacent(t.temp, cells=1:Nv, direction=8), nrow=grid.h, ncol=grid.w)
		A[adjac] <- 1 # Put a value of 1 to indicate vertices that are connected


		# ============================
		# = Create Transition Matrix =
		# ============================
		Trans <- A # Create Transition Matrix

		# Logic for dispersal, movement, and self-return
		Disperse.i <- (!is.na(spp.pres.t1) & !is.na(pers.outcome))[r2m] # present previously and currently
		Move.i <- (!is.na(spp.pres.t1) & is.na(pers.outcome))[r2m] # present previously but not currently
		AR1.i <- Disperse.i # same as for dispersal, but giving a new name just for clarity

		# Create vectors for the edge proportions
		D <- Disperse.i*D.frac
		M <- Move.i*M.frac
		AR1 <- AR1.i*AR1.coef

		# Put the edges for when i!=j into matrix form, and weight them by the degree of j
		W.v <- (D+M) * (1/colSums(A)) # vector of of the weighted movements and dispersals
		W <- matrix(W.v, nrow=Nv, ncol=Nv, byrow=TRUE) # Note: this `byrow=TRUE` should be this way regardless of the matrix/raster notation of D M etc.
		W[is.na(pers.outcome)[r2m],] <- 0 # sum(is.na(pers.outcome)[r2m] & AR1==0.8) shows that this only needs to be done for the W, not for the AR1 on the diagonal.

		# Update Transition Matrix
		Trans <- Trans*W # Update Transition matrix to include edges between vertices (no self loops yet)
		diag(Trans) <- AR1 # Update Transition matrix to include self loops (essentially autocorrelation coefficient)
	
	
		# ====================================
		# = Transition to the Next Time Step =
		# ====================================
		G.t1 <- spp.bio[r2m,s,i-1]
		G.na <- is.na(G.t1)
		G.t1[G.na] <- 0
	
		G <- Trans%*%G.t1
		G[G==0] <- NA # TODO might want to find a more reliable way of doing this; I could use pers.outcome, but this isn't always reliable because sometimes there are cells 
	
		# is.na(pers.outcome)[r2m] & G!=0
		# !is.na(pers.outcome)[r2m] & G==0
		# all((rowSums(Trans)==0) == (c(G)==0)) # maybe could use the rowSums of the transition matrix instead of the G==0
		
		spp.pres[r2m,s,i][!is.na(G)] <- 1
		spp.bio[r2m,s,i] <- G
	

	
	
	}
}



# Plot up transition results
smplt <- c(0.85,0.88, 0.2,0.8)
bgplt <- c(0.05,0.82,0.15,0.95)
axargs <- list(mgp=c(0.5,0.15,0))

dev.new(width=5, height=2)

n.steps <- 6
spp.id <- 9

par(mfrow=c(2, n.steps), oma=c(0.5,1,0,0))
year.index <- seq(1, grid.t, length.out=n.steps)
for(i in 1:n.steps){
	t.y <- year.index[i]
	
	par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(1,i))
	image.plot(t(matrix(values(subset(grid.temp, t.y)), nrow=grid.h, byrow=TRUE)), zlim=range(values(grid.temp), na.rm=TRUE), ylim=c(1.085,0), axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
	
	par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(2,i))
	image.plot(t(matrix(spp.bio[,spp.id,t.y], nrow=grid.h, byrow=TRUE)), zlim=range(spp.bio[,spp.id,], na.rm=TRUE), ylim=c(1.085,0), bg="gray", axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
}




# ============
# = Colonize =
# ============
# Give "extinct" species a chance to colonize.
colSums(spp.pres[,,1], na.rm=TRUE)==0