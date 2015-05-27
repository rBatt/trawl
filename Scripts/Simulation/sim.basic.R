
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
grid.w <- 50 # Width
grid.h <- 70 # Height
grid.t <- 12 # Time

Nv <- (Nv) # Number of vertices if the Grid is a Graph (number of cells in 1 year of the grid)



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

S.obs.temps <- mapply(rtemp, alpha=ab[1,], beta=ab[2,], min=mm[1,], max=mm[2,], MoreArgs=list(n=300)) # Temperatures at which each species has been observed

S.dens.temps <- apply(S.obs.temps, 2, density, from=temp.range[1], to=temp.range[2]) # density (prob) of each species at all possible temperatures


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

# quick reminder for myself that the area under these curves == 1
# t.d <- density(rnorm(10))
# sum(t.d$y*(diff(range(t.d$x))/512))


# =====================================
# = Initial Secies Population of Grid =
# =====================================
p.s.bg <- 1 #rep(runif(n=ns, min=0.005, max=0.5), each=Nv)

# For each environmental temperature, what is the species' density at the closest match density()$x (temperature) value??

# rbinom(n=ns*Nv, size=1, prob=p.s.bg)
species.prob.background <- matrix(p.s.bg, nrow=Nv, ncol=ns)

species.prob.temp <- sapply(X=S.dens.temps, FUN=dsample, values(subset(grid.temp, 1)), relative=TRUE) # columns are different species, rows are different elements of the grid, with the ordering of the rows mapping onto the grid according to the default of raster package (starting upper-left corner, progressing as when reading English; note this is different than default in matrix or array). Thus, if the grid is 7x5, species.prob.temp[23, 60] is the probability of the 60th species of existing at the temperature of the 23rd element of the grid, and the 23rd element of the grid is at [5,3] (column = 23%%5, row = ceiling(23/5))

spp.prob <- species.prob.background * species.prob.temp # cell ordering is for raster

# relative.spt <- sapply(X=S.dens.temps, FUN=dsample, values(subset(grid.temp, 1)), relative=TRUE)

spp.bio0 <- rep(rnorm(n=ns), each=Nv) + 1*(c(species.prob.temp)-0.5) # think of this as a linear regression on log(biomass), where the right-hand side of the equation is = intercept + coefficient*temperature + coefficient*temperature^2. But the 2 temperature terms are condensed and slightly more complicated by using the empirical density. The -0.5 is there to make it so that temperatures outside of the optimum lower the biomass relative to the average. Won't really matter much. Overall point is that the right-hand term is added to the default biomass, not multiplied, because this is in log space, and that this setup should represent something recoverable by a regression of some form.

# Set up empty arrays to store species presence and biomass
spp.pres <- array(NA, dim=c(Nv, ns, grid.t))
spp.bio <- array(NA, dim=c(Nv, ns, grid.t))

# Add initial species presence and biomass to arrays
spp.pres[,,1] <- c(NA,1)[1+rbinom(n=ns*Nv, size=1, prob=spp.prob)] # cell ordering is for raster
spp.bio[,,1] <-  spp.pres[,,1] * spp.bio0 # cell ordering is for raster; first part is pres-abs, second is biomass given present

# Plot Initial Biomass of Each Species
spp.1 <- setValues(S[[1]], spp.bio[,,1]) # cell ordering is for raster

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
for(i in 1:200){
	rich[i] <- spp.sample(spp.1.m, n=i)
}
dev.new()
plot(rich, type="o", ylim=c(0,ns))



R.1 <- stackApply(spp.1, indices=1, function(x, ...)sum(!is.na(x)))



# IDK
# S.start <- # TODO 
# S.1 <- subset(S,1)


# ====================
# = Spatial Dynamics =
# ====================
i = 2
s = 1

suit.pers <- array(NA, dim=c(Nv, ns, grid.t))


# Create a matrix indicating which cells are neighbors on the grid
G.adj <- adjacent(subset(grid.blank,1), cells=1:Nv) # G.adj calculates which cells on the Grid are neighbors
G.n <- matrix(0, nrow=Nv, ncol=Nv) # G.n is an NvxNv matrix indicating whether cells can interact (are rook neighbors)
G.n[G.adj] <- 1 # update to indicate which cells can interact
# plot(raster(G.n)) # visualize the pattern b/c the matrix will likely be too big to print out in console


G <- matrix(1, nrow=3, ncol=4) # graph
A <- matrix(0, nrow=length(G), ncol=length(G))
A[adjacent(as.array(G), cells=1:length(G), direction=8)] <- 1 # adjacency matrix
D <- degree(graph.lattice(c(3,4))) # uses igraph ... might have to convert much of my code to this format for the sake of modeling interactions between cells
L <- as.matrix(graph.laplacian(graph.adjacency(A), norm=T))



# Suitability of Persistance (p.persist)
t.temp <- subset(grid.temp, i)
perist.bonus <- 1 # Factor by which to adjust the probability that a species will fail to persist; decreasing this factors makes the species more likely to stick around, increasing it makes it less likely to stick around. When 1, no adjustment is made; when 0, the species will always stick around.
for(s in 1:ns){
	suit.pers[,s,i] <- p.persist(temp.prop=values(t.temp), temp.obs=S.obs.temps[,s], method="ds")
	pers.outcome <- c(NA,1)[1+rbinom(n=length(suit.pers[,s,i]), size=1, prob=(1-(1-suit.pers)*perist.bonus))]
	spp.pres[,s,i] <- pers.outcome*spp.pres[,s,i-1] # TODO This alone shouldn't be the presence for this time step, because it doesn't account for dispersal. I'll have to delete this step once dispersal is determined. Either that or I keep it and overwrite the values by adding the dispersal result to it (removing NA's first, obviously). What's also tough is that I do need to know this product (persistence outcome times previous presence) so that I know what biomasses should attempt to disperse 100%
	
	# dev.new()
	# par(mar=c(1,1,0,0), mfrow=c(1,3))
	# image.plot((matrix(spp.pres[,s,i-1], ncol=grid.w)), ylim=c(1,0))
	# image.plot(matrix(c(suit.pers), ncol=grid.w), ylim=c(1,0), zlim=c(0,1))
	# image.plot((matrix(pers.outcome, ncol=grid.w)), ylim=c(1,0))
	# image.plot((matrix(spp.pres[,s,i], ncol=grid.w)), ylim=c(1,0), main="New Presence") # just a visual check/ reference
	
}
as.matrix(t.temp)

# Dispersal Targets (create adjacency matrix)
A <- matrix(0, nrow=Nv, ncol=Nv)
A[adjacent(as.matrix(t.temp), cells=1:length(t.temp), direction=8)] <- 1 # adjacency matrix

# Suitability of Dispersal Targets (p.persist on adjacency)

# Disperse (as a fraction of source biomass; each successful target gets ~50%)

# Persist (or not)

# Random Walk (or maybe AR(1); don't do any MA, might as well just use a population model at that point)

# End of Year
