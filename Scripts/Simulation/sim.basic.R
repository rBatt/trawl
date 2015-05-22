
# =================
# = Load Packages =
# =================
library(raster)
library(fields)


# ======================
# = Load Sim Functions =
# ======================
sim.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/SimFunctions"
invisible(sapply(paste(sim.location, list.files(sim.location), sep="/"), source, .GlobalEnv))



# ===============
# = Create Grid =
# ===============

# Grid Size

	# Width
	grid.w <- 5
	
	# Height
	grid.h <- 7
	
	# Time
	grid.t <- 12


# Grid Attributes

	# Grid Lat
	
	# Grid Lon
	
	# Grid temperature
	temp.h <- seq(-10, 0, length.out=grid.h)
	temp <- c()
	for(i in 1:grid.w){
		temp <- c(temp, temp.h+rnorm(grid.h))
	}
	
	# Grid depth

# Create Blank Grid
grid.blank <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

# Insert first year of temperature grid
grid.temp.1 <- gen.grid(temp, dims=c(grid.h, grid.w, 1), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

# Define temperature change in ºC per year
temp.slope <- 0.75

# Create time series of temperature grid
grid.temp <- grid.blank
grid.temp <- setValues(grid.temp, values(grid.temp.1), layer=1)

rg <- function(){ # function to add noise to the temperature process (kinda weird, b/c I apply the noise at each time step)
	rnorm(n=grid.w*grid.h, sd=0.1)
}
for(i in 2:grid.t){
	grid.temp <- setValues(grid.temp, values(subset(grid.temp, i-1))+rg()+temp.slope, layer=i)
}

# Plot temperature time series
temp.range <- range(values(grid.temp))
plot(grid.temp, zlim=c(temp.range[1], temp.range[2]), col=tim.colors())


# ==================
# = Create Species =
# ==================

# Number of Species
ns <- 100

# Create a matrix indicating which cells are neighbors on the grid
L <- (grid.w*grid.h) # L is the total number of cells on the grid
G.adj <- adjacent(subset(grid.blank,1), cells=1:L) # G.adj calculates which cells on the Grid are neighbors
G.n <- matrix(0, nrow=L, ncol=L) # G.n is an LxL matrix indicating whether cells can interact (are rook neighbors)
G.n[G.adj] <- 1 # update to indicate which cells can interact
plot(raster(G.n)) # visualize the pattern b/c the matrix will likely be too big to print out in console


# Create a brick of Species in space and time
S <- replicate(grid.t, gen.grid(dims=c(grid.h, grid.w, ns), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h), simplify=FALSE) # species information over space and time; different times are different elements of the list (top level), whereas different species are different layers in the brick. Note that in other bricks the different layers are different time steps, so be careful of that. It's done this way b/c time is going to be the top level of the process, so no more than 2 time steps need to be accessed at once, whereas 2 dimensions of space and all species might need to be accessed simultaneously.
R <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h) # Richness. Structured as other bricks (not S), b/c richness doesn't need species-specific information.

S.temp.mu <- rnorm(n=ns, mean=mean(values(grid.temp)), sd(values(grid.temp)))
S.temp.var <- runif(n=ns, min=1E-3, max=1E2)
S.obs.temps <- mapply(rnorm, mean=S.temp.mu, sd=sqrt(S.temp.var), MoreArgs=list(n=300))
S.dens.temps <- apply(S.obs.temps, 2, density, from=temp.range[1], to=temp.range[2])

Sdt.max <- max(sapply(S.dens.temps, function(x)max(x$y)))
myGray <- rgb(t(col2rgb("black", alpha=TRUE)), alpha=75, maxColorValue=256)
plot(S.dens.temps[[1]], ylim=c(0,Sdt.max), col=myGray)
for(i in 2:100){lines(S.dens.temps[[i]], col=myGray)}

store <- c()
for(d in 1:512){
	ti <- c()
	for(i in 1:length(S.dens.temps)){
		ti[i] <- S.dens.temps[[i]]$y[d]
	}
	store[d] <- mean(ti)
}
plot(S.dens.temps[[1]]$x, store, type="l", xlab="Temperature", ylab="Density (community average)")

# quick reminder for myself that the area under these curves == 1
# t.d <- density(rnorm(10))
# sum(t.d$y*(diff(range(t.d$x))/512))


# world.slots <- 1:200
# individs.world <- rep(NA, max(world.slots))
# species.pool <- 1:100
# for(i in 1:1E4){
# 	individs.world[sample(world.slots, size=1)] <- sample(species.pool, size=1)
# }
# hist(table(individs.world))


p.s.bg <- 0.5

# For each environmental temperature, what is the species' density at the closest match density()$x (temperature) value??

# rbinom(n=ns*grid.w*grid.h, size=1, prob=p.s.bg)
species.prob.background <- matrix(p.s.bg, nrow=grid.w*grid.h, ncol=ns)



dsample <- function(ref,x, relative=FALSE){
	# What is the density of each observation in X given the empirical distribution of a sample ref?
	# Similar to dnorm(), e.g., but instead of assuming the normal distribution, instead assumes an empirical distribution based on density(ref)
	# If ref is of class == "density", then the density of a value in x is calculated based on ref, otherwise, calculated based on density(ref).
	if(class(ref)!="density"){
		ref <- density(ref)
	}
	
	ref.y <- ref$y
	ref.x <- ref$x
	
	ds <- approxfun(ref)
	
	dsx <- ds(x)
	if(relative){
		return(dsx/max(dsx))
	}else{
		return(dsx)
	}
	
}

species.prob.temp <- sapply(X=S.dens.temps, FUN=dsample, values(subset(grid.temp, 1))) # columns are different species, rows are different elements of the grid, with the ordering of the rows mapping onto the grid according to the default of raster package (starting upper-left corner, progressing as when reading English; note this is different than default in matrix or array). Thus, if the grid is 7x5, species.prob.temp[23, 60] is the probability of the 60th species of existing at the temperature of the 23rd element of the grid, and the 23rd element of the grid is at [5,3] (column = 23%%5, row = ceiling(23/5))

spp.prob <- species.prob.background * species.prob.temp

relative.spt <- species.prob.temp <- sapply(X=S.dens.temps, FUN=dsample, values(subset(grid.temp, 1)), relative=TRUE)

species.biomass0 <- rep(rnorm(n=ns), each=grid.w*grid.h) + 1*(c(relative.spt)-0.5) # think of this as a linear regression on log(biomass), where the right-hand side of the equation is = intercept + coefficient*temperature + coefficient*temperature^2. But the 2 temperature terms are condensed and slightly more complicated by using the empirical density. The -0.5 is there to make it so that temperatures outside of the optimum lower the biomass relative to the average. Won't really matter much. Overall point is that the right-hand term is added to the default biomass, not multiplied, because this is in log space, and that this setup should represent something recoverable by a regression of some form.

species.pres <- c(NA,1)[1+rbinom(n=ns*grid.w*grid.h, size=1, prob=p.s.bg)]
species.biomass <-  species.pres * species.biomass0 # first part is pres-abs, second is biomass given present
spp.1.m <- matrix(species.pres, nrow=grid.w*grid.h, ncol=ns)
spp.1 <- setValues(S[[1]], species.biomass)

spp.sample <- function(x, n){
	sub <- matrix(x[sample(nrow(x), n),], ncol=ncol(x))
	sum(apply(sub, 2, function(x)any(!is.na(x))))
}

rich <- c()
for(i in 1:10){
	rich[i] <- spp.sample(spp.1.m, n=i)
}
plot(rich, type="o")



R.1 <- stackApply(spp.1, indices=1, function(x, ...)sum(!is.na(x)))


S.start <- # TODO 
S.1 <- subset(S,1)
nlayers

# Distribution (density) of Species across Grid Attributes

