
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
	grid.w <- 50
	
	# Height
	grid.h <- 70
	
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
ns <- 200

# Create a matrix indicating which cells are neighbors on the grid
L <- (grid.w*grid.h) # L is the total number of cells on the grid
G.adj <- adjacent(subset(grid.blank,1), cells=1:L) # G.adj calculates which cells on the Grid are neighbors
G.n <- matrix(0, nrow=L, ncol=L) # G.n is an LxL matrix indicating whether cells can interact (are rook neighbors)
G.n[G.adj] <- 1 # update to indicate which cells can interact
plot(raster(G.n)) # visualize the pattern b/c the matrix will likely be too big to print out in console


G <- matrix(1, nrow=3, ncol=4) # graph
A <- matrix(0, nrow=length(G), ncol=length(G))
A[adjacent(as.array(G), cells=1:length(G), direction=8)] <- 1 # adjacency matrix
D <- degree(graph.lattice(c(3,4))) # uses igraph ... might have to convert much of my code to this format for the sake of modeling interactions between cells

L <- as.matrix(graph.laplacian(graph.adjacency(A), norm=T))
# diag(L) <- 0

L2 <- L
diag(L2) <- 0

(L.out <- rowSums(L2))
(L.in <- colSums(-L2))
myPhi <- matrix(L.out+L.in, nrow=3)
image.plot(myPhi)


L.out <- L*1 # if each cell has 10% of it's mass diffuse 
L.in <- t(L.out)*-1

e.vec <- eigen(L)$vectors
e.val <- eigen(L)$values

G.v <- matrix(G, nrow=1)
C0V <- t(e.vec)%*%t(G.v)

Phi <- C0V*exp(-e.val*1)
Phi.new <- e.vec%*%Phi
Phi.mat <- matrix(Phi.new, nrow=3)
image.plot(Phi.mat)

G - matrix(G, nrow=1)%*%L.out
L.out%*%matrix(G, ncol=1)


# ============================================
# = Wikipedia Example for Laplacian Operator =
# ============================================
N <- 20
C0 <- matrix(0, nrow=N, ncol=N)
C0[2:5, 2:5] <- 5
C0[10:15, 10:15] <- 10
C0[2:5, 8:13] <- 7
C0 <- matrix(C0,ncol=1)

Adj <- matrix(0, nrow=N*N, ncol=N*N)
Adj[adjacent(as.array(matrix(1, nrow=N, ncol=N)), cells=1:(N*N), direction=8)] <- 1
Deg <- diag(degree(graph.adjacency(Adj), mode="out"))

L <- Deg - Adj
# L <- as.matrix(graph.laplacian(graph.adjacency(Adj), norm=F)) # same as above, but skips calculating the degree matrix and goes straight to the Laplacian matrix
eig.L <- eigen(L)
V <- eig.L$vectors
D <- eig.L$values

C0V <- t(V)%*%C0


dev.new(width=12, height=8)
par(mfrow=c(4,6), mar=c(1,1,0.1,0.1))
for(i in seq(0,5, by=0.25)){
	Phi <- C0V*exp(-D*i) # this is the solution to the differential equation dC/dt = k*lambda*c[i]; thus, to predict one time step ahead (dt=1), simply multiply the previous C by the eigenvalues (lambda)
	Phi <- V%*%Phi
	Phi <- matrix(Phi, nrow=N, ncol=N)
	image.plot(t(Phi), ylim=c(1,0), xlim=c(0,1), zlim=c(0,10))
}

# ======================
# = End Wiki Eg for LO =
# ======================


# ======================================
# = Experimented with Laplacian Matrix =
# ======================================
M <- 60
N <- 30
C0 <- matrix(0, nrow=M, ncol=N)
C0[(M-10):M, 10:20] <- 50
image.plot(t(C0), ylim=c(1,0))
# C0[10:15, 10:15] <- 10
# C0[2:5, 8:13] <- 7
C0 <- matrix(C0,ncol=1)

# Adj <- matrix(0, nrow=M*N, ncol=M*N)
# Adj[adjacent(as.array(matrix(1, nrow=M, ncol=N)), cells=1:(M*N), direction=8)] <- 1
# Deg <- diag(degree(graph.adjacency(Adj), mode="out"))


# L <- Deg - Adj
# L <- as.matrix(graph.laplacian(graph.adjacency(Adj), norm=F)) # same as above, but skips calculating the degree matrix and goes straight to the Laplacian matrix
# eig.L <- eigen(L)
# V <- eig.L$vectors
# D <- eig.L$values

# L0 <- -as.matrix(graph.laplacian(graph.adjacency(Adj), norm=F))
# Dinv <- solve(diag(diag(L0), nrow=nrow(L0), ncol=ncol(L0))) # inverse of the degree matrix


disp <- 0.1 # 10% of the biomass in each vertex will disperse at each time step; i.e., the non-self-connecting edges would be 1/D where D is the degree of the vertex from which an edge is leaving. This would be a directed graph.
# Dinv <- solve(Deg) # inverse of the degree matrix

Trans00 <- matrix(0, nrow=M*N, ncol=M*N)


cardinal <- function(nr, nc, cdir="north"){ # TODO This is doing a little bit of boundary reflection, kinda, which is not by design and needs to be fixed (what I'm calling a boundary reflection is not actually a boundary reflection, so it isn't right even if I wanted boundary reflection)
	# stopifnot(dim(mat)==2 & ncol(mat)==2)
	stopifnot(nr>=3 & nc>=3)
	
	N <- nr*nc
	mat <- expand.grid(1:N, 1:N)
	colnames(mat) <- c("row", "column")
	mat <- as.matrix(mat)
	
	del <- mat[,2] - mat[,1]
	
	ind <- switch(cdir,
		north = del==1,
		south = del==-1,
		northwest = del==(nr+1),
		southeast = del==(-nr-1),
		northeast = del==(-nr+1),
		southwest = del==(nr-1)
	)
	
	mat[ind,] # output refers to coordinates in an adjacency matrix
	
}


Trans00[cardinal(M, N, "north")] <- 0.5
Trans00[cardinal(M, N, "northwest")] <- 0.25
Trans00[cardinal(M, N, "northeast")] <- 0.25

Trans0 <- Trans00
Trans0 <- Trans0*disp

Trans <- Trans0
diag(Trans) <- 1 - colSums(Trans)



# C0V <- t(V)%*%C0
#
# beta <- 1
# # L.rw <- Adj%*%solve(Deg)
# L.rw <- solve(Deg)%*%Adj
# L.rw[lower.tri(L.rw)] <- L.rw[lower.tri(L.rw)]*beta # amplifying the upper triangle of the transition matrix makes things move "up" when the graph is represented as a matrix. If the columns are C, and the rows are R, if there is an element at [R,C], you can think of this element as representing an input from C to R; when C > R
# L.rw[upper.tri(L.rw)] <- L.rw[upper.tri(L.rw)]/beta
#

dev.new(width=16, height=12)
par(mfrow=c(10,15), mar=c(1,1,0.1,0.1))
C.old <- C0
for(i in 1:150){
	# Phi <- C0V*exp(-D*i) # this is the solution to the differential equation dC/dt = k*lambda*c[i]; thus, to predict one time step ahead (dt=1), simply multiply the previous C by the eigenvalues (lambda)
	# Phi <- V%*%Phi
	# Phi <- matrix(Phi, nrow=N, ncol=N)
	# image.plot(t(Phi), ylim=c(1,0), xlim=c(0,1), zlim=c(0,10))
	C.t <- matrix(Trans%*%C.old, nrow=M)
	C.old <- matrix(C.t, ncol=1)
	
	image.plot(t(C.t), zlim=c(0,50), ylim=c(1,0))
}








# ==================
# = End Expt w/ LM =
# ==================



lay.mat <- as.matrix(expand.grid(4:1, 1:3))
lay.mat <- lay.mat[,c(2,1)]
plot(graph.adjacency(A), layout=lay.mat)



# Create a brick of Species in space and time
S <- replicate(grid.t, gen.grid(dims=c(grid.h, grid.w, ns), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h), simplify=FALSE) # species information over space and time; different times are different elements of the list (top level), whereas different species are different layers in the brick. Note that in other bricks the different layers are different time steps, so be careful of that. It's done this way b/c time is going to be the top level of the process, so no more than 2 time steps need to be accessed at once, whereas 2 dimensions of space and all species might need to be accessed simultaneously.
R <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h) # Richness. Structured as other bricks (not S), b/c richness doesn't need species-specific information.



# S.temp.mu <- runif(n=ns, min=min(values(grid.temp)), max=max(values(grid.temp))) #rnorm(n=ns, mean=mean(values(grid.temp)), sd(values(grid.temp)))
# S.temp.var <- runif(n=ns, min=1E-3, max=1E2)
# S.obs.temps <- mapply(rnorm, mean=S.temp.mu, sd=sqrt(S.temp.var), MoreArgs=list(n=300))
# Replacing the above temperature preference simulation with a new one that uses the beta distribution (below)

rtemp <- function(n, alpha, beta, min, max){
	del <- max - min
	rbeta(n, alpha, beta)*del + min
}

min.t <- min(values(grid.temp))
max.t <- max(values(grid.temp))
mm <- replicate(ns, sort(runif(n=2, min=min.t, max=max.t)))
ab <- replicate(ns, runif(n=2, min=1, max=10))
S.obs.temps <- mapply(rtemp, alpha=ab[1,], beta=ab[2,], min=mm[1,], max=mm[2,], MoreArgs=list(n=300))

S.dens.temps <- apply(S.obs.temps, 2, density, from=temp.range[1], to=temp.range[2])


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


Sdt.max <- max(sapply(S.dens.temps, function(x)max(x$y)))
myGray <- rgb(t(col2rgb("black", alpha=TRUE)), alpha=75, maxColorValue=256)
plot(S.dens.temps[[1]], ylim=c(0,Sdt.max), col=myGray)
for(i in 2:length(S.dens.temps)){lines(S.dens.temps[[i]], col=myGray)}

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


p.s.bg <- 1 #rep(runif(n=ns, min=0.005, max=0.5), each=grid.w*grid.h)

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

species.prob.temp <- sapply(X=S.dens.temps, FUN=dsample, values(subset(grid.temp, 1)), relative=TRUE) # columns are different species, rows are different elements of the grid, with the ordering of the rows mapping onto the grid according to the default of raster package (starting upper-left corner, progressing as when reading English; note this is different than default in matrix or array). Thus, if the grid is 7x5, species.prob.temp[23, 60] is the probability of the 60th species of existing at the temperature of the 23rd element of the grid, and the 23rd element of the grid is at [5,3] (column = 23%%5, row = ceiling(23/5))

spp.prob <- species.prob.background * species.prob.temp


relative.spt <- sapply(X=S.dens.temps, FUN=dsample, values(subset(grid.temp, 1)), relative=TRUE)

species.biomass0 <- rep(rnorm(n=ns), each=grid.w*grid.h) + 1*(c(relative.spt)-0.5) # think of this as a linear regression on log(biomass), where the right-hand side of the equation is = intercept + coefficient*temperature + coefficient*temperature^2. But the 2 temperature terms are condensed and slightly more complicated by using the empirical density. The -0.5 is there to make it so that temperatures outside of the optimum lower the biomass relative to the average. Won't really matter much. Overall point is that the right-hand term is added to the default biomass, not multiplied, because this is in log space, and that this setup should represent something recoverable by a regression of some form.

species.pres <- c(NA,1)[1+rbinom(n=ns*grid.w*grid.h, size=1, prob=spp.prob)]
species.biomass <-  species.pres * species.biomass0 # first part is pres-abs, second is biomass given present
spp.1.m <- matrix(species.pres, nrow=grid.w*grid.h, ncol=ns)
spp.1 <- setValues(S[[1]], species.biomass)

smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.75,0.5,0))
dev.new(width=16, height=8)
plot(spp.1, maxnl=200, col=tim.colors(), zlim=range(values(spp.1), na.rm=TRUE),smallplot=smplt, bigplot=bgplt, axis.args=axargs, nr=8, nc=25, legend=FALSE, colNA="darkgray")

spp.sample <- function(x, n){
	sub <- matrix(x[sample(nrow(x), n),], ncol=ncol(x))
	sum(apply(sub, 2, function(x)any(!is.na(x))))
}

rich <- c()
for(i in 1:200){
	rich[i] <- spp.sample(spp.1.m, n=i)
}
plot(rich, type="o", ylim=c(0,ns))



R.1 <- stackApply(spp.1, indices=1, function(x, ...)sum(!is.na(x)))


S.start <- # TODO 
S.1 <- subset(S,1)
nlayers

# Distribution (density) of Species across Grid Attributes

