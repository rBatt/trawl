

sim.spp.proc <- function(grid.X, ns=200, niche.bias, dynamic=TRUE){
	
	# =================
	# = Niche Options =
	# =================
	if(missing(niche.bias)){
		niche.bias <- c(1, 1)
	}else{
		stopifnot(length(niche.bias)==2 & all(niche.bias>=0))
	}
	 # Alters the potential environmental preferences of species relative to the observed minimum and maximum of that environmental variable. Goes to beta distribution. If the first number is higher, high X favored; if second number higher, low X favored. If niche.bias[1]/niche.bias[2] > 1, then the upper extreme of the environment will be favored by more species; if < 1, then the lower extreme will be favored. If niche.bias[1] == niche.bias[2] and they are both less than 1, then extreme environments will be equally preferred over moderate temperature (concave pdf). If niche.bias[1] == niche.bias[2] and both are greater than 1, the moderature temperatures will be preferred over extremes.

	# ====================
	# = Dynamics Options =
	# ====================
	AR1.coef <- 0.8 # first order autoregressive coefficient for each cell; to be placed on the diagonal of the transition matrix for cells that previous had biomass and presently remain suitable
	D.frac <- 0.5 # fraction of biomass to disperse from local cell to surroundings; think of it like reproduction, such that it doesn't result in a decrease in the biomass of the focal cell
	M.frac <- 1 # proportion of biomass that will leave cell when it becomes unsuitable (this proportion is split evenly among neighbors, but not all neighbors will be suitable, so in many cases less than 100% of the biomass will find a new home)
	perist.bonus <- 1 # Factor by which to adjust the probability that a species will fail to persist; decreasing this factors makes the species more likely to stick around, increasing it makes it less likely to stick around. When 1, no adjustment is made; when 0, the species will always stick around.

	# ================
	# = Grid Options =
	# ================
	# Grid Size
	dim.X <- dim(grid.X)
	grid.w <- dim.X[2] #5 # Width
	grid.h <- dim.X[1] #7 # Height
	grid.t <- dim.X[3] #12 # Time

	Nv <- (grid.w*grid.h) # Number of vertices if the Grid is a Graph (number of cells in 1 year of the grid)

	# Range of observed temperture values across all years and cells
	X.range <- range(values(grid.X))
	min.X <- X.range[1] #min(values(grid.X)) # minimum possible temperature tolerance
	max.X <- X.range[2] #max(values(grid.X)) # the maximum temperature a species can tolerate


	# ==================
	# = Create Species =
	# ==================

	# Create a brick of Species in space and time
	S <- replicate(grid.t, gen.grid(dims=c(grid.h, grid.w, ns), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h), simplify=FALSE) 
	# species information over space and time; different times are different elements of the list (top level), whereas different species are different layers in the brick. 
	# Note that in other bricks the different layers are different time steps, so be careful of that. It's done this way b/c time is going to be the top level of the process, so no more than 2 time steps need to be accessed at once, whereas 2 dimensions of space and all species might need to be accessed simultaneously.

	# Give the Species Temperature Preferences
	# rX <- function(n, alpha, beta, min, max){
	# 	del <- max - min
	# 	rbeta(n, alpha, beta)*del + min # random beta distribution, rescaled
	# }
	
	# mm <- replicate(ns, sort(rX(n=2, niche.bias[1], niche.bias[2], min=min.X, max=max.X)))
	# mm <- replicate(ns, sort(runif(n=2, min=min.X, max=max.X))) # give each species its own min/max X tolerance
	# ab <- replicate(ns, runif(n=2, min=1, max=5)) # alpha beta parameters determine the "shape" of the beta distribution; increasing the average values of these parameters narrows the niche width of the species, and will also create more skew. See scratch script and equation for variance of beta distribution.
	# deg.sep <- 2 # how many degrees of separation do we want for a species' min and max possible temps?
	# added.sep <- pmax(2-(mm[2,]-mm[1,]),0) # how many
	# ab <- replicate(ns, rlnorm(2, 0, 0.5))
	
	# simulate in a way that is more directly comparable to analysis
	# of course, this is what I was trying to avoid, but 
	# at least if I do this I'll know what the results mean
	# mus <- runif(ns, min.X, max.X)
	# sds <- sqrt(rgamma(ns, 3, 2))
	# rX2 <- function(mean, sd){rnorm(n=n, mean=mean, sd=sd)}
	
	
	# OK, fine. I'll simulate in a way that's damn near identical to the way the
	# MSOM works. I've been having trouble recovering parameters, and I'm not sure
	# what the deal is (maybe I have an error somewhere, statistical or
	# programmatic), but I've fiddled enough to feel like it's time to do what I can
	# to get this to run and return the "right" answer.
	
	psiMod <- function(b0, b3, b4, X, n){

		if(missing(X)){
			if(missing(n)){
				n <- 100
			}
			X <- seq(1,30, length.out=n)
			cbind(X=X, psi=plogis(b0+b3*X+b4*X^2))
		}else{
			list(x=X, y=plogis(b0 + b3*X + b4*X^2))
		}
		
	}
	
	# psi.opt <- function(b1,b2){-b1/(2*b2)}
	# psi.tol <- function(b2){1/sqrt(-2*b2)}
	# psi.max <- function(b0,b1,b2){1/(1+exp((b1^2)/(4*b2)-b0))}
	

	# parent means
	mu.u.a0 <- 0.5
	mua3 <- 0.00
	mua4 <- -0.025

	# precisions (all species share a precision)
	# I'm additionally assuming all of these parameters have the same
	# precision, but that might not be true
	# (this constraint does not exist in the msom)
	tau.u.a0 <- 1/0.5^2
	tau.a3 <- 1/0.21^2
	tau.a4 <- 1/0.008^2

	# species-specific means of logistic regression parameters
	u.a0 <- rnorm(ns, mu.u.a0, sqrt(1/tau.u.a0))
	a3 <- rnorm(ns, mua3, sqrt(1/tau.a3)) #~ dnorm(0, 0.001)
	a4 <- rnorm(ns, mua4, sqrt(1/tau.a4)) #~ dnorm(0, 0.001)
	

	# ======================================================================================
	# = Calculate Probabilities of Species Occurrence Given Logistic Regression Parameters =
	# ======================================================================================
	range.X <- range(values(grid.X))
	Xvals <- do.call("seq",c(as.list(range.X),list(length.out=500)))
	S.dens.X <- mapply(psiMod, b0=u.a0, b3=a3, b4=a4, MoreArgs=list(X=Xvals), SIMPLIFY=F)
	
	p.suit2 <- simplify2array(mapply(function(...)psiMod(...)$y, u.a0, a3, a4, MoreArgs=list(X=values(subset(grid.X, 1))), SIMPLIFY=F)) # this can now go to colonize() instead of having to rely on dsample(); I tested, and it shouldn't matter much provided that a large enough reference sample size is used.
	
	
	
	
	# ==========================================================================
	# = Graph Psi_max, Environmental Optima, and Environmental Response curves =
	# ==========================================================================
	# mua34 <- expand.grid(sort(a3),  sort(a4))
# 	mu.psi.max <- psi.max(mu.u.a0, mua34[,1], mua34[,2])
# 	mu.psi.opt <- psi.opt(mua34[,1], mua34[,2])
# 	# image.plot(matrix(mu.psi.max, nrow=length(a3), byrow=TRUE, dimnames=list(sort(a3), sort(a4))), zlim=0:1)
# 	par(mfrow=c(1,3))
#
#
# 	image.plot(x=sort(a3), y=sort(a4), z=matrix(mu.psi.max, nrow=length(unique(mua34[,1])), byrow=F), zlim=0:1)
# 	points(a3,a4, pch=20)
# 	abline(v=c(-0.5, 0.5))
# 	abline(h=-0.04)
# 	image.plot(x=sort(a3), y=sort(a4), z=matrix(mu.psi.opt, nrow=length(unique(mua34[,1])), byrow=F), zlim=c(-15,15))
# 	points(a3,a4, pch=20)
# 	abline(v=c(-0.5, 0.5))
# 	abline(h=-0.04)
#
# 	# Plot response curves that were just generated:
# 	plot(S.dens.X[[1]], ylim=0:1, type="l", col=adjustcolor("black",alpha.f=0.25))
# 	invisible(sapply(S.dens.X[-1], lines, col=adjustcolor("black",alpha.f=0.25)))
# 	lines(S.dens.X[[1]]$x, apply(simplify2array(lapply(S.dens.X, function(x)x$y)), 1, mean), lwd=3)
#
	
	# =====================================
	# = Initial Secies Population of Grid =
	# =====================================

	# Set up empty arrays to store species suitability, presence, and biomass
	suit.pers <- array(NA, dim=c(Nv, ns, grid.t)) # environmental suitability to species persistance
	spp.pres <- array(NA, dim=c(Nv, ns, grid.t)) # binary; species presence/ absence
	spp.bio <- array(NA, dim=c(Nv, ns, grid.t)) # biomass (log units)

	# Give each species its own average biomass
	# Note that this biomass feature doesn't affect the simulation in terms of presence/ absence
	spp.bio.mu <- rnorm(n=ns)

	# Initial Colonization (colonize() is a custom function)
	# colonize() looks at the empirical history of an enivornmental
	# gradient for a species, and then uses that history to calculate the probability of the
	# species being present at a given temperature
	# given that probability, we can flip a count to decide 1 (present) or 0 (absent)
	# This process is repeated for each grid cell in the first year, and each species
	# The result is an initial distribution of species
	c0 <- colonize(values(subset(grid.X, 1)), S.dens.X, spp.bio.mu, relative=FALSE, probs=p.suit2)

	# Store some values from the initial colonization
	# These array were defined above.
	suit.pers[,,1] <- c0[["suit.pers"]]
	spp.pres[,,1] <- c0[["spp.pres"]]
	spp.bio[,,1] <- c0[["spp.bio"]]
	
	# ===========================
	# = Create Adjacency Matrix =
	# ===========================
	# Dispersal Targets (create adjacency matrix)
	A <- matrix(0, nrow=Nv, ncol=Nv)
	adjac <- r2m.cell(adjacent(subset(grid.X, 1), cells=1:Nv, direction=8), nrow=grid.h, ncol=grid.w)
	A[adjac] <- 1 # Put a value of 1 to indicate vertices that are connected
	
	
	# ====================
	# = Spatial Dynamics =
	# ====================
	# Create indices to convert between matrix() and raster() conventions for naming/indexing cells
	r2m <- order(c(r2m.cell(1:Nv, nrow=grid.h, ncol=grid.w))) # put this in brackets after a vector of cells ordered by the raster convention to put them in the order of the matrix convention
	m2r <- order(r2m) # use this to reverse the above process
	
	if(grid.t>1){
		
		# Begin looping through time steps to simulate dynamics
		for(i in 2:grid.t){ # through time

			# Suitability of Persistance (p.persist)
			t.X <- subset(grid.X, i)
			c.t <- colonize(values(t.X), S.dens.X, spp.bio.mu)
			suit.pers[,,i] <- c.t[["suit.pers"]]

			for(s in 2:ns){ # through species
	
				# =========================
				# = Define Suitable Cells =
				# =========================
				spp.pres.t1 <- spp.pres[,s,i-1]
				pers.outcome <- c.t[["spp.pres"]][,s]
				# spp.pres.t.0 <- pers.outcome*spp.pres.t1

				if(dynamic){
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
					G[G==0] <- NA # TODO might want to find a more reliable way of doing this; I could use pers.outcome, but this isn't always reliable because sometimes there are cells that are suitable but are isolated and did not previously have anything.
		
					# ==================================
					# = Colonize if Previously Extinct =
					# ==================================
					if(all(G.na)){ # If completely absent last time step, give it a chance to colonize
						G[] <- c.t[["spp.bio"]][r2m,s]
					}
	
					# is.na(pers.outcome)[r2m] & G!=0
					# !is.na(pers.outcome)[r2m] & G==0
					# all((rowSums(Trans)==0) == (c(G)==0)) # maybe could use the rowSums of the transition matrix instead of the G==0
		
		
					# ===============
					# = Random Walk =
					# ===============
					G <- G + rnorm(n=length(G), mean=0, sd=0.1)
				
				}else{
					G <- matrix(c.t[["spp.bio"]][r2m,s], nrow=Nv)
				}
		
				# ====================
				# = Fill in Matrices =
				# ====================
				spp.pres[r2m,s,i][!is.na(G)] <- 1
				spp.bio[r2m,s,i] <- G
	
			}
		}
	}
	
	
	
	out <- structure(spp.pres, class=c("spp", "array"))
	
	tr <- sum(apply(out, c(2), function(x)any(!is.na(x))))
	s <- summary(colSums(apply(out, c(2,3), function(x)any(!is.na(x)))))
	s2 <- summary(c(apply(out, c(1,3), function(x)sum(!is.na(x)))))
	
	
	attr(out, "spp.bio") <- spp.bio
	
	attr(out, "grid.X") <- grid.X
	attr(out, "spp.densX") <- S.dens.X
	# attr(out, "proc.params") <- list(sd.occupiedX=apply(S.obs.X,2,sd),cov.params=data.frame(mus=mus,sds=sds))
	
	# Add True Psi Parameter Value as Attribute
	list.grid.X <- unlist(apply(values(grid.X), 2, list),F,F)
	psi0 <- mapply(
		function(b0,b3,b4,X){
			sapply(X, function(x){psiMod(X=x, b0, b3, b4)$y})
			},
			b0=u.a0, b3=a3, b4=a4, MoreArgs=list(X=list.grid.X), SIMPLIFY=F
		)
	# psi0 <- mapply(
# 		function(mu0, sd0, X){
# 			sapply(X, function(x){dnorm(x, mu0, sd0)/dnorm(mu0, mu0, sd0)})
# 		},
# 		mu0=mus, sd0=sds, MoreArgs=list(X=list.grid.X), SIMPLIFY=F
# 	)
	psi <- aperm(simplify2array(psi0), c(1,3,2))
	attr(out, "psi") <- psi
	attr(out, "u.a0") <- u.a0
	attr(out, "a3") <- a3
	attr(out, "a4") <- a4
	
	# Add True Z as Attribute
	# Z <- spp.pres[m2r,,]
	Z <- spp.pres
	Z[is.na(Z)] <- 0
	# plot(colSums(apply(Z,2:3,max)), type="l") # e.g., plot total richness over time
	# par(mfrow=c(2,2)) # histograms of species' psi values, summarized as mean, median, max, or min across years and sites
	# hist(apply(psi, c(2), mean))
	# hist(apply(psi, c(2), median))
	# hist(apply(psi, c(2), max))
	# hist(apply(psi, c(2), min))
	attr(out, "Z") <- Z
	
	
	
	d <- c(dim(grid.X), ns)
	names(d) <- c("grid.h","grid.w","grid.t", "ns")
	attr(out, "dims") <- d
	
	attr(out, "prnt") <- list(tr=tr, s=s, s2=s2)
	
	attr(out, "obs") <- NA
	attr(out, "X.obs") <- NA
	attr(out, "Z.obs") <- NA
	attr(out, "p") <- NA
	attr(out, "n.haul") <- NA
	attr(out, "obs.params") <- NA
	attr(out, "prnt.obs") <- NA
	attr(out, "richness") <- NA
	
	out
}