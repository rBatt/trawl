
# sppd <- setClass("sppd", slots=c(pres.true="array", grid.X="RasterBrick", spp.densX="list"), contains="array")

# setMethod("print", c("spp"),

print.spp <- function(x, ...) {
		cat(
			"Dimensions: ", paste(dim(x), collapse=", "), "\n", 
			paste(c("grid.h","grid.w","grid.t"), c(dim(attr(x, "grid.X"))), sep=" = ", collapse="\n"),
			sep=""
		)
		
		cat("\n\n", "Number Species Possible (ns):", "\n", dim(x)[2], sep="")
		
		cat("\n", "Total Species Richness:", "\n", sum(apply(x, c(2), function(x)any(!is.na(x)))), sep="")
		
		# cat("\n\n", "Richness Across Years: ", "\n")
		s <- summary(colSums(apply(x, c(2,3), function(x)any(!is.na(x)))))
		# print(summary(s))
		
		# cat("\n\n", "Richness in any given Year/ Grid Cell: ", "\n")
		s2 <- summary(c(apply(x, c(1,3), function(x)sum(!is.na(x)))))
		# print(summary(s2))
		cat("\n\n", "Annual Species Richness:", "\n", sep="")
		rn <- c("All cells", "One cell")
		print(matrix(c(s, s2), nrow=2, byrow=TRUE, dimnames=list(rn,names(s))))
		
		invisible(x)			
}

getS <- function(x){
	stopifnot(any(class(x)=="spp"))
	d <- attr(x, "dims") #c(dim(attr(x, "grid.X")), dim(x)[2])
	# names(d) <- c("grid.h","grid.w","grid.t", "ns")
	# replicate(d["grid.t"], gen.grid(x, dims=c(d["grid.h"], d["grid.w"], d["ns"]), xmn=0, ymn=0, xmx=d["grid.w"], ymx=d["grid.h"]), simplify=FALSE)
	# gen.grid(x, dims=c(d["grid.h"], d["grid.w"], d["ns"]), xmn=0, ymn=0, xmx=d["grid.w"], ymx=d["grid.h"])
	
	S0 <- unlist(apply(out, 3, function(x)list(array(c(x), dim=c(d["grid.h"],d["grid.w"],d["ns"])))),F,F)
	lapply(S0, brick, xmn=0, xmx=d["grid.w"], ymn=0, ymx=d["grid.h"])
	
}  

# et <- sim.env()
# Rprof()
# sim.spp.proc(et)
# Rprof(NULL)
# summaryRprof()

sim.spp.proc <- function(grid.X, ns=200, niche.bias){
	
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
	rX <- function(n, alpha, beta, min, max){
		del <- max - min
		rbeta(n, alpha, beta)*del + min # random beta distribution, rescaled
	}
	
	mm <- replicate(ns, sort(rX(n=2, niche.bias[1], niche.bias[2], min=min.X, max=max.X)))
	# mm <- replicate(ns, sort(runif(n=2, min=min.X, max=max.X))) # give each species its own min/max X tolerance
	ab <- replicate(ns, runif(n=2, min=1, max=10)) # alpha beta parameters determine the "shape" of the beta distribution

	# Use the rescaled beta distribution to give each species a fake history of observed temperatures
	S.obs.X <- mapply(rX, alpha=ab[1,], beta=ab[2,], min=mm[1,], max=mm[2,], MoreArgs=list(n=500))

	# Use that fake history of observed temperatures to generate an empirical density (like histogram)
	S.dens.X <- apply(S.obs.X, 2, density, from=X.range[1], to=X.range[2], adjust=1)


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
	c0 <- colonize(values(subset(grid.X, 1)), S.dens.X, spp.bio.mu)

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
		
		
			# ====================
			# = Fill in Matrices =
			# ====================
			spp.pres[r2m,s,i][!is.na(G)] <- 1
			spp.bio[r2m,s,i] <- G
	
		}
	}
	
	out <- structure(spp.pres, class=c("spp", "array"))
	attr(out, "spp.bio") <- spp.bio
	attr(out, "grid.X") <- grid.X
	attr(out, "spp.densX") <- S.dens.X
	d <- c(dim(grid.X), ns)
	names(d) <- c("grid.h","grid.w","grid.t", "ns")
	attr(out, "dims") <- d
	
	out
}