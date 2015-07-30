
# Function for observing species



# @' n.ss number of substrata that exist in each stratum (default 9)
# @' n.ss.mu number of substrata to sample over the whole region (default \code{max(trunc((n.strat*n.ss)/3), n.strat)})
# @' n.noID number of taxa that will not be ID'd (0% detectability) in the first half of the time series
# @' base.chance numeric vector with length equal number species indicating each species' detectability (0-1)

obs.spp <- function(x, n.ss=9, n.ss.mu, n.noID, base.chance, t.noID){
	
	stopifnot(any("spp" == class(x)))
	
	
	
	
	# spp.bio <- attr(x, "spp.bio")
	grid.X <- attr(x, "grid.X")
	# S.dens.X <- attr(x, "spp.densX")
	dims <- attr(x, "dims")
	
	# =====================
	# = Observe Substrata =
	# =====================
	# Probabilities for sampling a spot
	# Each grid cell needs to be subdivided
	S <- getS(x)
	S.ss <- lapply(S, disaggregate, fact=sqrt(n.ss), method="")
	
	S.ss <- lapply(S.ss, function(x){x[is.na(x)]<-0;x})
	
	# Subdivided grid cells have constant probability of being observed
	
	stratum <- setValues(subset(grid.X,1), 1:ncell(grid.X))
	sub.stratum <- disaggregate(stratum, fact=sqrt(n.ss), method="")
	
	n.strat <- prod(attr(out, "dims")[1:2])
	if(missing(n.ss.mu)){
		n.ss.mu <- max(trunc((n.strat*n.ss)/3), n.strat)
	}else{
		if(n.ss.mu < n.strat){
			n.ss.mu <- n.strat
			warning("Fewer sampling locations than strata; n.ss.mu set to n.strat")
		}else if(n.ss.mu>(n.strat*n.ss)){
			n.ss.mu <- n.strat*n.ss
			warning("More sampling locations than substrata; n.ss.mu set to n.strat*n.ss")
		}
	}
	
	# n.ss.mu <- trunc((ncell(stratum)*n.ss)/3) # average number of substrata observed (total)
	# n.ss.obs0 <- min(rpois(1, n.ss.mu), ncell(sub.stratum)) # number of substrata observed this time
	n.ss.obs0 <- n.ss.mu # number of substrata observed this time
	
	all.ss <- 1:ncell(sub.stratum) # id of all possible substrata
	guaran.ss <- sampleStratified(sub.stratum, 1)[,"cell"] # the guaranteed substrata; 1 from each strat
	other.ss <- all.ss[!all.ss%in%guaran.ss] # the substrata that are not guaranteed (but could be selected); note: could be done numerically, b/c strata are ordered 1:n, but keeping this way just b/c I've been thinking of the [sub]strata as arbitrary ID's
	
	n.ss.obs <- n.ss.obs0 - length(guaran.ss) # length(guaran.ss) is set to ncell(stratum) right now	
	
	obs.ss.cells <- c(guaran.ss, sample(other.ss, n.ss.obs, F)) # the cells of the observed substrata
	obs.ss <- setValues(sub.stratum, NA) # default is to have all cells as NA (cells that were unvisited (unsampled) will always be NA, regardless of whether a species is there or not/ regardless of whether or not a species would be detected if it were to be visited)
	obs.ss[obs.ss.cells] <- 1 # replace cells that are visisted with 1's
	# obs.ss is the substrata that are observed. This doesn't change across years (as currently setup), and is (obviously) constant across speices
	
	# Number of substrata sampled per stratum
	n.haul <- table(extract(sub.stratum, obs.ss.cells)) # the number of sampled substrata per stratum
	# hist(table(extract(sub.stratum, obs.ss.cells)))
	
	# Plot showing which substrata were sampled
	# dev.new(height=3.5, width=2.5+0.95)
	# par(mar=c(2,2,0.25,0.1))
	# plot(sub.stratum)
	# image((obs.ss), add=TRUE, col=adjustcolor("black", 0.25), axes=FALSE, xlab="", ylab="")
	
	
	# =================
	# = Detectability =
	# =================
	# Baseline detectability
	if(missing(base.chance) | length(base.chance)!=ns | any(base.chance<0) | any(base.chance>1)){
		if(length(base.chance)!=ns | any(base.chance<0) | any(base.chance>1)){
			warning("Discarding supplied base.chance, using default. Number of chances needs to equal number of species, and all chances between 0 and 1.")
		}
		base.chance <- runif(ns, 0.2, 0.8) # baseline detectability; specific to species, not to place or time
	}
	
	# Taxonomic detectability
	if(missing(n.noID)){
		n.noID <- dims["ns"]/2 # number of species that won't be ID'd in first part of time series (say half)
	}
	if(missing(t.noID) | length(t.noID)!=dims["grid.t"]){
		if(length(t.noID)!=dims["grid.t"]){
			warning("t.noID needs to have 1 integer value per year, but had incorrect length; setting t.noID to default.")
		}
		r1 <- floor(grid.t/2)
		r2 <- ceiling(grid.t/2)
		t.noID <- c(rep(0, r1), rep(1, r2))
	}
	tax.chance <- matrix(1, nrow=dims["grid.t"], ncol=dims["ns"]) # start but giving each species 100% chance of being ID'd
	tax.chance[,1:n.noID] <- t.noID # then make select species impossible to ID for part of time series (first half, e.g.)
	
	# Total detectabilty
	detectability <- base.chance * t(tax.chance) # thus the detectability of a species (given that it is present, and given that the spot is sampled) is the product of its baseline detectability and whether or not it is being ID'd
	

	# =========================
	# = Detectability Outcome =
	# =========================
	# The Observation Outcome Given Presence and Sampling
	
	# Reformat chance of observation (which is just detectability)
	detect.chance0 <- aperm(array(c(detectability), dim=c(dims["ns"], dims["grid.t"], ncell(grid.X))),c(3,1,2)) # reformat
	detect.chance <- apply(detect.chance0, c(2,3), rep, each=n.ss) # expand (rep) based on number of substrata
	detect.chance <- unlist(apply(detect.chance, 3, list),F,F) # reformat
	
	# Function to flip a coin based on chance of observation (for this data format)
	rb2 <- function(x){
		x[] <- rbinom(n=prod(dim(x)), size=1, prob=c(x))
		x
	}
	
	# Execute coin flip
	detect.outcome0 <- lapply(detect.chance, rb2) #rbinom(prod(dim(detect.chance)), 1, c(detect.chance)) 
	detect.outcome <- mapply(setValues, S.ss, detect.outcome0) # put coinflip outcome into S.ss raster format
	
	
	# =======================
	# = Observation Outcome =
	# =======================
	# Convert detectability outcome to observation outcome by 
	# multiplying by 1 or 0 for species presence/ absence,
	# and multiplying by 1 or 0 for whether or not a substratum was sampled
	S.obs <- mapply(bquote("*"), detect.outcome, S.ss) # multiply detectability chance by presence/absence
	obs <- lapply(S.obs, function(x)x*obs.ss) # multiply 
	
	# only places that you didn't visit should be NA
	# for every place visited, value should be 1 or 0
	
	# ==================
	# = Prepare Output =
	# ==================
	# Values needed for printing method
	xva <- simplify2array(lapply(obs, values))
	tr <- sum(apply(xva, c(2), function(x)any(!is.na(x)&x==1)))
	rich.true <- colSums(apply(x, c(2,3), function(x)any(!is.na(x))))
	rich.obs <- colSums(apply(xva, c(2,3), function(x)any(!is.na(x)&x==1)))
	s <- summary(rich.obs)
	s2 <- summary(c(apply(xva, c(1,3), function(x)sum(!is.na(x)&x==1))))
	
	detect.smry <- sapply(detect.chance, function(x)apply(x, 2, mean))
	
	
	# Assign attributes
	attr(x, "obs") <- obs
	attr(x, "obs.arr") <- xva
	attr(x, "n.haul") <- n.haul
	attr(x, "obs.params") <- list(base.chance=base.chance, tax.chance=tax.chance, detect.chance=detect.chance, detect.smry=detect.smry)
	attr(x, "prnt.obs") <- list(tr=tr, s=s, s2=s2)
	attr(x, "richness") <- data.frame(rich.true=rich.true, rich.obs=rich.obs)
	
	
	# Return observed species
	x
	
}





















# 1 - (p(1-A) * p(1-B) * p(1-C)) == p(A|B|C)

# A <- 0.25
# B <- 0.25
# C <- 0.25
# p <- c(A,B,C)
#
# all.same <- function(x){
# 	abs(max(x) - min(x)) < 8.881784e-16 # number is (.Machine$double.eps)*4 on my rMBP
# }
#
# n <- 3:1
# pTot <- function(p, n=1){
# 	ln <- length(n)
# 	if(ln>1 | !all.same(p)){
# 		# stopifnot(ln == length(p))
# 		pn <- 1-((1-p)^n)
# 	}else{
# 		pn <- p
# 	}
#
# 	1-prod(1 - pn)
#
# }



