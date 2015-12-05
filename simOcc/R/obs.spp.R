#' Function for observing species
#' 
#' Given simulated species process, observe that process
#' 
#' @param n.ss number of substrata that exist in each stratum (default 9). Must be a perfect square.
#' @param n.ss.mu number of substrata to sample over the whole region (default \code{trunc((n.ss*grid.w*grid.h)*(100/100))})
#' @param base.chance numeric vector with length equal number species indicating each species' detectability (0-1). Can leave it as missing.
#' 
#' @import raster
#' @export
obs.spp <- function(x, n.ss=9, n.ss.mu, base.chance=rep(1, ns), t.noID){
	
	stopifnot(any("spp" == class(x)))
	
	if(sqrt(n.ss)%%1 != 0){
		stop("n.ss must be a perfect square")
	}
	
	grid.X <- attr(x, "grid.X")
	dims <- attr(x, "dims")
	grid.w <- dims["grid.w"]
	grid.h <- dims["grid.h"]
	grid.t <- dims["grid.t"]
	ns <- dims["ns"]
	
	if(missing(n.ss.mu)){
		n.ss.mu <- trunc((n.ss*grid.w*grid.h)*(100/100))
	}
	

	
	# =====================
	# = Observe Substrata =
	# =====================
	# Probabilities for sampling a spot
	# Each grid cell needs to be subdivided
	S <- getS(x)
	S.ss <- lapply(S, disaggregate, fact=sqrt(n.ss), method="")
	
	S.ss <- lapply(S.ss, function(x){x[is.na(x)]<-0;x})
	
	# Subdivided grid cells have constant probability of being observed
	
	stratum <- setValues(raster::subset(grid.X,1), 1:ncell(grid.X))
	sub.stratum <- disaggregate(stratum, fact=sqrt(n.ss), method="")
	
	n.strat <- prod(attr(x, "dims")[1:2])
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
	n.haul <- table(raster::extract(sub.stratum, obs.ss.cells)) # the number of sampled substrata per stratum
	
	# Plot showing which substratawere sampled
	# dev.new(height=3.5, width=2.5+0.95)
	# par(mar=c(2,2,0.25,0.1))
	# plot(sub.stratum)
	# image((obs.ss), add=TRUE, col=adjustcolor("black", 0.25), axes=FALSE, xlab="", ylab="")
	
	ss.key <- sampleStratified(sub.stratum, n.ss)[,1]
	visit.chance <- values(obs.ss)
	visit.chance[is.na(visit.chance)] <- 0
	visit.chance <- aperm(array(visit.chance[ss.key], dim=c(n.ss, grid.w*grid.h)), 2:1)
	
	# =================
	# = Detectability =
	# =================
	# Baseline detectability
	if(missing(base.chance) | length(base.chance)!=ns | any(base.chance<0) | any(base.chance>1)){
		if(length(base.chance)!=ns | any(base.chance<0) | any(base.chance>1)){
			warning("Discarding supplied base.chance, using default. Number of chances needs to equal number of species, and all chances between 0 and 1.")
		}
		base.chance <- plogis(rnorm(ns))#runif(ns, 0.2, 0.8) # baseline detectability; specific to species, not to place or time
	}
	
	tax.chance <- matrix(1, nrow=dims["grid.t"], ncol=dims["ns"]) # start but giving each species 100% chance of being ID'd
	if(missing(t.noID) | !all.equal(dim(tax.chance),dim(t.noID))){
		t.noID <- 1
		if(!all.equal(dim(tax.chance),dim(t.noID))){warning("Incorrect dim for t.noID; set to 1")}
	}
	tax.chance[,] <- t.noID # now this is really more of a time & species-specific detectability
	
	# Total detectabilty
	detectability <- base.chance * t(tax.chance) # thus the detectability of a species (given that it is present, and given that the spot is sampled) is the product of its baseline detectability and whether or not it is being ID'd
	

	# =========================
	# = Detectability Outcome =
	# =========================
	# The Observation Outcome Given Presence and Sampling
	
	# Reformat chance of observation (which is just detectability)
	detect.chance0 <- aperm(array(c(detectability), dim=c(dims["ns"], dims["grid.t"], ncell(grid.X))),c(3,1,2)) # reformat
	p0 <- apply(detect.chance0, c(2,3), rep, each=n.ss) # expand (rep) based on number of substrata; use this later to calculate p, the probability of detection (to conform with msom)
	detect.chance <- unlist(apply(p0, 3, list),F,F) # reformat
	
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
	Z.obs0 <- simplify2array(lapply(obs, values))
	tr <- sum(apply(Z.obs0, c(2), function(x)any(!is.na(x)&x==1)))
	rich.true <- colSums(apply(x, c(2,3), function(x)any(!is.na(x))))
	rich.obs <- colSums(apply(Z.obs0, c(2,3), function(x)any(!is.na(x)&x==1)))
	s <- summary(rich.obs)
	s2 <- summary(c(apply(Z.obs0, c(1,3), function(x)sum(!is.na(x)&x==1)))) # is per-substratum, not per stratum
	
	
	
	# Finish calculating p, probability of detection
	p <- array(NA, dim=c(n.ss, grid.h*grid.w, ns, grid.t))
	p[] <- p0
	p <- aperm(p, c(2,1,3,4))
	p[] <- c(p) * c(visit.chance)
	
	# Finish calculating Z.obs, presence/ absence at each j,s,t (no substrat [k])
	# > str(visit.chance) # structure before the last step which overwrites
	#  num [1:315] 0 0 1 1 1 1 1 0 1 1 ...
	# > str(visit.chance) # structure after last step
	#  num [1:35, 1:9] 0 1 1 1 1 1 1 1 1 0 ...
	split2jk <- function(x.ss){
		# Note on ordering of dimensions:
		# order is substrtaa, the width, then height
		# for Z.obs as of 2015-08-21
		# aggregate(orderD1(x.ss, ss.key)[,1,1], by=list(ss=rep(1:54, each=4)), sd) # should show all 0's when x.ss is identical in all substrata; otherwise, these sd's should be lower than the case where instead of `each=4` you had `times=4`
		array(orderD1(x.ss, ss.key), dim=c(n.ss, grid.w*grid.h, ns, grid.t))
	} 
	X.obs <- aperm(split2jk(Z.obs0),c(2,1,3,4))
	Z.obs <- apply(X.obs,c(1,3,4),max, na.rm=T)
	
	
	# Assign attributes
	attr(x, "obs") <- obs
	attr(x, "X.obs") <- X.obs
	attr(x, "Z.obs") <- Z.obs
	attr(x, "p") <- p
	attr(x, "n.haul") <- n.haul
	attr(x, "obs.params") <- list(base.chance=base.chance, tax.chance=tax.chance)
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



