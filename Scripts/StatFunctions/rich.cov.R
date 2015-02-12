


rich.cov <- function(data, covs, cov.precs, nzeroes=100, nChains=3, nIter=2E3, nThin=NULL){
	if(is.null(nThin)){
		nThin <- max(1, floor((nIter - floor(nIter/2)) / 1000))
	}


	# =================
	# = Load packages =
	# =================
	require(data.table)
	require(R2jags)


	# =====================
	# = Define Dimensions =
	# =====================
	# Will eventually be used in loops in jags
	nStrat <- dim(data)[1] # number of strata (J)
	nK <- apply(data[,,1], 1, function(x)sum(!is.na(x))) # needs to be a vector to avoid jags seeing an NA (K)
	nSpp <- as.numeric(dim(data)[3]) # count the number of species in that choice (n)


	# ====================
	# = Add 0's to array =
	# ====================
	x0s <- apply(data, 1:2, sumna) # summing up the wtcpue of all species in a given stratum-rep; only using to infer if sampled
	x0s[!is.na(x0s)] <- 0 # Create template matrix for unobserved spp. Changed all sampled location to 0.

	Xaug <- array(-0.1, dim=dim(data)+c(0,0,nzeroes)) # template array for augmented occurrence matrix
	Xaug[,,1:nSpp] <- data # fill in the first part of the array with the actual data
	Xaug[,,(nSpp+1:nzeroes)] <- x0s # fill in the second part of the array with the unobserved matrix


	# ===============================
	# = Change positive values to 1 =
	# ===============================
	Xaug1 <- ceiling(pmin(Xaug, 1)) # only do this for the occurrence model


	# ==================
	# = Initial values =
	# ==================
	# This function needs to be created so that invalid initial values aren't used
	make.inits <- function(){ # TODO need to make initial values for covariate parameters (maybe? maybe not ...)
		omegaGuess <- runif(1, nSpp/(nSpp+nzeroes), 1)
		psi.meanGuess <- runif(1, 0.25, 1)
		list(
			omega=omegaGuess,
			w=c(rep(1, nSpp), rbinom(nzeroes, size=1, prob=omegaGuess)),
	        u.a0=rnorm(nSpp+nzeroes), 
			v.a0=rnorm(nSpp+nzeroes),
			# the change below necessary to avoid invalid initials; see: http://mbjoseph.github.io/blog/2013/02/24/com_occ/
			Z = apply(Xaug1, c(1,3), max, na.rm=TRUE) #matrix(rbinom((n+nzeroes)*J, size=1, prob=psi.meanGuess), nrow=J, ncol=(n+nzeroes))
		)	
	}

	# Generate inital values
	sp.inits <- list()
	for(i in 1:nChains){
		sp.inits[[i]] <- make.inits()
	}


	

	

	# ===================================================
	# = Load data, define parameters, choose model file =
	# ===================================================
	# These things need to change based on whether or not temperature data are available
	if(!all(is.na(covs[[1]]))){
		# Model File
		modelFile <- "msom.cov.txt"
		
		# Parameters to Trace
		sp.params <- c("N", "omega", "Nsite", "Z", "u.a0", "v.a0", "a1", "a2", "a3", "a4") # TODO needs to include the other parameters, need to drop paramters a1 and a2 when no temperature data are available
		
		# Data
		sp.data <- list(
			n=nSpp, 
			nzeroes=nzeroes, 
			J=nStrat, 
			K=nK, 
			X=Xaug1, 
			temp.mu=covs[[1]], 
			temp.prec=cov.precs[[1]], 
			depth.mu=covs[[2]], 
			depth.prec=cov.precs[[2]]
		)
	}else{
		# Model File
		modelFile <- "msom.cov.noTemp.txt"
		
		# Parameters to Trace
		sp.params <- c("N", "omega", "Nsite", "Z", "u.a0", "v.a0", "a3", "a4")
		
		#Data
		sp.data <- list(
			n=nSpp, 
			nzeroes=nzeroes, 
			J=nStrat, 
			K=nK, 
			X=Xaug1,
			depth.mu=covs[[2]], 
			depth.prec=cov.precs[[2]]
		)
	}
	

	# =============
	# = Run model =
	# =============
	fit.cov <- jags(
		data=sp.data,
		inits=sp.inits,
		parameters.to.save=sp.params,
		model.file=modelFile,
		n.chains=nChains,
		n.iter=nIter,
		n.thin=nThin,
		working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
	)

	# return(list(summary.jags(fit.cov, doPlot=FALSE, doPanels=FALSE), fit.cov))
	return(fit.cov)

}