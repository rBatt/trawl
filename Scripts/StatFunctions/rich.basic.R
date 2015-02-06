


rich.basic <- function(data, nzeroes=100, nChains=3, nIter=2E3, nThin=NULL, doP=FALSE){
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
	make.inits <- function(){
		omegaGuess <- runif(1, nSpp/(nSpp+nzeroes), 1)
		psi.meanGuess <- runif(1, 0.25, 1)
		list(
			omega=omegaGuess,
			w=c(rep(1, nSpp), rbinom(nzeroes, size=1, prob=omegaGuess)),
	        u=rnorm(nSpp+nzeroes), 
			v=rnorm(nSpp+nzeroes),
			# the change below necessary to avoid invalid initials; see: http://mbjoseph.github.io/blog/2013/02/24/com_occ/
			Z = apply(Xaug1, c(1,3), max, na.rm=TRUE) #matrix(rbinom((n+nzeroes)*J, size=1, prob=psi.meanGuess), nrow=J, ncol=(n+nzeroes))
		)	
	}

	# Generate inital values
	if(!doP){
		sp.inits <- list()
		for(i in 1:nChains){
			sp.inits[[i]] <- make.inits()
		}
	}else{
		sp.inits[[i]] <- list(make.inits())
	}

	



	# ====================================
	# = Define data to be loaded to jags =
	# ====================================
	sp.data <- list(n=nSpp, nzeroes=nzeroes, J=nStrat, K=nK, X=Xaug1)
	if(doP){
		n=nSpp
		# nzeroes=nzeroes
		J=nStrat
		K=nK
		X=Xaug1
		sp.data <- list("n","nzeroes","J","K","X")
	}


	# =======================================
	# = Define parameters for jags to track =
	# =======================================
	sp.params <- c("Z","u", "v", "mu.u", "mu.v", "tau.u", "tau.v", "omega", "N")


	# =============
	# = Run model =
	# =============
	if(!doP){
		fit.basic <- jags(
			data=sp.data,
			inits=sp.inits,
			parameters.to.save=sp.params,
			model.file="msom.basic.txt",
			n.chains=nChains,
			n.iter=nIter,
			n.thin=nThin,
			working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
		)
	}


	if(doP){
		fit.basic <- do.call(
			jags.parallel,
			list(
				data=sp.data,
				inits=sp.inits,
				parameters.to.save=sp.params,
				model.file="msom.basic.txt",
				n.chains=nChains,
				n.iter=nIter,
				n.thin=nThin,
				working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
			)
		)
	}

	# return(list(summary.jags(fit.basic, doPlot=FALSE, doPanels=FALSE), fit.basic))
	return(summary.jags(fit.basic, doPlot=FALSE, doPanels=FALSE))

}