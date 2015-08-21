

all.same <- function(x){
    abs(max(x) - min(x)) < 8.881784e-16 # number is (.Machine$double.eps)*4 on my rMBP
}

rich.cov <- function(data, covs, cov.precs, nameID, nzeroes=100, nChains=3, nIter=2E3, nThin=NULL, save.out.dir, save.fit.cov.dir, Save=TRUE){
	
	if(missing(save.out.dir)){
		save.out.dir <- "trawl/Results/Richness/msomCov/msomCov.smry/"
	}
	if(missing(save.fit.cov.dir)){
		save.fit.cov.dir <- "trawl/Results/Richness/msomCov/msomCov.full/"
	}
	
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
	maxK <- dim(data)[2]

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
			# p=array(plogis(-1E1), dim=c(dim(data)[1],maxK,nSpp+nzeroes)),
			# specify initial for bernoulli outcome; see: http://mbjoseph.github.io/blog/2013/02/24/com_occ/
			Z = apply(Xaug1, c(1,3), max, na.rm=TRUE)
		)	
	}

	# Generate inital values
	sp.inits <- list()
	for(i in 1:nChains){
		sp.inits[[i]] <- make.inits()
	}


	# Ensure that NA's are at the end of the
	# columnar index before passing into JAGS model
	# so that looping through 1:nK[j] never indexes an NA
	# unfortuneately this means that I'll be jumbling the substrata.
	# But that's something I'll just have to correct on the other end, I suppose
	# (I can recalculate k.OK directly from the input data, so I'm not even
	# going to bother returning it from this function)
	k.OK <- t(apply(data[,,1],1,function(x)c(which(is.finite(x)),which(is.na(x)))))
	for(j in 1:dim(data)[1]){
			data[j,,] <- data[j,k.OK[j,],]
	}
	

	# ===================================================
	# = Load data, define parameters, choose model file =
	# ===================================================
	# These things need to change based on whether or not temperature data are available
	if(!all(is.na(covs[[1]]))){
		# Model File
		modelFile <- "msom.cov.txt"
		
		# Parameters to Trace
		# sp.params <- c("N", "omega", "Nsite", "Z", "u.a0", "v.a0", "a1", "a2", "a3", "a4")
		sp.params <- c("N", "omega", "Nsite", "Z", "w", "u.a0", "v.a0", "a1", "a2", "a3", "a4")
		
		# Data
		sp.data <- list(
			n=nSpp, 
			nzeroes=nzeroes, 
			J=nStrat, 
			K=nK,
			maxK=maxK,
			# k.OK=k.OK,
			X=Xaug1, 
			temp.mu=covs[[1]], 
			temp.prec=cov.precs[[1]], 
			depth.mu=covs[[2]], 
			depth.prec=cov.precs[[2]]
		)
	}else{
		if(all.same(cov.precs[[2]])){
			# Model File
			modelFile <- "msom.1cov.txt"
		
			# Parameters to Trace
			# sp.params <- c("N", "omega", "Nsite", "Z", "u.a0", "v.a0", "a3", "a4")
			sp.params <- c("omega", "Z", "w", "p", "psi", "u.a0", "v.a0", "a3", "a4", "mu.u.a0", "mu.v.a0","tau.u.a0","tau.v.a0")
		
			#Data
			sp.data <- list(
				n=nSpp, 
				nzeroes=nzeroes, 
				J=nStrat, 
				K=nK,
				# maxK=maxK,
				# k.OK=k.OK,
				X=Xaug1,
				cov1=covs[[2]]
			)
		}else{
			# Model File
			modelFile <- "msom.cov.noTemp.txt"
		
			# Parameters to Trace
			# sp.params <- c("N", "omega", "Nsite", "Z", "u.a0", "v.a0", "a3", "a4")
			sp.params <- c("N", "omega", "Nsite", "Z", "w", "p", "psi", "u.a0", "v.a0", "a3", "a4")
		
			#Data
			sp.data <- list(
				n=nSpp, 
				nzeroes=nzeroes, 
				J=nStrat, 
				K=nK,
				maxK=maxK,
				# k.OK=k.OK,
				X=Xaug1,
				depth.mu=covs[[2]], 
				depth.prec=cov.precs[[2]]
			)
		}
		
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
		# working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
		working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
	)
	
	
	out <- list(mean=fit.cov$BUGSoutput$mean, median=fit.cov$BUGSoutput$median)

	
	
	# ========================
	# = Save Objects to Disk =
	# ========================
	if(Save){
		save(out, file=paste0(getwd(), "/", save.out.dir, nameID, "_smry", ".RData"))
		save(fit.cov, file=paste0(getwd(), "/", save.fit.cov.dir, nameID, "_full", ".RData"))
		# save(out, file=paste0("./trawl/Results/Richness/msomCov/msomCov.smry/",nameID,"_smry",".RData"))
		# save(fit.cov, file=paste0("./trawl/Results/Richness/msomCov/msomCov.full/",nameID,"_full",".RData"))
	}
	
	
	
	# ======================================
	# = Return the Summary for Convenience =
	# ======================================
	return(out)

}
