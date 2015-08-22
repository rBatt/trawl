
# Colonize a grid with species given the tempereatures on the grid, the densities (from density() ) of those species across a wide range of temperatures, and the mean biomass of those species.
# Function can be applied to 1 species or many species. If many, the length of bio.mu should be of length n.spp, where n.spp is the number of species, and spp.dens.temps should be a list of length n.spp of densities. `temps` would remain unchanged.
# The function returns a list with 3 elements:
	 # `suit.pers` is the probability that each element of `temps` is suitable for the species, i.e., the suitability of persistence (not a great name, I know)
	 # `spp.pres` is NA or 1, where NA means the species is absent, and 1 means the species is present (rbinom(size=1, prob=suit.pers) realization); NA is used to be consistent with the conventions of the final element of the list, which is biomass, and which might be modeled on a log() scale
	 # `spp.bio` are biomasses of the species. These biomasses are NA if the species is not present (i.e., an NA in spp.pres), and otherwise are equal to the `bio.mu` value associated with the species, adjusted up or down (additively) according to the probability of suitability. The adjustment is between -0.5 and 0.5, depending on p.suit. Biomasses are return as NA as opposed to 0 so that the biomass is unambiguous in the event that the bio.mu supplied is on a log() scale (exp(0)==1).
#
# temps <- values(subset(grid.X, 1))
# spp.dens.temps <- S.dens.X
# bio.mu <- spp.bio.mu
# relative=FALSE
# probs=NULL
colonize <- function(temps, spp.dens.temps, bio.mu, relative=FALSE, probs=NULL){
	
	Nv <- length(temps)
	
	if(length(bio.mu)==1){
		# Probability suitable
		if(!is.null(probs)){
			p.suit <- probs
		}else{
			p.suit <- dsample(spp.dens.temps, temps, relative=relative)
		}
		
		# Binary Presence/Absence
		pres <- c(NA,1)[1+rbinom(n=Nv, size=1, prob=p.suit)]
	
		# Species Biomass
		bio <- pres * rep(bio.mu, each=Nv) + (p.suit-0.5)
	
	}else{
	
		# Number of Species
		n.spp <- length(bio.mu)
		
		# Matricies to hold multi-species output
		p.suit <- matrix(NA, nrow=Nv, ncol=n.spp) # environmental suitability to species persistance
		pres <- matrix(NA, nrow=Nv, ncol=n.spp) # binary; species presence/ absence
		bio <- matrix(NA, nrow=Nv, ncol=n.spp) # biomass (log units)
		
		# Probability suitable
		if(!is.null(probs)){
			p.suit[] <- probs
		}else{
			p.suit[] <- sapply(X=spp.dens.temps, FUN=dsample, temps, relative=relative)
		}
		
		# Binary Presence/Absence
		# set.seed(1)
		pres[] <- c(NA,1)[1+rbinom(n=n.spp*Nv, size=1, prob=p.suit)]
	
		# Species Biomass
		bio[] <- pres * rep(bio.mu, each=Nv) + (p.suit-0.5)
		
	}
	
	# Return the following:
	out <- list(
		suit.pers=p.suit,
		spp.pres=pres,
		spp.bio=bio
	)
	
	return(out)
	
}


# par(mfrow=c(6,6), mar=c(1,1,0.1,0.1), cex=1, ps=8, mgp=c(0.5,0.1,0), tcl=-0.1)
# lim <- c(0,1)
# xv <- seq(-3,3,length.out=100)
# gen1 <- function(){dlogis(xv)}
# gen2 <- function(){dsample(plogis(rnorm(1E1)), xv)}
# replicate(36,{
# 	plot(gen1(), gen2(), xlim=lim, ylim=lim, xlab="", ylab="", pch=20, cex=0.5, col=adjustcolor("black",alpha.f=0.6)); abline(a=0,b=1)
# })
