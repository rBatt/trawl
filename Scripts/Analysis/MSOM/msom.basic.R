

# =================
# = Load packages =
# =================
library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)
library(R2jags)


# ===========
# = Options =
# ===========
n0s <- 100
nChains <- 3
nIter <- 1E3


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# ======================
# = Load MSOM Data set =
# ======================
load("./trawl/Data/MSOM/basic.dat.RData")


# =====================================
# = Trim to MSOM data example (md.eg) =
# =====================================
md.eg.ind <- msom.dat[[2]][s.reg=="neus"&year=="2012", as.numeric(num)] # identifying the index of a choice region and year
md.eg <- msom.dat[[1]][[md.eg.ind]] # subset to that choice


# =====================
# = Define Dimensions =
# =====================
# Will eventually be used in loops in jags
nStrat <- dim(md.eg)[1] # number of strata (J)
nK <- apply(md.eg[,,1], 1, function(x)sum(!is.na(x))) # needs to be a vector to avoid jags seeing an NA (K)
nSpp <- as.numeric(dim(md.eg)[3]) # count the number of species in that choice (n)


# ====================
# = Add 0's to array =
# ====================
x0s <- apply(md.eg, 1:2, sumna) # summing up the wtcpue of all species in a given stratum-rep; only using to infer if sampled
x0s[!is.na(x0s)] <- 0 # Create template matrix for unobserved spp. Changed all sampled location to 0.

Xaug <- array(-0.1, dim=dim(md.eg)+c(0,0,n0s)) # template array for augmented occurrence matrix
Xaug[,,1:nSpp] <- md.eg # fill in the first part of the array with the actual data
Xaug[,,(nSpp+1:n0s)] <- x0s # fill in the second part of the array with the unobserved matrix


# ===============================
# = Change positive values to 1 =
# ===============================
Xaug1 <- ceiling(pmin(Xaug, 1)) # only do this for the occurrence model


# ==================
# = Initial values =
# ==================
# This function needs to be created so that invalid initial values aren't used
make.inits <- function(){
	omegaGuess <- runif(1, nSpp/(nSpp+n0s), 1)
	psi.meanGuess <- runif(1, 0.25, 1)
	list(
		omega=omegaGuess,
		w=c(rep(1, nSpp), rbinom(n0s, size=1, prob=omegaGuess)),
        u=rnorm(nSpp+n0s), 
		v=rnorm(nSpp+n0s),
		# the change below necessary to avoid invalid initials; see: http://mbjoseph.github.io/blog/2013/02/24/com_occ/
		Z = apply(Xaug1, c(1,3), max, na.rm=TRUE) #matrix(rbinom((n+nzeroes)*J, size=1, prob=psi.meanGuess), nrow=J, ncol=(n+nzeroes))
	)	
}

# Generate inital values
sp.inits <- list()
for(i in 1:nChains){
	sp.inits[[i]] <- make.inits()
}
omega <- sp.inits[[1]][1]
w <- sp.inits[[1]][2]
u <- sp.inits[[1]][3]
v <- sp.inits[[1]][4]
Z <- sp.inits[[1]][5]

sp.inits2 <- list(spp.inits[[i]]) #list(names(sp.inits[[1]]))


# ====================================
# = Define data to be loaded to jags =
# ====================================
sp.data <- list(n=nSpp, nzeroes=n0s, J=nStrat, K=nK, X=Xaug1)
n=nSpp
nzeroes=n0s
J=nStrat
K=nK
X=Xaug1
sp.data2 <- list("n","nzeroes","J","K","X")


# =======================================
# = Define parameters for jags to track =
# =======================================
sp.params <- c("u", "v", "mu.u", "mu.v", "tau.u", "tau.v", "omega", "N")


# =============
# = Run model =
# =============
fit.basic <- jags(
	data=sp.data,
	inits=sp.inits,
	parameters.to.save=sp.params,
	model.file="msom.basic.txt",
	n.chains=nChains,
	n.iter=nIter,
	working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
)


fit.basic <- do.call(
	jags.parallel,
	list(
		data=sp.data2,
		inits=sp.inits[1],
		parameters.to.save=sp.params,
		model.file="msom.basic.txt",
		n.chains=nChains,
		n.iter=nIter,
		working.directory=paste0(getwd(),"/","trawl/Scripts/Analysis/JAGS")
	)
)


# ===========
# = Summary =
# ===========
smry.basic <- summary.jags(fit.basic)