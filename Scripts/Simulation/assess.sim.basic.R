# =================
# = Load Packages =
# =================
# Data structure
library(raster)
library(data.table)

# Graphing
library(fields)

# Statistics
library(igraph)
library(R2jags)

# Computing
library(parallel)
library(doParallel)
library(foreach)

# Other
library(rbLib) # library(devtools); install_github("rBatt/rbLib")


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================================
# = Load Full Image of sim.basic.R =
# ==================================
load("./trawl/Results/Richness/sim.basic.RData")

big.out.obs

sim.rich.cov


simR0 <- lapply(big.out.obs, function(x)as.matrix(attr(x, "richness")))
simR <- simR0[[1]]
for(i in 2:length(simR0)){
	simR <- rbind(simR, simR0[[i]])
}
estR <- sapply(sim.rich.cov, function(x)x$mean$N)

R <- data.frame(simR, rich.est=estR)

plot(R[,"rich.obs"], abs(R[,"rich.true"]-R[,"rich.est"]))