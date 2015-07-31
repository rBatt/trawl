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
load("./trawl/Results/Simulation/sim.basic.RData")

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



taxChance <- as.integer(unlist(lapply(big.out.obs, function(x)sapply(attributes(x)$obs.params[[3]],min)),F,F)>0)

R <- cbind(R, taxChance=taxChance)


dev.new(width=3.5, height=6)
par(mfrow=c(3,1), mar=c(2.5,2.5,0.1,0.1), cex=1, ps=9, mgp=c(1.5, 0.2, 0), tcl=-0.15)
boxplot(rich.true~taxChance, data=R, ylab="True Richness")
boxplot(rich.obs~taxChance, data=R, ylab="Observed Richness")
boxplot(rich.est~taxChance, data=R, ylab="Estimated Richness", xlab="% Chance of Being ID'd")


