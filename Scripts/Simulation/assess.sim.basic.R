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


# ======================================================
# = Compare True, Obs, Est Richness for Tax ID-ability =
# ======================================================
simR0 <- lapply(big.out.obs, function(x)as.matrix(attr(x, "richness")))
simR <- simR0[[1]]
for(i in 2:length(simR0)){
	simR <- rbind(simR, simR0[[i]])
}
estR <- sapply(sim.rich.cov, function(x)x$mean$N)

R <- data.frame(simR, rich.est=estR)

# taxChance <- as.integer(unlist(lapply(big.out.obs, function(x)sapply(attributes(x)$obs.params[[3]],min)),F,F)>0)
taxChance <- c(sapply(big.out.obs, function(x)rowMeans((attributes(x)$obs.params)$tax.chance)))


mu.p0 <- lapply(sim.rich.cov, function(x)t(c(plogis(x$mean$v.a0))*t(x$mean$Z)))
mu.p <- apply(sapply(mu.p0, function(x)apply(x,2,pTot)),2,sum)


# Not sure which Z I should be comparing to the richness realized in the process simulation
# Z <- apply(sapply(sim.rich.cov, function(x)apply(x$mean$Z[,1:ns],2,pTot)),2,sum)
Z <- apply(sapply(sim.rich.cov, function(x)apply(x$mean$Z,2,pTot)),2,sum)


R[,"taxChance"] <- taxChance
R[,"mu.p"] <- mu.p
R[,"Z"] <- Z
R[,"year"] <- 1:grid.t
R[,"rep"] <- rep(1:n.obs.reps, each=grid.t)

dev.new(width=3.5, height=5)
# pdf("~/Desktop/richnessAssessment1.pdf", width=3.5, height=5)
par(mfrow=c(3,2), mar=c(2.1,2.0,0.1,0.1), cex=1, ps=9, mgp=c(1.15, 0.2, 0), tcl=-0.15, oma=c(0,0,1,0))
boxplot(rich.true~taxChance, data=R, ylab="Realized Richness")
mtext("True ", side=3, line=0.1, font=2)
boxplot(Z~taxChance, data=R, ylab="")
mtext("Estimated", side=3, line=0.1, font=2)
boxplot(rich.obs~taxChance, data=R, ylab="Observed Richness")
boxplot(mu.p~taxChance, data=R, ylab="")
plot(0,0, xaxt="n",yaxt="n", ylab="Richness Asymptote", xlab="", pch="?")
boxplot(rich.est~taxChance, data=R, ylab="", xlab="")
# mtext(paste("Number Simulated Species =",ns), side=3, line=0, outer=TRUE, font=2)
mtext("Fraction Capable of Being ID'd", side=1, line=-1, outer=TRUE)
# dev.off()


# all.same <- function(x){
#     abs(max(x) - min(x)) < 8.881784e-16 # number is (.Machine$double.eps)*4 on my rMBP
# }
#
# pTot <- function(p, n=1){
#     ln <- length(n)
#     if(ln>1 | !all.same(p)){
#         # stopifnot(ln == length(p))
#         pn <- 1-((1-p)^n)
#     }else{
#         pn <- p
#     }
#
#     1-prod(1 - pn)
#
# }

# ===============
# = Time Series =
# ===============
R.mu <- function(name){
	aggregate(structure(list(R[,name]),.Names=name),by=list(year=R[,"year"]),mean)
}
R.ylim <- function(name){
	range(R[,name])
}


dev.new(width=3.5, height=5)
# pdf("~/Desktop/richnessAssessment1.pdf", width=3.5, height=5)
par(mfrow=c(3,2), mar=c(2.1,2.0,0.1,0.1), cex=1, ps=9, mgp=c(1.15, 0.2, 0), tcl=-0.15, oma=c(0,0,1,0))

# rich.true and Z
rt.z.ylim <- range(c(R.ylim("rich.true"),R.ylim("Z")))
plot(R[R[,"rep"]==1,c("year","rich.true")], type="l", lwd=2, col="blue", ylim=rt.z.ylim)

plot(R.mu("Z"), type="n", ylim=rt.z.ylim)
for(i in 2:n.obs.reps){lines(R[R[,"rep"]==i,c("year","Z")], col="gray50")}
lines(R.mu("Z"), type="l", lwd=2, col="blue")

# rich.obs and mu.p
ro.mup.ylim <- range(c(R.ylim("rich.obs"),R.ylim("mu.p")))
plot(R.mu("rich.obs"), type="n", ylim=ro.mup.ylim)
for(i in 2:n.obs.reps){lines(R[R[,"rep"]==i,c("year","rich.obs")], col="gray50")}
lines(R.mu("rich.obs"), type="l", lwd=2, col="blue")

plot(R.mu("mu.p"), type="n", ylim=ro.mup.ylim)
for(i in 2:n.obs.reps){lines(R[R[,"rep"]==i,c("year","mu.p")], col="gray50")}
lines(R.mu("mu.p"), type="l", lwd=2, col="blue")



plot(R[R[,"rep"]==1,c("year","rich.est")], type="l")

plot(R[R[,"rep"]==1,c("year","mu.p")], type="l")

plot(R[R[,"rep"]==1,c("year","Z")], type="l")






# ============================================
# = Compare True and Estimated Detectability =
# ============================================

spp.detect0 <- lapply(big.out.obs, function(x)sapply(attributes(x)$obs.params[[3]], colMeans))
spp.detect <- array(NA, dim=c(ns, grid.t, length(spp.detect0)))
spp.detect[,,1] <- spp.detect0[[1]]
for(i in 2:length(spp.detect0)){
	spp.detect[,,i] <- spp.detect0[[i]]
}


# TODO THIS WHOLE SECTION IN PROGRESS; TOO TIRED

p.true <- c(apply(spp.detect, c(2,3), mean))
p.hat0 <- lapply(sim.rich.cov, function(x)t(c(plogis(x$mean$v.a0))*t(x$mean$Z)))
p.hat <- apply(sapply(p.hat0, function(x)apply(x,2,pTot)),2,mean)
plot(p.true, p.hat)
abline(a=0, b=1)




x <- sim.rich.cov[[1]]$mean
covX <- simCov[[1]]
get.psi <- function(x, covX){
	a0 <- x$u.a0
	a3 <- x$a3
	a4 <- x$a4
	Z <- x$Z
	curve0 <- matrix(NA, nrow=length(covX), ncol=length(a0)+1)
	psi <- curve0[,-1]
	curve0[,1] <- covX
	for(i in 1:length(a0)){
		psi0 <- a3[i]*covX + a4[i]*covX^2
		curve0[,i+1] <- plogis(psi0)
		psi[,i] <- plogis(a0[i] + psi0)
	}	
}

ref.g <- unstack(grid.X)
ref.g.i <- (1:length(sim.rich.cov))%%grid.t
ref.g.i[ref.g.i==0] <- grid.t
grid.in <- list()
for(i in 1:length(sim.rich.cov)){
	grid.in[[i]] <- ref.g[[ref.g.i[i]]]
}