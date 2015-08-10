
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
load("./trawl/Results/Simulation/sim.basic.small.RData")
# load("./trawl/Results/Simulation/sim.basic.RData")


# ======================================================
# = Compare True, Obs, Est Richness for Tax ID-ability =
# ======================================================
simR0 <- lapply(big.out.obs, function(x)as.matrix(attr(x, "richness")))
simR <- simR0[[1]]
for(i in 2:length(simR0)){
	simR <- rbind(simR, simR0[[i]])
}
estR <- sapply(sim.rich.cov, function(x)x$N)

R <- data.frame(simR, rich.est=estR)

# taxChance <- as.integer(unlist(lapply(big.out.obs, function(x)sapply(attributes(x)$obs.params[[3]],min)),F,F)>0)
taxChance <- c(sapply(big.out.obs, function(x)rowMeans((attributes(x)$obs.params)$tax.chance)))


mu.p0 <- lapply(sim.rich.cov, function(x)t(c(plogis(x$v.a0))*t(x$Z)))
mu.p <- apply(sapply(mu.p0, function(x)apply(x,2,pTot)),2,sum)


# Not sure which Z I should be comparing to the richness realized in the process simulation
Z <- apply(sapply(sim.rich.cov, function(x)apply(x$Z[,1:ns],2,pTot)),2,sum)
# Z <- apply(sapply(sim.rich.cov, function(x)apply(x$Z,2,pTot)),2,sum)


R[,"taxChance"] <- taxChance
R[,"mu.p"] <- mu.p
R[,"Z"] <- Z
R[,"year"] <- 1:grid.t
R[,"rep"] <- rep(1:n.obs.reps, each=grid.t)

dev.new(width=3.5, height=5)
# pdf("~/Desktop/richnessAssessment1.pdf", width=3.5, height=5)
tC.names <- round(unique(taxChance),2)
par(mfrow=c(3,2), mar=c(2.1,2.0,0.1,0.1), cex=1, ps=9, mgp=c(1.15, 0.2, 0), tcl=-0.15, oma=c(0,0,1,0))
boxplot(rich.true~taxChance, data=R, ylab="Realized Richness", names=tC.names)
mtext("True ", side=3, line=0.1, font=2)
boxplot(Z~taxChance, data=R, ylab="", names=tC.names)
mtext("Estimated", side=3, line=0.1, font=2)
boxplot(rich.obs~taxChance, data=R, ylab="Observed Richness", names=tC.names)
boxplot(mu.p~taxChance, data=R, ylab="", names=tC.names)
plot(0,0, xaxt="n",yaxt="n", ylab="Richness Asymptote", xlab="", pch="?")
boxplot(rich.est~taxChance, data=R, ylab="", xlab="", names=tC.names)
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



# ============================
# = Estimated and True Prams =
# ============================
proc.params.true <- attr(big.out.obs[[1]], "proc.params")[[2]]
proc.params.hat <- lapply(sim.rich.cov, function(x)data.frame(x$mean[c("u.a0","a3","a4")]))

# terrible
# for(j in 1:3){
# 	p.j <- c("u.a0","a3","a4")[j]
# 	# plot(proc.params.true[,"u.a0"], proc.params.hat[[1]][1:ns,"u.a0"])
# 	plot(plogis(proc.params.true[,p.j]), plogis(proc.params.hat[[1]][1:ns,p.j]))
# 	for(i in 2:length(proc.params.hat)){
# 		points(plogis(proc.params.true[,p.j]), plogis(proc.params.hat[[i]][1:ns,p.j]))
# 	}
# }



plot(plogis(proc.params.true[,p.j]), plogis(proc.params.hat[[1]][1:ns,p.j]))
for(i in 2:length(proc.params.hat)){
	points(plogis(proc.params.true[,p.j]), plogis(proc.params.hat[[i]][1:ns,p.j]))
}





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


# This plot is interesting because it shows that the only exceptionally high or exceptionally
# low chances to be observed occurs fo rthe species that were never observed;
# i.e., this says that if you didn't observe it, it just takes on the mean.
# that's reasonable, I guess; but I also would think that the things that 
# were never observed could also be things that had a low chance of observability;
# but they could also have just a low chance of actually being present.
# So I suppose in the end it just doesn't get informed, and reverts to the mean?
plot(plogis(sim.rich.cov[[1]]$mean$v.a0)[1:200])

#
# x <- sim.rich.cov[[1]]$mean
# covX <- simCov[[1]]
# get.psi <- function(x, covX){
# 	a0 <- x$u.a0
# 	a3 <- x$a3
# 	a4 <- x$a4
# 	Z <- x$Z
# 	curve0 <- matrix(NA, nrow=length(covX), ncol=length(a0)+1)
# 	psi <- curve0[,-1]
# 	curve0[,1] <- covX
# 	for(i in 1:length(a0)){
# 		psi0 <- a3[i]*covX + a4[i]*covX^2
# 		curve0[,i+1] <- plogis(psi0)
# 		psi[,i] <- plogis(a0[i] + psi0)
# 	}
# }
#
# ref.g <- unstack(grid.X)
# ref.g.i <- (1:length(sim.rich.cov))%%grid.t
# ref.g.i[ref.g.i==0] <- grid.t
# grid.in <- list()
# for(i in 1:length(sim.rich.cov)){
# 	grid.in[[i]] <- ref.g[[ref.g.i[i]]]
# }


# ==================================
# = Compare True and Estimated Psi =
# ==================================
# Estimated psi
psi.hat.mega <- lapply(sim.rich.cov, function(x)x$mean$psi)
psi.hat.mu <- lapply(psi.hat.mega, function(x)apply(x, 2, mean)) # averages over space


# True psi
# values(subset(grid.X, 1))
v.grid.X <- values(grid.X)
get.psi <- function(x){
	list(sapply(attr(big.out.obs[[1]], "spp.densX"), dsample, x=x, relative=TRUE))
}
psi.true.mega <- unlist(apply(v.grid.X, 2, get.psi),F,F)
psi.true.mu <- lapply(psi.true.mega, function(x)apply(x, 2, mean)) # averages over space


true.vec <- psi.true.mu[[1]]
hat.vec <- psi.hat.mu[[1]][1:ns]
plot(psi.true.mu[[1]], psi.hat.mu[[1]][1:ns])
for(i in 2:(n.obs.reps*grid.t)){
	true.i <- i%%grid.t
	if(true.i==0){true.i <- 1}
	points(psi.true.mu[[true.i]], psi.hat.mu[[i]][1:ns])
	
	true.vec <- c(true.vec,psi.true.mu[[true.i]])
	hat.vec <- c(hat.vec, psi.hat.mu[[i]][1:ns])
}


# ================================================================
# = Compare Site-specific True, Observed, and Estimated Richness =
# ================================================================

# True Space-Time Richness
# Simple, b/c no replicates, and no substrata
Nsite.true <- apply(attributes(big.out.obs[[1]])$spp.bio, c(1,3), function(x)sum(!is.na(x)))
Nsite.true <- aperm(array(Nsite.true, dim=c(grid.w,grid.h,grid.t)), c(2,1,3))



# Observed Space-Time Richness
# More complicated b/c first have to aggregate to remove substrata (take max for each spp-year-strat)
# Then have to sum over spp (same as for True)
# Then have to average over replicates
get.rich.obs <- function(x){
	x2 <- aggregate(x, fact=3, max)
	matrix(values(stackApply(x2, rep(1,nlayers(x2)), sum)),byrow=T, nrow=nrow(x2),ncol=ncol(x2))
	# stackApply(x2, rep(1,nlayers(x2)), sum)
}
Nsite.obs <- lapply(big.out.obs, function(x){lapply(attributes(x)$obs, get.rich.obs)})
Nsite.obs.array <- simplify2array(lapply(Nsite.obs, simplify2array)) # 1=height,2=width,3=grid.t,4=n.obs.reps
Nsite.obs.mu <- apply(Nsite.obs.array, 1:3, mean)


# Estimated Space-Time Richness
Nsite.msom0 <- lapply(sim.rich.cov, function(x)matrix(x$mean$Nsite,nrow=grid.h,byrow=TRUE))
Nsite.msom <- array(unlist(Nsite.msom0), dim=c(grid.h,grid.w,grid.t,n.obs.reps))
Nsite.msom.mu <- apply(Nsite.msom, 1:3, mean)




my.image <- function(x, smplt=c(0.85,0.88, 0.2,0.8), bgplt=c(0.05,0.82,0.15,0.95), xaxt="n", yaxt="n", ...){
	image.plot(t(x), smallplot=smplt, bigplot=bgplt, axis.args=axargs, xaxt=xaxt, yaxt=yaxt, ...)
}


for(j in 1:2){
	zlim.true <- range(Nsite.true) # this is true, observed will be less, but idk about estimated, b/c could be overestimate
	zlim.obs <- range(Nsite.obs.mu)
	zlim.msom <- range(Nsite.msom.mu)
	ylim <- c(1.085,-0.085)
	
	if(j==2){
		zlim.true <- zlim.obs <- zlim.msom <- range(c(Nsite.true,Nsite.obs.mu,Nsite.msom.mu))
	}


	smplt <- c(0.85,0.88, 0.2,0.8)
	bgplt <- c(0.05,0.82,0.15,0.95)
	axargs <- list(mgp=c(0.5,0.15,0))
	
	# dev.new(width=10, height=3.5)
	if(j==1){
		png("./trawl/Figures/Simulation/Nsite.compare.maps.png",width=10,height=3.5,units="in",res=150)
	}else{
		png("./trawl/Figures/Simulation/Nsite.compare.maps_scaled.png",width=10,height=3.5,units="in",res=150)
	}
	
	par(mfcol=c(3,grid.t), mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1, oma=c(0,0.5,0,0))

	# Plot True Richness
	for(i in 1:grid.t){
		my.image(Nsite.true[,,i], ylim=ylim, zlim=zlim.true)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)} # this is so weird ... why do I have to do this?
		if(i==1){text("True",x=-0.3,y=0.5, xpd=NA,srt=90, cex=1.25)}
	}

	# Plot obs richness
	for(i in 1:grid.t){
		my.image(Nsite.obs.mu[,,i], zlim=zlim.obs, ylim=ylim)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)}
		if(i==1){text("Observed",x=-0.3,y=0.5, xpd=NA,srt=90, cex=1.25)}
	}


	# Plot msom estimates of richness
	for(i in 1:grid.t){
		my.image(Nsite.msom.mu[,,i], zlim=zlim.msom, ylim=ylim)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)}
		if(i==1){text("MSOM",x=-0.3,y=0.5, xpd=NA,srt=90, cex=1.25)}
		# text(i,x=0.5,y=-0.1, xpd=T,srt=90, cex=1.25)
		text("",x=0.5,y=1.15, xpd=T,srt=0, cex=1.25)
		text(paste0("t=",i),x=0.5,y=1.15, xpd=NA,srt=0, cex=1.25)
	}
	dev.off()
}


dev.new(width=11, height=2.5)
# png("./trawl/Figures/Simulation/Nsite.compare.scatter.png",width=11,height=2.5,units="in",res=150)
par(mfcol=c(2,grid.t), mar=c(0.5,0.5,0.15,0), ps=6, mgp=c(1,0.0,0), tcl=-0.15, cex=1, oma=c(1,0.65,0.25,0))
ylim <- range(c(Nsite.true,Nsite.obs.mu,Nsite.msom.mu))
col <- adjustcolor("black",alpha.f=0.25)
for(i in 1:grid.t){
	yaxt <- ifelse(i==1, "s", "n")
	plot(Nsite.true[,,i], Nsite.obs.mu[,,i], xaxt="n", yaxt=yaxt, xlab="", ylab="", ylim=ylim, xlim=ylim, pch=20,col=col)
	axis(side=1, labels=F)
	abline(a=0,b=1)
	if(i==1){mtext("Observed",side=2,line=0.5, font=2)}
	mtext(paste0("t=",i),side=3,line=-0.12, xpd=NA)
	
	plot(Nsite.true[,,i], Nsite.msom.mu[,,i], xaxt="s", xlab="", yaxt=yaxt, ylab="", ylim=ylim, xlim=ylim, pch=20,col=col)
	# axis(side=1, labels=F)
	abline(a=0,b=1)
	if(i==1){mtext("MSOM",side=2,line=0.5, font=2)}
		
	
}
mtext("True", side=1, line=0, outer=TRUE, font=2, cex=1)
# dev.off()
