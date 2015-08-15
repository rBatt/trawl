

#' ---
#' title: "assess.sim.basic.R"
#' author: "Ryan Batt"
#' date: "2015-08-14"
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#' theme: "journal"
#' ---

# rmarkdown::render("~/Documents/School&Work/pinskyPost/trawl/Scripts/Simulation/assess.sim.basic.R")

#' #Setup
#+ setup, include=TRUE, echo=TRUE
# ================
# = Report Setup =
# ================
library(knitr)
library(rmarkdown)
opts_chunk$set(fig.path = '../../Figures/Simulation/assess.sim.basic/')


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
	setwd("~/Documents/School&Work/pinskyPost/trawl/Scripts/Simulation")
}else{
	setwd("~/Documents/School&Work/pinskyPost/trawl/Scripts/Simulation")
}


# ==================================
# = Load Full Image of sim.basic.R =
# ==================================
# load("./trawl/Results/Simulation/sim.basic.small.RData")
load("../../Results/Simulation/sim.basic.RData")


#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#' 

#' #Simulation Settings
#+ print-sim-msom-info, include=TRUE, echo=TRUE
# Dimensions of the simulation (and basic richness info)
big.out.obs[[1]]
data.frame(t.noID.mus=t.noID.mus, t.noID.sd=t.noID.sd)

 

#' 
# Check the attributes
names(attributes(big.out.obs[[1]]))
str(attributes(big.out.obs[[1]])[c("X.obs","Z","Z.obs","psi","p")])

#'
# Check on the MSOM output
data.frame(nChains=nChains, nIter=nIter, n0s=n0s, nSamples=nSamples)
str(sim.rich.cov[[1]])

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'

#' #Richness Boxplots
#+ richnessBoxplots, fig.width=3.5, fig.height=3.5
# ======================================================
# = Compare True, Obs, Est Richness for Tax ID-ability =
# ======================================================
simR0 <- lapply(big.out.obs, function(x)as.matrix(attr(x, "richness")))
simR <- simR0[[1]]
for(i in 2:length(simR0)){
	simR <- rbind(simR, simR0[[i]])
}

R <- data.frame(simR)
taxChance <- c(sapply(big.out.obs, function(x)rowMeans((attributes(x)$obs.params)$tax.chance)))

mu.p0 <- lapply(sim.rich.cov, function(x)t(c(plogis(x$mean$v.a0))*t(x$mean$Z)))
mu.p <- apply(sapply(mu.p0, function(x)apply(x,2,pTot)),2,sum)

Z <- apply(sapply(sim.rich.cov, function(x)apply(x$mean$Z[,1:ns],2,pTot)),2,sum) # this is right @mtingley

R[,"taxChance"] <- taxChance # note that this averages over all species
R[,"mu.p"] <- mu.p # originally "mu.p", but actually estimated obs richness
R[,"Z"] <- Z # originally "Z", but this is actually estimated richness (N, but w/o the asymptote)
R[,"year"] <- 1:grid.t
R[,"rep"] <- rep(1:n.obs.reps, each=grid.t)


tC.names <- round(unique(taxChance),2)
par(mfrow=c(2,2), mar=c(2.1,2.0,0.1,0.1), cex=1, ps=9, mgp=c(1.15, 0.2, 0), tcl=-0.15, oma=c(0,0,1,0))
boxplot(rich.true~taxChance, data=R, ylab="Realized Richness", names=tC.names)
mtext("True ", side=3, line=0.1, font=2)
boxplot(Z~taxChance, data=R, ylab="", names=tC.names)
mtext("Estimated", side=3, line=0.1, font=2)
boxplot(rich.obs~taxChance, data=R, ylab="Observed Richness", names=tC.names)
boxplot(mu.p~taxChance, data=R, ylab="", names=tC.names)
mtext("Fraction Capable of Being ID'd", side=1, line=-1, outer=TRUE)
#' **Figure.** Old plot, not sure if everything in it is still valid.

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'

#' #Richness Time Series
#+ richnessTimeSeries, fig.width=3.5, fig.height=3.5
# ===============
# = Time Series =
# ===============
R.mu <- function(name){
	aggregate(structure(list(R[,name]),.Names=name),by=list(year=R[,"year"]),mean)
}
R.ylim <- function(name){
	range(R[,name])
}

par(mfrow=c(2,2), mar=c(2.1,2.0,0.1,0.1), cex=1, ps=9, mgp=c(1.15, 0.2, 0), tcl=-0.15, oma=c(0,0,1,0))

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
#' **Figure.** Time series of richness. Gray lines are individual replicates. Blue lines are averages.

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'


#' #Demo: Effect of MSOM Hierarchy on $p$
#+ egHierarchEffect_PsiMu, fig.width=4, fig.height=4

demo.Zobs <- attr(big.out.obs[[1]],"Z.obs")[,,1] # Z.obs for the demo year
demo.p.nobs <- apply(demo.Zobs, 2, sum, na.rm=TRUE) # n detections 4 each species
demo.p.col <- c("black","red")[((demo.p.nobs!=0) + 1)] # red is detected, black is never detected


plot(plogis(sim.rich.cov[[1]]$mean$v.a0)[1:ns], ylab="p (detectability)", col=demo.p.col)
# abline(h=taxChance[1]) # not quite right – but I'm too tired to figure out why wrong
abline(h=plogis(t.noID.mus)[1]) # horizontal line at mean chance
#' **Figure.** Probability of being detected, $p$. Horizontal line is mean probability. Figure only shows results for the first year of the simulation/ observation, and only 1 replicate. Different points are different species. Probability of being detected is a species-specific parameter (does not vary among sites, e.g.). Red points are species that were observed, black points are species that were never observed.
#' 
#' 
#' The above plot is interesting because it shows that exceptionally high or exceptionally
#' low chances to be observed only occur for the species that were observed at least once;
#' i.e., this says that if you didn't observe it, it just takes on the mean.
#' That's reasonable, I guess; but I also would think that the things that
#' were never observed could also be things that had a low chance of observability;
#' but they could also have just a low chance of actually being present.
#' So I suppose in the end it just doesn't get informed, and reverts to the mean?

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#' 

#' #Scatter Plot of Aggregated $\psi$
#+ psiAggFig, fig.width=3.5, fig.height=3.5, cache=TRUE
# ==================================
# = Compare True and Estimated Psi =
# ==================================
dim.conv1 <- c(grid.w*grid.h, ns, grid.t, n.obs.reps)

psi.true <- attr(big.out.obs[[1]], "psi")
psi.true <- logit(pmax(pmin(array(psi.true, dim=dim.conv1),1-1E-3),1E-3))

psi.hat <- lapply(sim.rich.cov, function(x)x$mean$psi[,1:ns])
psi.hat <- logit(pmax(pmin(array(simplify2array(psi.hat), dim=dim.conv1),1-1E-3),1E-3))

psi.true.aggRep <- apply(psi.true, c(1,2,3), mean)
psi.hat.aggRep <- apply(psi.hat, c(1,2,3), mean)

lims.agg <- range(c(psi.hat.aggRep, psi.true.aggRep))

# Set up plot
par(mar=c(2.5,2.5,0.1,0.1), mgp=c(1.25,0.15,0), tcl=-0.15, ps=10, cex=1)
expXlab <- bquote(logit(psi[true]))
expYlab <- bquote(logit(hat(psi)))
plot(psi.true.aggRep[,,],psi.hat.aggRep[,,], ylim=lims.agg, xlim=lims.agg, xlab=expXlab, ylab=expYlab)
abline(a=0, b=1, lwd=2, col="white")
abline(a=0, b=1, lwd=0.5, col="black")
#' **Figure.** MSOM estimates of $\psi$ ($\hat{\psi}$) vs. true values of $\psi$ ($\psi_{true}$). Each point is a $\psi$ value for a particular site-species-year, averaged across *r*=\Sexpr{n.obs.reps} simulated replicate observations (i.e., the "true" value is the same, but the each simulated replicate has a different outcome of how the same true process was observed). The white and black line is the 1:1 line.

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'

#' # $\psi$ Scatter Plots -- Panels Split Years & Reps
#+ fig-psi-full, cache=TRUE, echo=TRUE, fig.width=10, fig.height=6
cols2ramp <- c("blue","green","yellow","orange","red")
box.cols.index <- as.numeric(as.factor(c(t(matrix(taxChance, nrow=grid.t)))))
box.cols <- colorRampPalette(cols2ramp)(lu(taxChance))[box.cols.index]
lims <- range(c(psi.hat, psi.true))
col <- adjustcolor("black",alpha.f=0.1)
 
par(mfrow=c(grid.t,n.obs.reps), mar=c(0.25,0.25,0.1,0.1), ps=6, mgp=c(0.75,0.1,0), tcl=-0.05, cex=1, oma=c(0,0.25,0.5,0))

for(j in 1:dim(psi.true)[3]){
	for(i in 1:dim(psi.true)[4]){
		counter <- i + (j-1)*dim(psi.true)[4]
		plot(psi.true[,,j,i],psi.hat[,,j,i], ylim=lims, xlim=lims, xlab="", ylab="", cex=0.1, xaxt="n",yaxt="n", col=col,pch=20,bty="n")
		axis(side=1, labels=F)
		axis(side=2, labels=F)
		box(col=box.cols[counter])
		
		if(i==1){
			mtext(paste0("t = ",j), side=2, line=0, font=2, cex=1)
		}
		
		if(j==1){
			mtext(paste0("rep = ",i), side=3, line=-0.1, font=2, cex=1, adj=0)
		}
		
		abline(a=0,b=1, col=adjustcolor("black",alpha.f=0.5))
	}
}

#' **Figure.** True (horizontal axes) and MSOM estimates (vertical axes) of occupancy probabilities ($\psi_{j,i,t,r}$) of species *i* occupying a location *j* in year *t*. In our simulation, $\psi$ is a function of individual species characteristics (niche) and the environment, the latter of which changes among years. The simulated (true) outcome of each year was subject to *r* replicate observations of the true process. Each simulated observation (*r*) was an independent realization, but the *r* replicates also differed in the probability that a species would be detected ($p$): the color of the boxes around each panel indicate whether the among-species average of the probability of detection was low (<span style="color:blue">blue</span> ; $p_{min}=$ `r round(min(taxChance),2)`) or high (<span style="color:red">red</span>; $p_{max}=$ `r round(max(taxChance),2)`). The year *t* of the simulated true process changes across the rows of panels, and the simulated replicate observation *r* changes across columns.

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'

#' #E.g. LME for $\psi$ Evaluation
#+ ExploratoryLMER
# ====================
# = LME Model on Psi =
# ====================
# Just exploration/ starting point
library(car)
library(lme4)

#' Motivation: MSOM skill might differ across dimensions, trying to figure out
#' what patterns I should expect to pick out (spatial patterns in richness, temporal?)
#' E.g., Is the correlation between MSOM and True the same comparing
#' across sites as comparing across years? Species, reps, also.
#' 
#' 
#' 
#' Motivation: What factors influence MSOM skill in a given dimension?
#' E.g., Skill in finding differences in $\psi$ across species may depend on $p$,
#' the chance of being identified. If $p$ changes among years, might also explain

#' This example is looking at $\psi$, probability of an individual species being present

# http://stats.stackexchange.com/questions/31569/questions-about-how-random-effects-are-specified-in-lmer
blah <- reshape2:::melt.array(psi.true, varnames=c("site","spp","time","rep"), value.name="true", as.is=T)
blah.hat <- reshape2:::melt.array(psi.hat, varnames=c("site","spp","time","rep"), value.name="hat", as.is=T)
blah <- cbind(blah, hat=blah.hat[,"hat"])

blah$site <- as.factor(blah$site)
blah$spp <- as.factor(blah$spp)
blah$time <- as.factor(blah$time)
blah$rep <- as.factor(blah$rep)

(blah.mod <- lmer(hat~true+(1|spp)+(1|time), data=blah))

Anova(blah.mod)


#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'


#' #`Nsite` Comparison
#' ##Spatial & Temporal Representation of `Nsite` 
#+ NsiteComparison, include=TRUE
# ================================================================
# = Compare Site-specific True, Observed, and Estimated Richness =
# ================================================================

# True Space-Time Richness
# Simple, b/c no replicates, and no substrata
Nsite.true <- apply(attributes(big.out.obs[[1]])$Z.obs, c(1,3), function(x)sum(x))
Nsite.true <- aperm(array(Nsite.true, dim=c(grid.w,grid.h,grid.t)), c(2,1,3))

# Observed Space-Time Richness
# More complicated b/c first have to aggregate to remove substrata (take max for each spp-year-strat)
# Then have to sum over spp (same as for True)
# Then have to average over replicates
Nsite.obs <- apply(attributes(big.out.obs[[1]])$Z.obs, c(1,3), function(x)sum(x))
Nsite.obs <- aperm(array(Nsite.obs, dim=c(grid.w,grid.h,grid.t)), c(2,1,3))

Nsite.obs <- lapply(big.out.obs, function(x)attributes(x)$Z.obs)


get.rich.obs <- function(x){
	x2 <- aggregate(x, fact=3, max)
	matrix(values(stackApply(x2, rep(1,nlayers(x2)), sum)),byrow=T, nrow=nrow(x2),ncol=ncol(x2))
	# stackApply(x2, rep(1,nlayers(x2)), sum)
}
Nsite.obs <- lapply(big.out.obs, function(x){lapply(attributes(x)$obs, get.rich.obs)})
Nsite.obs.array <- simplify2array(lapply(Nsite.obs, simplify2array)) # 1=height,2=width,3=grid.t,4=n.obs.reps
Nsite.obs.mu <- apply(Nsite.obs.array, 1:3, mean)


# Estimated Space-Time Richness
get.Nsite <- function(x, ns, format=TRUE){
	if(missing(ns)){
		ns <- get("ns", envir=parent.env(environment()))
	}
	xo <- apply(x$mean$Z[,1:ns], 1, sum)
	if(format){
		xo <- matrix(xo, nrow=grid.h, byrow=TRUE)
	}
	return(xo)
}

Nsite.msom0 <- lapply(sim.rich.cov, get.Nsite)
Nsite.msom <- array(unlist(Nsite.msom0), dim=c(grid.h,grid.w,grid.t,n.obs.reps))
Nsite.msom.mu <- apply(Nsite.msom, 1:3, mean)



#+ uglyNsiteRich_fig_code, include=FALSE, echo=FALSE
# ==================================
# = Nsite Graphing Code/ Functions =
# ==================================
# Graphs comparing True, Observed, and Estimated Site-&-Year-Specific Richness
my.image <- function(x, smplt=c(0.85,0.88, 0.2,0.8), bgplt=c(0.05,0.82,0.15,0.95), xaxt="n", yaxt="n", ...){
	smplt <- c(0.85,0.88, 0.2,0.8)
	bgplt <- c(0.05,0.82,0.15,0.95)
	axargs <- list(mgp=c(0.5,0.15,0))
	image.plot(t(x), smallplot=smplt, bigplot=bgplt, axis.args=axargs, xaxt=xaxt, yaxt=yaxt, ...)
}

	
Nsite_spaceTime <- function(j){
	zlim.true <- range(Nsite.true) # this is true, observed will be less, but idk about estimated, b/c could be overestimate
	zlim.obs <- range(Nsite.obs.mu)
	zlim.msom <- range(Nsite.msom.mu)
	ylim <- c(1.085,-0.085)
	
	if(j==2){
		zlim.true <- zlim.obs <- zlim.msom <- range(c(Nsite.true,Nsite.obs.mu,Nsite.msom.mu))
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
	# dev.off()
}


#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'


#+ Nsite_compare_spaceTime_diffScale, fig.width=6, fig.height=3.5, include=T
Nsite_spaceTime(j=1)
#' **Figure.** Maps of site- and year-specific species richness (`Nsite`) from the simulation of the True process (top row), simulation of the Observed process (middle row), and the MSOM estimates (bottom row). X-axis and Y-axis indicate position in 2 dimensional space; it is important to note that the environmental variable changes linearly across the y-axis, and randomly (and much less) across the x-axis. The different columns represent separate years. The environmental variable changes linearly among years (the rate of change is the same for all x-y locations). Colors indicate species richness (warm colors are higher richness than cool colors), averaged over the simulated replicate observations ($r=`r n.obs.reps`$ ). Horizontal and vertical axes Each row of panels is scaled independently, columns within a row are scaled equally.

#' 
#' 
#' 
#' 
#' 

#+ Nsite-compare-spaceTime-sameScale, fig.width=6, fig.height=3.5, include=T
Nsite_spaceTime(j=2)
#' **Figure.** Same as previous figure, but all panels are on the same scale.

#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#'

#' ##Scatter Plots of `Nsite` Split by Year
#+ Nsite-compare-scatter, fig.width=6, fig.height=2.5
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
#' **Figure.** Site-specific richness (`Nsite`, $N_j$) from simulated observations (vertical axis, top row; $N_j^{obs}$) and from MSOM estimates (vertical axis, bottom row, $\hat{N_j}$) vs true site-specific richness (horizontal axis; $N_j^{*}$). The panel columns delineate the years of the simulation. Each point is site-specific ($j= `r grid.h` \times `r grid.w` = `r grid.h*grid.w`$) species richness that has been averaged over the simulated replicate observations ($r=`r n.obs.reps`$).
