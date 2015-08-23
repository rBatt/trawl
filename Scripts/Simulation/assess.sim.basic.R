#' ---
#' author: "Ryan Batt"
#' date: "2015-08-22"
#' abstract: "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enimad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#'     fig_caption: true
#'     theme: "readable"
#'   pdf_document:
#'     toc: true
#'     template: latex-ryan.template
#'     fig_caption: true
#' geometry: margin=1.0in
#' lineno: true
#' lineSpacing: true
#' titlesec: true
#' documentclass: article
#' ---

#+ deleted-pandoc-headers, include=FALSE, echo=FALSE
# #'      pandoc_args: [
# #'      "--chapters"
# #'      ]

#+ setup, include=FALSE, echo=FALSE
# =================
# = Load Packages =
# =================
# #' date: "2015-08-22"
# #' title: "assess.sim.basic.R"
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

# Report
library(knitr)
library(rmarkdown)
library(xtable)

# Other
library(rbLib) # library(devtools); install_github("rBatt/rbLib")


# ================
# = Report Setup =
# ================
opts_chunk$set(fig.path = '../../Figures/Simulation/assess.sim.basic/', echo=FALSE, include=TRUE, cache=TRUE)

# render!
# rmarkdown::render("~/Documents/School&Work/pinskyPost/trawl/Scripts/Simulation/assess.sim.basic.R")
# rmarkdown::render("~/Documents/School&Work/pinskyPost/trawl/Scripts/Simulation/assess.sim.basic.R", output_format="pdf_document")


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
load("../../Results/Simulation/sim.basic.RData")


# ========================
# = Constant Definitions =
# ========================
rangeK <- range(sapply(big.out.obs, function(x)attr(x,"n.haul")))


#'  
#'  
#'  
#'  
#' ***
#'  
#'  
#'  


#' #<u>Conventions and Settings</u>
#' ##Dimension Conventions
#' ####Summary
#+ dimensionConventions, indent="    "
#'   
#' 1. Site ($j=1, 2, \dots, j_{max}= `r grid.h` \times `r grid.w` = `r grid.w*grid.h`$)  
#' 	+ Sites are unique combinations of latitude and longitude  
#' 	+ The spatial arrangement of sites is *not* arbitrary, although importance depends on settings (see `dynamic` below)  
#' 	+ The environmental variable $X$ varies among sites (and years, below)
#' 2. Sub-sites ($k=1, 2, ...$)  
#' 	+ Sub-sites are only relevant to the "observation" process
#' 	+ Each site has the same number of possible sub-sites, but the number of sub-sites observed can vary
#' 	+ In this simulation, $k_{max}=`r n.ss`$, $k_{min}^{observed}=`r rangeK[1]` sdf$, and $k_{max}^{observed}=`r rangeK[2]`$  
#' 	+ Substrata are are primarily useful for determining $p$, the [detection probability](#definition-of-p)  
#' 3. Species ($i=1, 2, \dots i_{max}=R=`r ns`$)  
#' 	+ Does not include "augmented" species
#' 	+ For this MSOM analysis, the species array was padded with `r n0s` 0's
#' 4. Time ($t=1, 2, \dots `r grid.t`$)  
#' 	+ Time is primarily used to vary the parameters controlling the "true" process
#' 	+ When those parameters don't change, time provides independent$^*$realizations of the same "true" process
#' 		* $^*$*Note: only when `dynamic=FALSE` in `sim.spp.proc`*
#' 5. Replicates ($r=`r n.obs.reps`$)
#' 	+ Replicates are *simulated* repeated human observations of the same *realization* of the "true" process at $\text{Time}_t$
#' 	+ Replicates are used to vary the parameters that control the "observation" process
#' 	+ When those parameters don't change, each replicate provides an independent$^*$realization of the same "observation" process
#'   
#' ####In Code
#' The MSOM analyzes each year$_t$--replicate$_r$ combination independently.Parameters subscripted by these dimensions are derived from separate analyses.  
#'  
#' In my code, I've tried to be consistent in my use of these indices to describe arrays, matrices, and rasters. Rows are dimension 1, columns dimension 2, etc. The precedent of subscripted dimensions follows the numeric ordering of the list above. E.g., in the matrix $X_{i,t}$ each row will refer to a different species, and each column a different year (note that site$_j$ is skipped, so species$_i$ is "promoted" to dimension 1, the row.). By default, R fills matrices and arrays by column, whereas the `raster` package fills them by row. In most cases where an R object needs to split sites into the lat/ lot components, I make use of the `raster` package. Therefore, the numbering of the sites proceeds row-wise, where each site is numbered according to the order in which it is filled, as in this $2 \times 3$ matrix:
#'  $J = \left( \begin{array}{ccc}
#' 1 & 2 & 3 \\
#' 4 & 5 & 6
#' \end{array} \right)$  
#' Note that even though this matrix is numbered row-wise, it is still indexed as $J_{row, column}$, such that $J_{1,2}=2$. As mentioned previously, this information is primarily important for understanding the code involved with this project, and in most cases it is not crucial to be explicitly aware of the spatial arrangement of sites.  


#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'   


#' ##Simulation Settings
#+ print-sim-msom-info, include=TRUE, echo=TRUE
#' I created a class called `"spp"`, which has methods for `print()`. The `Dimensions` are the number of sites, the number of species, then the number of years.  
#' Also `print`ed are some richness summary statistics. `All cells` refers to the collective richness over all $j$ taken together. The meaning of `One cell` differs slightly between the true and observed printouts: in the true printout the richness is of a particular site ($j$), and in the observed printout it is of a particular sub-site ($k$).
# Dimensions of the simulation (and basic richness info)
big.out.obs[[1]]
#'   
#' In the MSOM, detectability ($p_i$) is determined in the form of a logistic regression, which currently only has an intercept ($v_0$) as predictor (so just a mean). That intercept varies among species (i.e., $v_{0,i}$), and that variation is generated by drawing each individual species's intercept ($v_{0,i}$) from a parent distribution: $v_{0,i}\sim \mathcal{N}(\mu_{v_0}, \sigma^2_{v_0})$. See [section about $p$](#definition-of-p) for more info. 
#' `r kable(data.frame(year=1:grid.t, mu.v0=t.noID.mus, sigma.v0=t.noID.sd))`

print.xtable.booktabs <- function(x){

    print(xtable(x),
        floating=F,
        hline.after=NULL,
        add.to.row=list(pos=list(-1,0, nrow(x)),
        command=c(
             '\\toprule\n',
            '\\midrule\n',
             '\\bottomrule\n')))

}
#' `r print.xtable.booktabs(data.frame(year=1:grid.t, mu.v0=t.noID.mus, sigma.v0=t.noID.sd))`


 

#' 
# Check the attributes
# names(attributes(big.out.obs[[1]]))
# str(attributes(big.out.obs[[1]])[c("X.obs","Z","Z.obs","psi","p")])

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'   

#'
# ==========================
# = JAGS Settings for MSOM =
# ==========================
#' ##JAGS Settings for MSOM
#+ jagsSettings
# Check on the MSOM output
#' `r kable(data.frame(nChains=nChains, nIter=nIter, n0s=n0s, nSamples=nSamples))`
# str(sim.rich.cov[[1]])


#'   
#'   
#'   
#'   
#' ***  
#'   
#'   
#'   
#'   


#' ##Assessment Settings
#' ####Central Tendency
#+ assessmentSettings-centralT, include=TRUE, echo=FALSE
centralT <- c("mean","median")[2]
#' The posterior samples from JAGS consist of `r nSamples` samples (above). However, to save memory and hard drive space, I have often only saved measures of central tendency for each of these. In this assessment, I have performed all calculations on the `centralT`=**`r centralT`** of the posterior samples.


#'   
#'   
#'   
#'   
#' ***  
#'   
#'   
#'   
#'   
  


#' #<u>Species Richness</u>
#+ richnessDefinition, indent="    "
#' ##Definition of species richness
#' Species richness is the number of different species, or more generically, unique taxa. The point is moot in the simulation study, and in the emprical trawl data it refers to species.  
#'   


#' Estimates of richness can be made spatially or temporally explicit (or neither, or both), but obviously a  

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'  


#' ##Regional Richness
#'   
#' These estimates of species richness only distinguish between replicates and years. They do not contain any site-specific information.
#'   
#' ####Richness Boxplots
#' With the boxplots we're mostly looking to see if the estimates of richness vary with the mean [probability of detection, $p$](definition-of-p). In the empirical data, we know that taxonomic identification changed over time (it improved; generally, more species were ID'd in later years). We also suspect that gear might change, which affects the probability of observing a species. The "Fraction Capable of Being ID'd" category in the boxplots is essentially the cross-species average of $p$.
#+ speciesRichness-boxplots, fig.width=3.5, fig.height=3.5
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

mu.p0 <- lapply(sim.rich.cov, function(x)t(c(plogis(x[[centralT]]$v.a0))*t(x[[centralT]]$Z)))
mu.p <- apply(sapply(mu.p0, function(x)apply(x,2,pTot)),2,sum)

Z <- apply(sapply(sim.rich.cov, function(x)apply(x[[centralT]]$Z[,1:ns],2,pTot)),2,sum) # this is right @mtingley

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
#' **Figure.** Boxplots of species richness. Numeric groupings indicate the average value of $p$ across species during a given year--replicate combination. The panels in the left column are the true simulated values, and the panels on the right are the corresponding MSOM estimates. The top row indicates the latent realized species richness or MSOM estimates of the richness. The bottom row's panels are the simulated observed values of richness (the response variable in the MSOM) and the MSOM estimates of the observed values.

#'   
#'   
#'   
#'   
#' ***  
#'   
#'   
#'   
#'   

#' ####Richness Time Series
#+ speciesRichness-timeSeries, fig.width=3.5, fig.height=3.5
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

# Panels for True Richness
rt.z.ylim <- range(c(R.ylim("rich.true"),R.ylim("Z")))
plot(R[R[,"rep"]==1,c("year","rich.true")], type="l", lwd=2, col="blue", ylim=rt.z.ylim, ylab=bquote(R^{true}), xlab="")

plot(R.mu("Z"), type="n", ylim=rt.z.ylim, ylab=bquote(hat(R)), xlab="")
for(i in 1:n.obs.reps){lines(R[R[,"rep"]==i,c("year","Z")], col="gray50")}
lines(R.mu("Z"), type="l", lwd=2, col="blue")

# Panels for Observed Richness
ro.mup.ylim <- range(c(R.ylim("rich.obs"),R.ylim("mu.p")))
plot(R.mu("rich.obs"), type="n", ylim=ro.mup.ylim, ylab=bquote(R^{obs}))
lines(R[R[,"rep"]==i,c("year","rich.obs")], col="gray50")
for(i in 1:n.obs.reps){lines(R[R[,"rep"]==i,c("year","rich.obs")], col="gray50")}
lines(R.mu("rich.obs"), type="l", lwd=2, col="blue")

plot(R.mu("mu.p"), type="n", ylim=ro.mup.ylim, ylab=bquote(hat(R)*phantom()^{obs}))
for(i in 1:n.obs.reps){lines(R[R[,"rep"]==i,c("year","mu.p")], col="gray50")}
lines(R.mu("mu.p"), type="l", lwd=2, col="blue")
#' **Figure.** Time series of richness. Gray lines are individual replicates. Blue lines are averages. Note that  detection probabilities ($p_{i,t,r}$, see [simulation settings above](#simulation-settings), as well as [definition of $p$ below](#definition-of-p)) change over time, and their temporal ordering differs among replicates.

#' Text explanation goes here  
#'   
#' Need explanations for how each panel was calculated.  
#'   
#' 1. $R^{true}$ is straightforward  
#' 2. $\hat{R}$ is from $\sum\limits_{i=1}^{R^{true}}max(\hat{Z}_{i,\nabla j\in J}$); and to be clear, $\hat{R}$ does not include the "unobserved" species introduced to the MSOM occurrence matrix ($Y$)

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'   


#' ##Site Specific Richness (`Nsite`)
#+ calculateNsite, incldue=TRUE



# ======================
# = Nsite Calculations =
# ======================
# True Space-Time Richness
# Simple, b/c no replicates, and no substrata
Nsite.true <- apply(attributes(big.out.obs[[1]])$Z, c(1,3), function(x)sum(x))
Nsite.true <- aperm(array(Nsite.true, dim=c(grid.w,grid.h,grid.t)), c(2,1,3))


# Observed Space-Time Richness
# More complicated b/c first have to aggregate to remove substrata (take max for each spp-year-strat)
# Then have to sum over spp (same as for True)
# Then have to average over replicates
# Nsite.obs <- apply(attributes(big.out.obs[[1]])$Z.obs, c(1,3), function(x)sum(x))
# Nsite.obs <- aperm(array(Nsite.obs, dim=c(grid.w,grid.h,grid.t)), c(2,1,3))
#
# Nsite.obs <- lapply(big.out.obs, function(x)attributes(x)$Z.obs)


# get.rich.obs <- function(x){
# 	x2 <- aggregate(x, fact=sqrt(n.ss), max)
# 	matrix(values(stackApply(x2, rep(1,nlayers(x2)), sum)),byrow=T, nrow=nrow(x2),ncol=ncol(x2))
# 	# stackApply(x2, rep(1,nlayers(x2)), sum)
# }
# Nsite.obs <- lapply(big.out.obs, function(x){lapply(attributes(x)$obs, get.rich.obs)})
# Nsite.obs.array <- simplify2array(lapply(Nsite.obs, simplify2array)) # 1=height,2=width,3=grid.t,4=n.obs.reps
# Nsite.obs.mu <- apply(Nsite.obs.array, 1:3, mean)

Z.obs <- lapply(big.out.obs, function(x)attributes(x)$Z.obs)
Z.obs.array <- aperm(array(simplify2array(Z.obs), dim=c(grid.w, grid.h, ns, grid.t, n.obs.reps)),c(2,1,3:5))
Nsite.obs <- apply(Z.obs.array, c(1,2,4,5), sum)
Nsite.obs.mu <- apply(Nsite.obs,c(1,2,3), mean)


# Estimated Space-Time Richness
get.Nsite <- function(x, ns, format=TRUE){
	if(missing(ns)){
		ns <- get("ns", envir=parent.env(environment()))
	}
	xo <- apply(x[[centralT]]$Z[,1:ns], 1, sum)
	if(format){
		xo <- matrix(xo, nrow=grid.h, byrow=TRUE)
	}
	return(xo)
}

Nsite.msom0 <- lapply(sim.rich.cov, get.Nsite)
Nsite.msom <- array(unlist(Nsite.msom0), dim=c(grid.h,grid.w,grid.t,n.obs.reps))
Nsite.msom.mu <- apply(Nsite.msom, 1:3, mean)



#+ plotNsite-functions, include=TRUE
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
		if(i==1){text("True",x=-0.2,y=0.5, xpd=NA,srt=90, cex=1.25)}
	}

	# Plot obs richness
	for(i in 1:grid.t){
		my.image(Nsite.obs.mu[,,i], zlim=zlim.obs, ylim=ylim)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)}
		if(i==1){text("Observed",x=-0.2,y=0.5, xpd=NA,srt=90, cex=1.25)}
	}

	# Plot msom estimates of richness
	for(i in 1:grid.t){
		my.image(Nsite.msom.mu[,,i], zlim=zlim.msom, ylim=ylim)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)}
		if(i==1){text("MSOM",x=-0.2,y=0.5, xpd=NA,srt=90, cex=1.25)}
		# text(i,x=0.5,y=-0.1, xpd=T,srt=90, cex=1.25)
		text("",x=0.5,y=1.15, xpd=T,srt=0, cex=1.25)
		text(paste0("t=",i),x=0.5,y=1.15, xpd=NA,srt=0, cex=1.25)
	}
	# dev.off()
}


#' ####Scatter Plots of `Nsite` Split by Year
#+ compareNsite-scatter, fig.width=6, fig.height=2.5
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

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'  


# =========================
# = Plot Nsite Space-Time =
# =========================
#' ####Maps of Richness (space and time)
#+ plotNsite-spaceTime-diffScale, fig.width=6, fig.height=3.5
Nsite_spaceTime(j=1)
#' **Figure.** Maps of site- and year-specific species richness (`Nsite`) from the simulation of the True process (top row), simulation of the Observed process (middle row), and the MSOM estimates (bottom row). X-axis and Y-axis indicate position in 2 dimensional space; it is important to note that the environmental variable changes linearly across the y-axis, and randomly (and much less) across the x-axis. The different columns represent separate years. The environmental variable changes linearly among years (the rate of change is the same for all x-y locations). Colors indicate species richness (warm colors are higher richness than cool colors), averaged over the simulated replicate observations ($r=`r n.obs.reps`$ ). Horizontal and vertical axes Each row of panels is scaled independently, columns within a row are scaled equally.

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'   

#+ compareNsite-spaceTime-sameScale, fig.width=6, fig.height=3.5
# ====================================
# = compareNsite-spaceTime-sameScale =
# ====================================
Nsite_spaceTime(j=2)
#' **Figure.** Same as previous figure, but all panels are on the same scale.  
#'   
#' Text explanation goes here  

#'  
#'  
#'  
#'  
#' ***
#'  
#'  
#'  


# ====================================
# = # ==============================
# = Occupancy Probability, Psi =
# ============================== =
# ====================================
#' #<u>Occupancy Probability, $\psi$</u>
#'   
#'   
#' ## Definition of $\psi$
#'   
#' Definition description goes here  
#' Probably need to describe how it's generated in the simulation  
#' As well as how it's estimated in the MSOM  
#' In particular, important to point out that they may or may not match  


#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'    

#' ##Scatter Plot of Aggregated $\psi$
#+ psiAggFig, fig.width=3.5, fig.height=3.5
# ==================================
# = Compare True and Estimated Psi =
# ==================================
use.logit.psi <- FALSE
agg.psi <- FALSE
dim.conv1 <- c(grid.w*grid.h, ns, grid.t, n.obs.reps)


get.psiTrue <- function(x, use.logit=FALSE, agg=FALSE){
	psi.true <- attr(x, "psi")
	psi.true <- array(psi.true, dim=dim.conv1)
	if(use.logit){
		psi.true <- logit(pmax(pmin(psi.true,1-1E-3),1E-3))
	}
	if(agg){
		psi.true <- apply(psi.true, c(1,2,3), mean)
	}
	return(psi.true)
}
man.psi.true <- attributes(big.out.obs[[1]])$psi
psi.true <- get.psiTrue(big.out.obs[[1]], use.logit.psi, agg.psi)


get.psiHat <- function(x, use.logit=FALSE, agg=FALSE){
	psi.hat <- lapply(x, function(x)x[[centralT]]$psi[,1:ns])
	psi.hat <- array(simplify2array(psi.hat), dim=dim.conv1)
	if(use.logit){
		psi.hat <- logit(pmax(pmin(psi.hat,1-1E-3),1E-3))
	}
	if(agg){
		psi.hat <- apply(psi.hat, c(1,2,3), mean)
	}
	return(psi.hat)
}
man.psi.hat0 <- lapply(sim.rich.cov, function(x)x[["median"]]$psi[,1:ns])
man.psi.hat <- array(simplify2array(man.psi.hat0), dim=dim.conv1)
psi.hat <- get.psiHat(sim.rich.cov, use.logit.psi, agg.psi)



image.plot(t(matrix(apply(sim.rich.cov[[1]]$median$psi[,1:ns],1, sum), nrow=grid.h, nc=grid.w, byrow=TRUE)))
image.plot(t(matrix(apply(man.psi.true[,,1],1, sum), nrow=grid.h, nc=grid.w, byrow=TRUE)))



# Set up plot
par(mar=c(2.5,2.5,0.1,0.1), mgp=c(1.25,0.15,0), tcl=-0.15, ps=10, cex=1)
lims.psi <- range(c(psi.hat, psi.true))
if(use.logit.psi){
	expXlab.psi <- bquote(logit(psi[true]))
	expYlab.psi <- bquote(logit(hat(psi)))
}else{
	expXlab.psi <- bquote(psi[true])
	expYlab.psi <- bquote(hat(psi))
}


col <- adjustcolor("black", alpha.f=0.25)
plot(psi.true, psi.hat, ylim=lims.psi, xlim=lims.psi, xlab=expXlab.psi, ylab=expYlab.psi, pch=20, col=col, cex=0.5)
abline(a=0, b=1, lwd=3, col="white")
abline(a=0, b=1, lwd=1, col="black")
#' **Figure.** MSOM estimates of $\psi$ ($\hat{\psi}$) vs. true values of $\psi$ ($\psi_{true}$). Each point is a $\psi$ value for a particular site-species-year~~, averaged across $r=`r n.obs.reps`$ simulated replicate observations (i.e., the "true" value is the same, but each simulated replicate has a different outcome of how the same true process was observed)~~. The white and black line is the 1:1 line.

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'  

#' ##Scatter Plot of $\hat{\psi}$ vs $\psi_{true}$, split by year and replicate
#+ psiPlot-splitScatter, fig.width=10, fig.height=6
# ======================================================
# = Full Psi Figure w/ Panels Splitting Years and Reps =
# ======================================================
cols2ramp <- tim.colors(8)[-c(1,8)]
box.cols.index <- as.numeric(as.factor(c(t(matrix(taxChance, nrow=grid.t)))))
box.cols <- colorRampPalette(cols2ramp)(lu(taxChance))[box.cols.index]
lims <- range(c(psi.hat, psi.true))
col <- adjustcolor("black",alpha.f=0.2)
 
par(mfrow=c(grid.t,n.obs.reps), mar=c(0.25,0.25,0.1,0.1), ps=6, mgp=c(0.75,0.1,0), tcl=-0.05, cex=1, oma=c(0,0.25,0.5,0))

for(j in 1:dim(psi.true)[3]){
	for(i in 1:dim(psi.true)[4]){
		counter <- i + (j-1)*dim(psi.true)[4]
		plot(psi.true[,,j,i],psi.hat[,,j,i], ylim=lims, xlim=lims, xlab="", ylab="", cex=0.2, xaxt="n",yaxt="n", col=col,pch=20,bty="n")
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

#' **Figure.** True (horizontal axes) and MSOM estimates (vertical axes) of occupancy probabilities ($\psi_{j,i,t,r}$) of species *i* occupying a location *j* in year *t*. In our simulation, $\psi$ is a function of individual species characteristics (niche) and the environment, the latter of which changes among years. The simulated (true) outcome of each year was subject to *r* replicate observations of the true process. Each simulated observation (*r*) was an independent realization, but the *r* replicates also differed in the probability that a species would be detected ($p$): the color of the boxes around each panel refer to the among-species average of the probability of detection; warm colors indicate that the mean detection probability is high (<span style="color:red">red</span>; $p_{max}=$ `r round(max(taxChance),2)`), whereas cool colors indicate that $p$ was low (<span style="color:blue">blue</span>; $p_{min}=$ `r round(min(taxChance),2)`). The year *t* of the simulated true process changes across the rows of panels, and the simulated replicate observation *r* changes across columns. *Note: what I refer to as $p$ here is really just the probability that a species will be detected if an occupied site is sampled. In this simulation, `r round(n.ss.mu / (n.ss*grid.w*grid.h),2)*100`% of substrata were sampled, which doesn't influence $p$, but can add noise to its estimates.*

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'  

# =======================
# = Psi Response Curves =
# =======================
#' ##Occupancy Response Curves

#' Occupancy response curves are calculated as $logit(\psi_i)=\mathbf{X} \times \mathbf{a_i}$, where 
#' $$ \mathbf{X} =
#' \left( \begin{array}{ccc}
#' 1 & X_{min} & X^{2}_{min} \\
#' \vdots & \vdots & \vdots \\
#' 1 & X_{max} & X^{2}_{min}
#' \end{array} \right) 
#' \text{;  }
#' \mathbf{a_i} =
#' \left( \begin{array}{ccc}
#' a_{0,i} \\
#' a_{3,i} \\
#' a_{4,i} 
#' \end{array} \right) $$
#'   
#' Therefore, these curves are tantamount to values of $\psi$, except that $\psi$ generally pertains to a simulated, observed, or true occupancy probability, whereas the occupancy probability in the response curves is calculated over hypothetical conditions (i.e., over hypothetical values of the environmental gradient $X$).  


# ========================
# = True Response Curves =
# ========================
#' ####True Occupancy Response Curves
#+ responseCurve-true, fig.height=3.5, fig.width=3.5
par(mar=c(1.75,1.75,0.1,0.1), ps=10, mgp=c(0.65,0.05,0), tcl=-0.15, cex=1)
plot(S.dens.X[[1]], ylim=0:1, type="l", col=adjustcolor("black",alpha.f=0.25), xlab="", ylab="")
invisible(sapply(S.dens.X[-1], lines, col=adjustcolor("black",alpha.f=0.25)))
psiCurve.true.mu <- apply(simplify2array(lapply(S.dens.X, function(x)x$y)), 1, mean)
lines(S.dens.X[[1]]$x, psiCurve.true.mu, lwd=3)
lines(S.dens.X[[1]]$x, psiCurve.true.mu, lwd=1, col="white", lty="dotted")
mtext("Environmental Variable", side=1, line=0.85)
mtext(bquote(psi^{true}), side=2, line=0.65)
#' **Figure.** True simulated response curves. Vertical axis is the value of $\psi^{true}$, horizontal axis is the value of the environmental variable that, along with species-specific regression parameters, determines $\psi^{true}$. The thick line is the among-species mean value of $\psi^{true}$ at a given value of the environmental variable.
#'   
#' In the response curve, the values of the environmental variable are an arbitrary gradient, and do not necessarily correspond to what was observed in the simulated environment (although they are intended to cover the same range).


# =============================
# = Estimated Response Curves =
# =============================
#' ####Estimated Occupancy Response Curves
#+ responseCurve-msom, include=TRUE, fig.height=6, fig.width=10
range.X <- range(values(grid.X))
pred.X <- seq(range.X[1], range.X[2], length.out=100)
u.a0.hat <- sapply(sim.rich.cov, function(x)x[[centralT]]$u.a0[1:ns])
a3.hat <- sapply(sim.rich.cov, function(x)x[[centralT]]$a3[1:ns])
a4.hat <- sapply(sim.rich.cov, function(x)x[[centralT]]$a4[1:ns])
# dev.new(width=9, height=6)
par(mfrow=c(grid.t,n.obs.reps), mar=c(0.25,0.25,0.1,0.1), ps=6, mgp=c(0.75,0.1,0), tcl=-0.05, cex=1, oma=c(0,0.25,0.5,0))
psiPred <- function(X, u.a0, a3, a4){
	cbind(x=X, y=plogis(u.a0 + a3*X + a4*X^2))
}

cols2ramp <- tim.colors(8)[-c(1,8)]
box.cols.index <- as.numeric(as.factor(c(t(matrix(taxChance, nrow=grid.t)))))
box.cols <- colorRampPalette(cols2ramp)(lu(taxChance))[box.cols.index]
col <- adjustcolor("black",alpha.f=0.2)


for(j in 1:grid.t){
	for(i in 1:n.obs.reps){
		cntr <- i + (j-1)*n.obs.reps
				
		# t.psi1 <- psiPred(X=pred.X, u.a0=u.a0.hat[1,cntr], a3=a3.hat[1,cntr], a4=a4.hat[1,cntr])
		t.psi <- mapply(psiPred, 
			u.a0=u.a0.hat[,cntr], a3=a3.hat[,cntr], a4=a4.hat[,cntr], 
			MoreArgs=list(X=pred.X), 
			SIMPLIFY=F
		)
		# t.psi <- c(list(t.psi1), t.psi.not1)
		psi.mus <- apply(simplify2array(t.psi)[,2,],1,mean)
		
		
		plot(t.psi[[1]], ylim=0:1, xlab="", ylab="", type="l", xaxt="n", yaxt="n", col=col, bty="n")
		invisible(lapply(t.psi[-1], lines, col=col))
		lines(pred.X, psi.mus, lwd=3, col="black")
		lines(pred.X, psi.mus, lwd=1, col="white", lty="dotted")
		
		axis(side=1, labels=F)
		axis(side=2, labels=F)
		box(col=box.cols[cntr])
		
		if(i==1){
			mtext(paste0("t = ",j), side=2, line=0, font=2, cex=1)
		}
		
		if(j==1){
			mtext(paste0("rep = ",i), side=3, line=-0.1, font=2, cex=1, adj=0)
		}
		
	}
}
#' **Figure.** Response curves of species' probability of occupancy ($\psi_{i}$, vertical axis) across the full range of temperatures in the simulation ($min(X)=`r round(range.X[1],1)`$, and $max(X)=`r round(range.X[2],1)`$). The color of the boxes around each panel refer to the among-species average of the probability of detection; warm colors indicate that the mean detection probability is high (<span style="color:red">red</span>; $p_{max}=$ `r round(max(taxChance),2)`), whereas cool colors indicate that $p$ was low (<span style="color:blue">blue</span>; $p_{min}=$ `r round(min(taxChance),2)`). The year *t* of the simulated true process changes across the rows of panels, and the simulated replicate observation *r* changes across columns.
#' 
#' 
#' 


#'  
#'  
#'  
#'  
#' ***
#'  
#'  
#'  




# =======================================
# = =================================
# = Probability of Detection, $p$ =
# ================================= =
# =======================================
#' #<u>Probability of Detection, $p$</u>
#' ##Definition of $p$
#+ pDefinition, indent="    ",
format_t.noID.mus <- gsub(", (?=[0-9]{1}$)", ", and ", paste(t.noID.mus, collapse=", "), perl=T)
#' The probability of detection ($p$), is a species specific parameter in the MSOM model. The MSOM analyzes all years ($t$) and replicates ($r$) separately, so I am going to leave those subscripts out of this description. In the simulation, the probability of observing a species is a function of two independent factors:
#' 
#'   1. The probability that site $j$ is occupied by species $i$; this is $\psi_{j,i}$
#' 	+ $\psi_{j,i}$ is a function of species-specific niche and an environmental variable that changes over space and time  
#' 	+ $Z_{j,i}$ is the species- and site-specific richness, which is a function of $\psi$ (given that we're only talking about species that are in the pool of possible species, determined by $w_i$)  
#'   
#'   2. A species-specific ($i$) chance of being identified (`taxChance`), given that it is present in a location that was sampled (i.e., detectability does not reflect the probability of sampling a place); this detectability parameter is $p_{i}$  
#' 	+ Detectability changed between years.
#' 	+ In a given year, $logit(p_i) \sim \mathcal{N(p_{\mu},\sigma^2)}$. $p_{\mu}$ changed between years (taking on values of `r format_t.noID.mus`), $\sigma^2=`r t.noID.sd`$ in all years.
#' 	+ The value of $p$ only changes between species (and years), but the observation process occurs at the substratum ($k$) level. Thus, the parameter is really $p_{j,k,i}$, but for a given $i$, all $p_{j,k}$ are constant. I represent this probability as $p_i$ with the understanding that this value is repeated over space.
#' 	+ $Y_{j,i}$ is the observed version of $Z_{j,i}$.
#' 	+ $Y_{j,i} \sim Bern(p_i \times Z_{j,i})$.  
#' 	    + *Note: Because $p$ is actually subscripted to $k$, the $Y$ are also actually subscripted to $k$. Maybe leaving these subscripts out is making things more confusing. I've only excluded them to emphasize how parameters are estimated.*
#' 	+ Our data about species presence/ absence correspond to $Y_{j,i}$. So it might be useful to think of the MSOM as estimating $\hat{Y}_{j,i}$, which is compared to the observed data $Y_{j,i}^{obs}$.
#' 
#' 

#'   
#'  
#'   
#'  
#' ***  
#'   
#'  
#'   
#'  

#' ##Demo: Effect of MSOM Hierarchy on $p$
#+ egHierarchEffect_PsiMu, fig.width=4, fig.height=4

demo.Zobs <- attr(big.out.obs[[1]],"Z.obs")[,,1] # Z.obs for the demo year
demo.p.nobs <- apply(demo.Zobs, 2, sum, na.rm=TRUE) # n detections 4 each species
demo.p.col <- c("black","red")[((demo.p.nobs!=0) + 1)] # red is detected, black is never detected


plot(plogis(sim.rich.cov[[1]][[centralT]]$v.a0)[1:ns], ylab="p (detectability)", col=demo.p.col)
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
#+ pCalculations  
# =========================================
# = Evaluate p (Probability of Detection) =
# =========================================
use.logit.p <- FALSE
agg.p <- FALSE

# True p
get.pTrue <- function(x, use.logit=FALSE, agg=FALSE){
	mini.get.pTrue <- function(x){
		op <- attr(x, "obs.params")
		miniP <- t(op$tax.chance)*op$base.chance
	}
	p.true <- lapply(x, mini.get.pTrue)
	p.true <- array(unlist(p.true,F,F),dim=c(ns,grid.t,n.obs.reps))
	
	return(p.true)
	
}

p.true <- get.pTrue(big.out.obs, use.logit.p, agg.p)


# MSOM p
# Note that I am only able to get the p parameter out of the MSOM model if all
# substrata are visited in all cases, otherwise the node isn't fully defined,
# and JAGS returns a warning that p can't be tracked as a paramter.
get.pHat <- function(x, use.logit=FALSE, agg=FALSE){
	mini.get.pHat <- function(x){
		names.pHat <- names(x[[centralT]])
		if("p"%in%names.pHat){
			if(length(dim)==3){ # putting this in here b/c I think I'm going to change how p is indexed
				x[[centralT]]$p[1,1,1:ns] # to be used when p indexed by j,k,i
			}else{
				x[[centralT]]$p[1:ns] # to be used when p index by i only
			}
			
		}else{ # p won't always be available; so just calculate it from v.a0
			# note that this won't be right if the observation process is not simply predicted by this intercept
			plogis(x[[centralT]]$v.a0[1:ns])
			
			# to check that they're the same:
			# plot(sim.rich.cov[[1]][[centralT]]$p[1,1,1:ns], plogis(sim.rich.cov[[1]][[centralT]]$v.a0[1:ns]))
			# abline(a=0, b=1)
			#
			# vals <- replicate(1E2,{
# 				vals <- rnorm(1E3, sd=2);
# 				c("mean.first"=plogis(mean(vals)), "mean.second"=mean(plogis(vals)))
# 			})
# 			plot(t(vals)); abline(a=0, b=1)
			#
			# vals2 <- replicate(1E2,{
			# 	vals2 <- rnorm(1E3, sd=2);
			# 	c("mean.first"=logit(mean(plogis(vals2))), "mean.second"=mean(logit(plogis(vals2))))
			# })
			# plot(t(vals2)); abline(a=0, b=1)
			
		}
	}
	p.hat <- lapply(sim.rich.cov, mini.get.pHat)
	p.hat <- array(unlist(p.hat,F,F), dim=c(ns, grid.t, n.obs.reps))
	return(p.hat)
}
p.hat <- get.pHat(sim.rich.cov, use.logit.p, agg.p)
# all(sapply(sim.rich.cov, function(x)apply(x[[centralT]]$p, c(3), all.same))) # 3rd dimension is species. Substrata are identical (within year, rep, species, stratum); this is as described previously, and why I am tempted to only write p with the i subscript.

# ======================
# = Scatter Plots of p =
# ======================
#' ##Scatter Plot of $\hat{p}$ vs $p_{true}$
#+ pPlot-fullScatter, fig.height=3.5, fig.width=3.5

par(mar=c(2.5,2.5,0.1,0.1), mgp=c(1.25,0.15,0), tcl=-0.15, ps=10, cex=1)
lims.p <- range(c(p.hat, p.true))
if(use.logit.p){
	expXlab.p <- bquote(logit(p[true]))
	expYlab.p <- bquote(logit(hat(p)))
}else{
	expXlab.p <- bquote(p[true])
	expYlab.p <- bquote(hat(p))
}

lims.p <- range(c(p.true, p.hat))
col <- adjustcolor("black", alpha.f=0.25)

par(mar=c(2.5,2.5,0.1,0.1), mgp=c(1.25,0.15,0), tcl=-0.15, ps=10, cex=1)
plot(p.true, p.hat, ylim=lims.p, xlim=lims.p, xlab=expXlab.p, ylab=expYlab.p, pch=20, col=col, cex=1)
abline(a=0, b=1, lwd=2, col="white")
abline(a=0, b=1, lwd=0.5, col="black")
#' 
#' **Figure.** MSOM estimates (vertical axis) and true values of $p_i$, the species-specific ($i$) detection probability. Each point is subscripted by species $i$`r ifelse(agg.p, "and", ",")` year $t$, `r ifelse(agg.p, paste0("but are averaged among the $r=",n.obs.reps, "$ observation replicates"), "and observation replicate $r$")`.
#' 
#'
#' 
#'
#' ***
#' 
#'
#' 
#' 


#' ##Scatter Plot of $\hat{p}$ vs $p_{true}$, split by year and replicate
#+ pPlot-splitScatter, fig.height=6, fig.width=10

plot(1, type="n")
text(1,1, labels="place holder!")
#' **Figure.** Caption goes here.  
#'   
#' Text explanation goes here

#'  
#'  
#'  
#'  
#' ***
#'  
#'  
#'  


#' #<u>Assessment with Mixed Effects Models</u>
#' ##E.g. LME for $\psi$ Evaluation
#' 
#' **Motivation**: MSOM skill might differ across dimensions, trying to figure out
#' what patterns I should expect to pick out (spatial patterns in richness, temporal?)
#' E.g., Is the correlation between MSOM and True the same comparing
#' across sites as comparing across years? Species, reps, also.
#'   
#'   
#'   
#' **Motivation**: What factors influence MSOM skill in a given dimension?
#' E.g., Skill in finding differences in $\psi$ across species may depend on $p$,
#' the chance of being identified. If $p$ changes among years, might also explain
#'   
#' Read more about [specifying mixed effects models using `lmer` in R here](http://stats.stackexchange.com/questions/31569/questions-about-how-random-effects-are-specified-in-lmer)
#'   
#' This example is looking at $\psi$, probability of an individual species being present
#+ exploratoryLMER, echo=TRUE
# ====================
# = LME Model on Psi =
# ====================
# Just exploration/ starting point
library(car)
library(lme4)

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

