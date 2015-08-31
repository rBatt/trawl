#' ---
#' title: "Predicting biodiversity dynamics in response to environmental change"
#' subtitle: " Can we do it? A report from assess.sim.basic.R"
#' author: "Ryan Batt"
#' date: "2015-08-23"
#' abstract: |
#'   I use simulated data to evaluate the ability of a multispecies occupancy model (MSOM) to estimate species richness. An emphasis is placed on the potential to use MSOMs to estimate richness over time and space, although the current version of the simulation and of the MSOM is somewhat simplistic in this regard. The principal finding is that the MSOM can be sensitive to changes in detectability. Not emphasized explicitly in this document, but discovered through experimenting in different versions, is that these sensitivies can depend quite heavily on sample size. Therefore, it may be prudent to analyze all years of trawl data at once, rather than separately. Finally, some of the methods for evaluating the performance of the MSOM on simulated data may provide a useful guide for how to gauge the reliability of the results when the MSOM is applied to empirical data. This document represents a first step towards what I think we want to do in a paper, and also serves to get everyone on the same page. I look forward to further progress and conversation.
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 2
#'     fig_caption: true
#'     theme: "readable"
#'     template: default
#'   pdf_document:
#'     toc: true
#'     template: latex-ryan.template
#'     fig_caption: true
#' geometry: margin=1.0in
#' lineno: true
#' lineSpacing: false
#' titlesec: true
#' documentclass: article
#' placeins: true
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
library(kfigr)
library(stargazer)
library(texreg)

# Other
library(rbLib) # library(devtools); install_github("rBatt/rbLib")


# ================
# = Report Setup =
# ================
o_f <- c("html_document", "pdf_document")[2]

# render!
# rmarkdown::render(
# 	"~/Documents/School&Work/pinskyPost/trawl/Scripts/Simulation/assess.sim.basic.R",
# 	output_format=o_f,
# 	output_dir='~/Documents/School&Work/pinskyPost/trawl/Reports/assess.sim.basic/',
# )

opts_chunk$set(
	fig.path = '../../Figures/Simulation/assess.sim.basic/', 
	cache.path='../../Reports/assess.sim.basic/assess.sim.basic_cache2/',
	echo=FALSE, 
	include=TRUE, 
	cache=TRUE,
	fig.show="hold",
	fig.lp = if(o_f=="html_document"){"**Figure.**"}else{NULL}
)


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


# ====================
# = Report Functions =
# ====================

# Nice Tables
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


#' \FloatBarrier   
#' 
#' ***  
#' 


# ================
# = Introduction =
# ================
#' #<u>Introduction</u>
#'   
#'   
#' ##Overview  
#' As water temperatures change, species may shift the size and location of their geographical ranges, bearing consequences for the food webs and economies linked to those species. However, species don't always respond similarly to shifting temperatures (different thermal tolerances, e.g.), which means that changing temperature may remix the composition and diversity of ecological communities.  
#'   
#' The biological, spatial, and temporal scale of community diversity shifting in response to climate is massive. A functional definition of a community may consist of 100's or 1000's of species, each of which may be shifting its range at a scale of decades and 100's kilometers. As a result, we need statistical methods for estimating biodiversity that don't rely on heavy replication and that make efficient use of available data. Enter the superstars: on the data side, the trawl data set has amazing spatiotemporal and taxonomic extent and resolution; on the statistical side, multispecies occupancy models (MSOMs) are hierarchical state space models that are designed to estimate species richness and don't require consistent or extensive "replication". Although they're superstars, even these data and models have their limitations and pitfalls.  
#'   
#' Can we estimate the dynamics of species richness from trawl data using an MSOM? It's a hard question to answer because we can never know the "truth" for sure, but we can get an idea of how reliable our analysis is by simulating fake data, for which we know true values because we created them. The trawl data set is generated by two distinct processes: Nature's data generating process (NDGP), and the process by which humans observe the result of NDGP. So we ask: to what extent is the accuracy of estimates from an MSOM dependent on characteristics of NDGP, and in particular, the way in which we observe the result of NDGP? The strategy for answering this question is to simulate fake data where we approximate Nature but gain knowledge of "truth", "observe" the results of the true process, then try to recover the true species richness from these simulated data.  


# ==================
# = The Simulation =
# ==================
#' ##The Simulation
#' The goal of this simulation was to use a very basic process to generate presences and absences of species in space and time. In this version of the simulation, there is no explicit connection between years (they are independent). There is a modest spatial connection, because in the simulation a spatially-explicit environmental variable determines habitat suitability. I think of this environmental variable as temperature, and I filled a grid with temperatures that ranged from the coldest at the top of the grid (north) and the warmest at the bottom (south) and added random variation among columns in the same row (among longitudes at the same latitude).
#'   
#' One level of the simulation mimics NDGP. In this level, NDGP is best characterized by $\psi$, which is the product of a temperature and species' response curves. I.e., temperatures were used to determine the suitability of each grid cell to each simulated species. This suitability is known as $\psi$ throughout this document.
#'   
#' A second level of the simulation mimics human observation of NDGP --- what we do when we collect data.  This process was simulated by assigning each species has a unique probability of being observed or "detected" (this variable is $p$). The observation process gets several attempts at observing a given species in a given grid cell; think of this as subdividing each site into subsites, and when you visit each subsite you have probability $p$ of observing a particular species (each species has its own $p$). Depending on the settings used in the analysis that this document summarizes, the maximum number of subsites can vary, as can the number of subsites per site (OK, fine; the maximum number of subsites in this version is `r n.ss`, the number of subsites per site varied between `r rangeK[1]` and `r rangeK[2]`, and overall `r format(n.ss.mu/(grid.w*grid.h*n.ss)*100,digits=2)`% of total possible subsites were sampled). 
#'   
#' As previously mentioned, the simulation included "time". In this basic version, not much changes between the "years" for the true process (temperature doesn't change, nor do the response curves), but the mean of $p$ does change. In a given year, the entire community has an overall mean probability of being detected, and each species randomly deviates from that mean. 
#'   
#' The simulation also has replicates. To understand the replicates, it needs to be clear that even when a parameter in the simulation does not change, the outcome can change. The replicates hold the realization of the simulated NDGP constant, and draw new realizations of the observation process. I.e., both $\psi$ and $p$ are constant among replicates, and the binary *outcome* of $\psi$ is also held constant, but the outcome for $p$ can change. Furthermore, although each replicate has same values of $p$ (both the mean $p$ and each species' individualized random draw from that distribution), each replicate switches which year is associated with which $p$'s. In this way we can observe each outcome of Nature's data generating process under a series of settings for the human observation process.  


# =========
# = MSOMs =
# =========
#' ##Multispecies Occupancy Models (MSOMs)
#' Multispecies occupancy models are Bayesian statespace hierarchical models. They distinguish between truth and observation of the truth, and many parameters share a common "parent" distribution. They are very flexible models, and can be adapted to include new types of processes. The MSOM being used here is a relatively simple version of these models. It predicts the probability of each species existing in a grid cell from a logistic regression equation that uses a second-order polynomial of the environmental variable as a covariate. The parameters in this level of the model are hierarchical, with species having their own paramter values, but these individual parameters are not wholly independent in the sense that they share a common parent distribution, which sort of acts to both limit how different they can be and to inform one another. The model also has an observation level, which only has a hierarchical intercept (just a mean) as a predictor variable.
#'   
#' The MSOM makes guesses of the true state of the system (whether a species is actually present or not). It then makes guesses at how the observation of that true state might turn out, which is effectively a prediction of what our data will be. The Bayesian model fitting process then uses this comparison of the observed data to the estimate of the observation to tweak the parameters in the MSOM. This process is repeated until the choice of paramters boils down to what is essentially the posterior distribution of the estimated parameters.
#'   
#' Right now the MSOM model is fit separately to each year and each replicate. So the model never gets to see multiple years or multiple replicates at the same time. Furthermore, when referring to a parameter value fitted in the MSOM, it is implied that it can be subscripted with time or replicate (because all years and replicates are fit independently).
#'   
#' The parameters in the logistic regression that predicts the value of $\psi$ vary among species, although $psi$ itself varies among species and space, because the regression parameters (subscripted by species) are multiplied by the environmental variable (subscripted by space). More or less, it can be said that, for a given species, $\psi$ varies among space because of the environmental variable, and in a given location it varies among species because of the regression parameters.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ============================
# = Conventions and Settings =
# ============================
#' #<u>Conventions and Settings</u>
#' In this section I outline the subscripting and notation used in the MSOM analysis and for the simulation. I also outline various settings (number of species simulated, replicates, etc.). Most of the numbers you see (and some of the text) is dynamically generated based on the code that produced the statistics and figures. Therefore, you can refer back to these sections to see what settings may have changed since the last version of this document.  
#'   
#'   ***Note:** I've often found myself having to get creative with subscripts and superscripts. I've tried to be clear an consistent, but small inconsistencies likely exist, so don't be confused by them. For example, if you see $max(Z)$ and $Z_{max}$ in two different sections, they are probably referring to the same thing. If you see something confusing, let me know (preferably by [creating an issue on GitHub](https://github.com/rBatt/trawl/issues)), and I'll fix it.*  


# =========================
# = Dimension Conventions =
# =========================
#' ##Dimension Conventions


# ====================================
# = Summary of Dimension Conventions =
# ====================================
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
#' 	+ In this simulation, $k_{max}=`r n.ss`$, $k_{min}^{observed}=`r rangeK[1]`$, and $k_{max}^{observed}=`r rangeK[2]`$
#' 	+ Substrata are are primarily useful for determining $p$, the [detection probability](#definition-of-p)  
#' 3. Species ($i=1, 2, \dots i_{max}=R=`r ns`$)
#' 	+ Does not include "augmented" species
#' 	+ For this MSOM analysis, the species array was padded with `r n0s` 0's  
#' 4. Time ($t=1, 2, \dots `r grid.t`$)
#' 	+ Time is primarily used to vary the parameters controlling the "true" process
#' 	+ When those parameters don't change, time provides independent$^*$realizations of the same "true" process
#' 		* $^*$*Note: only when `dynamic=FALSE` in `sim.spp.proc`* (`dynamic` is `FALSE` in the basic simulation)  
#' 5. Replicates ($r=`r n.obs.reps`$)
#' 	+ Replicates are *simulated* repeated human observations of the same *realization* of the "true" process at $\text{Time}_t$
#' 	+ Replicates are used to vary the parameters that control the "observation" process
#' 	+ When those parameters don't change, each replicate provides an independent$^*$realization of the same "observation" process  
#'   


# =================================
# = Dimension Conventions in Code =
# =================================
#' ####In Code
#' The MSOM analyzes each year$_t$--replicate$_r$ combination independently. Parameters subscripted by these dimensions are derived from separate analyses.  
#'  
#' In my code, I've tried to be consistent in my use of these indices to describe arrays, matrices, and rasters. Rows are dimension 1, columns dimension 2, etc. The precedent of subscripted dimensions follows the numeric ordering of the list above. E.g., in the matrix $X_{i,t}$ each row will refer to a different species, and each column a different year (note that site$_j$ is skipped, so species$_i$ is "promoted" to dimension 1, the row.). By default, R fills matrices and arrays by column, whereas the `raster` package fills them by row. In most cases where an R object needs to split sites into the lat/ lot components, I make use of the `raster` package. Therefore, the numbering of the sites proceeds row-wise, where each site is numbered according to the order in which it is filled, as in this $2 \times 3$ matrix:
#'  $J = \left( \begin{array}{ccc}
#' 1 & 2 & 3 \\
#' 4 & 5 & 6
#' \end{array} \right)$  
#' Note that even though this matrix is numbered row-wise, it is still indexed as $J_{row, column}$, such that $J_{1,2}=2$. As mentioned previously, this information is primarily important for understanding the code involved with this project, and in most cases it is not crucial to be explicitly aware of the spatial arrangement of sites.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ============
# = Settings =
# ============
#' ##Settings


# =======================
# = Simulation Settings =
# =======================
#' ####Simulation Settings
#+ settings-simulation, include=TRUE, echo=TRUE
#' I created a class called `"spp"`, which has methods for `print()`. The `Dimensions` are the number of sites, the number of species, then the number of years.  
#' Also `print`ed are some richness summary statistics. `All cells` refers to the collective richness over all $j$ taken together. The meaning of `One cell` differs slightly between the true and observed printouts: in the true printout the richness is of a particular site ($j$), and in the observed printout it is of a particular sub-site ($k$).
big.out.obs[[1]] # Dimensions of the simulation (and basic richness info)
#'   
#' In the MSOM, detectability ($p_i$) is determined in the form of a logistic regression, which currently only has an intercept ($v_0$) as a predictor (so just a mean). That intercept varies among species (i.e., $v_{0,i}$), and that variation is generated by drawing each individual species's intercept ($v_{0,i}$) from a parent distribution: $v_{0,i}\sim \mathcal{N}(\mu_{v_0}, \sigma^2_{v_0})$. See [section about $p$](#definition-of-p) for more info. 
#' `r kable(data.frame(year=1:grid.t, mu.v0=t.noID.mus, sigma.v0=t.noID.sd))`
##' `r print.xtable.booktabs(data.frame(year=1:grid.t, mu.v0=t.noID.mus, sigma.v0=t.noID.sd))`


#' \FloatBarrier   
#' 
#' ***  
#' 


# ==========================
# = JAGS Settings for MSOM =
# ==========================
#' ####Settings for JAGS & MSOM
#+ settings-jags
# Check on the MSOM output
#' `r kable(data.frame(nChains=nChains, nIter=nIter, n0s=n0s, nSamples=nSamples))`
#' In the table above, `nChains`, `nIter`, and `nSamples` are all variables that are strictly pertinent to the Bayesian analysis carried out in JAGS. The `n0s` value refers to the the degree of "data augmentation". In this process, you add extra species to the data set, and say that they were never observed. For our purposes, this is employed for purely technical reasons, although it can be used to extra further inferences about species richness.
#'   
#+ assessmentSettings-centralT, include=TRUE, echo=FALSE
centralT <- c("mean","median")[2]
#' The posterior samples from JAGS consist of `r nSamples` samples (above). However, to save memory and hard drive space, I have often only saved measures of central tendency for each of these. In this assessment, I have performed all calculations on the `centralT`=**`r centralT`** of the posterior samples.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ====================
# = Species Richness =
# ====================
#' #<u>Species Richness</u>


# ===========================
# = Define Species Richness =
# ===========================
#+ richnessDefinition, indent="    "
#' ##Definition of species richness
#' Species richness is the number of different species, or more generically, unique taxa. The point is moot in the simulation study, and in the empirical trawl data it refers to species.  
#'   
#' Estimates of richness ($R$) can be made spatially or temporally explicit (or neither, or both). In the following figures, different levels of aggregation are performed – for most figures $R$ is split by year (this is true for all figures but [the Boxplot Figure](#richness-boxplots)). [The Time Series of Richness Figure](#richness-time-series) emphasizes temporal dynamics and keeps replicates separated, but aggregates over space (the $j$ sites). [The `Nsite` Scatter Figure](#scatter-plots-of-nsite-split-by-year) doesn't aggregate over space or time, but it does aggregate over "replicate" observations; importantly, while the figure does present any spatial aggregation, it does not retain the spatial relationship (you can't tell which sites are next to others). The final figure of the section ([Heatmap of of Richness Figure](#maps-of-richness-space-and-time)) is similar to the previous figure, except that spatial relationship among points is retained via a heatmap representation.  
#'   
#' None of these estimates of richness include the `r n0s` species that were part of the "data augmented"/ "adding 0's" process. Richness values can either be true (true simulated NDGP; $R^{true}$), observed (true simulated human observation of NDGP; $R^{obs}$), or MSOM estimates of one of those two ($\hat{R}^{true}$ or $\hat{R}^{obs}$).


#' \FloatBarrier   
#' 
#' ***  
#' 


# =====================
# = Regional Richness =
# =====================
#' ##Regional Richness
#'   
#' These estimates of species richness only distinguish between replicates and years. They do not contain any site-specific information.
#'   
#' ####Richness Boxplots
#' With the boxplots we're mostly looking to see if the estimates of richness vary with the mean [probability of detection, $p$](#definition-of-p). In the empirical data, we know that taxonomic identification changed over time (it improved; generally, more species were ID'd in later years). We also suspect that gear might change, which affects the probability of observing a species. The "Average Detection Probability" category in the boxplots is the cross-species average of $p$ (which with large sample size approach the hyperparameter $p_{\mu}$).


# =========================================
# = Prepare Boxplots of Regional Richness =
# =========================================
# Caption
#+ speciesRichness-boxplots-cap
boxplot.cap <- "Boxplots of species richness. Numeric groupings indicate the average value of $p$ across species during a given year--replicate combination. The panels in the left column are the true simulated values, and the panels on the right are the corresponding MSOM estimates. The top row indicates the latent realized species richness or MSOM estimates of the richness. The bottom row's panels are the simulated observed values of richness (the response variable in the MSOM) and the MSOM estimates of the observed values."

# Actual code to prepare boxplot figure
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


# =================================
# = Boxplots of Regional Richness =
# =================================
#+ speciesRichness-boxplots, fig.width=3.5, fig.height=3.5, fig.cap=boxplot.cap 
tC.names <- round(unique(taxChance),2)
par(mfrow=c(2,2), mar=c(1.75,1.25,0.1,0.1), cex=1, ps=8, mgp=c(1, 0.1, 0), tcl=-0.15, oma=c(0.35,0.5,1,0))

ylim.rr <- range(R[,c("rich.true","Z")])
boxplot(rich.true~taxChance, data=R, ylab="", names=tC.names, ylim=ylim.rr)
mtext("Realized Richness", side=2, line=0.85)
mtext("True ", side=3, line=0.1, font=2)
boxplot(Z~taxChance, data=R, ylab="", names=tC.names, ylim=ylim.rr)
mtext("Estimated", side=3, line=0.1, font=2)

ylim.or <- range(R[,c("rich.obs","mu.p")])
boxplot(rich.obs~taxChance, data=R, ylab="", names=tC.names, ylim=ylim.or)
mtext("Observed Richness", side=2, line=0.85)
boxplot(mu.p~taxChance, data=R, ylab="", names=tC.names, ylim=ylim.or)
mtext("Average Detection Probability", side=1, line=-1, outer=TRUE)


# ================================================
# = Explanation of Boxplots of Regional Richness =
# ================================================


#' \FloatBarrier   
#' 
#' ***  
#' 


# ================================
# = Prepare Richness Time Series =
# ================================
#' ####Richness Time Series
#+ speciesRichness-timeSeries-cap
timeSeries.cap <- "Time series of richness. Gray lines are individual replicates. Blue lines are averages. Note that  detection probabilities ($p_{i,t,r}$, see [simulation settings above](#simulation-settings), as well as [definition of $p$ below](#definition-of-p)) change over time, and their temporal ordering differs among replicates."


# =============================
# = Plot Richness Time Series =
# =============================
#+ speciesRichness-timeSeries, fig.width=3.5, fig.height=3.5, fig.cap=timeSeries.cap
R.mu <- function(name){
	aggregate(structure(list(R[,name]),.Names=name),by=list(year=R[,"year"]),mean)
}
R.ylim <- function(name){
	range(R[,name])
}

par(mfrow=c(2,2), mar=c(2.1,2.0,0.1,0.1), cex=1, ps=9, mgp=c(1.0, 0.2, 0), tcl=-0.15, oma=c(0,0,1,0))

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


# =======================================
# = Explanation of Richness Time Series =
# =======================================

#'   
#' 1. $R^{true}$ is just true richness (among all sites) in each year  
#' 2. $\hat{R}$ is from $\sum\limits_{i=1}^{R^{true}}max(\hat{Z}_{i,\nabla j\in J}$);  
#'     + i.e., basically same as for $R^{true}$, except from $\hat{Z}$ instead of $Z$  
#'     + only difference is that $\hat{Z}$ is contains extra (fake, always absent) species compared to $Z$ (see next)  
#'     + $\hat{R}$ doesn't include the augmented species introduced to the MSOM occurrence matrix ($Y$)  
#' 3. $R^{obs}$ are the values of $R^{true}$ after they pass through the observation process  
#'   + Note that "true" and "observed" and "estimated" can be confusing here; the last term can prefix either of the first 2 terms. This notation needs some work still.  
#' 4. $\hat{R}^{obs}$ is the MSOM estimate of what was observed  
#'   + this the estimate that is compared to the data in the fitting process  
#'   + true values are latent (unobserved)  
#'   + thus, we need this extra step to connect our estimates of what's actually going on to our data  
#'   + although, it should be noted that our data don't arrive in terms of "richness", but in presences/ absences  
#' 
#' In these "time series" plots (so short!), the replciates (grey lines) look so jagged and incosistent because each replicate shifts which year gets which value of $p_{\mu}$ (average detectability).  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ==========================
# = Site Specific Richness =
# ==========================
#' ##Site Specific Richness (`Nsite`)


# ===================
# = Calculate Nsite =
# ===================
#+ calculateNsite, include=TRUE
# True Space-Time Richness
Nsite.true <- apply(attributes(big.out.obs[[1]])$Z, c(1,3), function(x)sum(x))
Nsite.true <- aperm(array(Nsite.true, dim=c(grid.w,grid.h,grid.t)), c(2,1,3))

# Observed Space-Time Richness
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


# ================================
# = Plotting Functions for Nsite =
# ================================
#+ plotNsite-functions, include=TRUE
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
		if(i==1){text("True",x=-0.15,y=0.5, xpd=NA,srt=90, cex=1.25)}
	}

	# Plot obs richness
	for(i in 1:grid.t){
		my.image(Nsite.obs.mu[,,i], zlim=zlim.obs, ylim=ylim)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)}
		if(i==1){text("Observed",x=-0.15,y=0.5, xpd=NA,srt=90, cex=1.25)}
	}

	# Plot msom estimates of richness
	for(i in 1:grid.t){
		my.image(Nsite.msom.mu[,,i], zlim=zlim.msom, ylim=ylim)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(0,0,0), tcl=-0.15, cex=1)
		if(i==1){text("",x=-0,y=0.5, xpd=T,srt=90, cex=1, ps=10)}
		if(i==1){text("MSOM",x=-0.15,y=0.5, xpd=NA,srt=90, cex=1.25)}
		# text(i,x=0.5,y=-0.1, xpd=T,srt=90, cex=1.25)
		text("",x=0.5,y=1.15, xpd=T,srt=0, cex=1.25)
		text(paste0("t=",i),x=0.5,y=1.15, xpd=NA,srt=0, cex=1.25)
	}
	# dev.off()
}


# ==============================
# = Prepare Nsite Scatter Plot =
# ==============================
#+ nsite-scatter.cap
nsiteScatter.cap <- "Site-specific richness (`Nsite`, $N_j$) from simulated observations (vertical axis, top row; $N_j^{obs}$) and from MSOM estimates (vertical axis, bottom row, $\\hat{N_j}$) vs true site-specific richness (horizontal axis; $N_j^{*}$). The panel columns delineate the years of the simulation. Each point is site-specific species richness that has been averaged over the simulated replicate observations."


# ======================
# = Plot Nsite Scatter =
# ======================
#' ####Scatter Plots of `Nsite` Split by Year
#+ compareNsite-scatter, fig.width=5, fig.height=2.5, fig.cap=nsiteScatter.cap
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


# =========================
# = Explain Nsite Scatter =
# =========================
#' The first thing I notice in the `Nsite` scatter plot figure is that the observations tend to underestimate true richness, and the absolute magnitude of this underestimate increases as true richness increases (i.e., with the observation as the response and the true value as the predictor, the intercept is fine, but the slope is too shallow). The MSOM tends to do a much better job of staying near the 1:1 line, but there's is more variance in the residuals at high richness. I'm not sure I understand why (in later figures, $\psi$ estimates don't get worse or more uncertain at high values of true $\psi$).  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ===============================
# = Prepare Nsite Scatter Split =
# ===============================
#+ nsiteScatter-split-cap
nsiteScatter.split.cap <- "Scatter plots of MSOM-estimated species richness (y-axis) and the true species richness (x-axis), with each panel representing a particular year-replicate combination. The color of the panel border indicates the value of $p_{\\mu}$, the community-wide mean detectability."


# ============================
# = Plot Nsite Scatter Split =
# ============================
#+ nsiteScatter-split, fig.width=8*(n.obs.reps/8), fig.height=3.5, fig.cap=nsiteScatter.split.cap
cols2ramp <- tim.colors(8)[-c(1,8)]
box.cols.index <- as.numeric(as.factor(c(t(matrix(taxChance, nrow=grid.t)))))
box.cols <- colorRampPalette(cols2ramp)(lu(taxChance))[box.cols.index]
col <- adjustcolor("black",alpha.f=0.15)

par(mfrow=c(grid.t,n.obs.reps), mar=c(0.5,0.5,0.1,0.1), ps=6, mgp=c(0.75,0.05,0), tcl=-0.15, cex=1, oma=c(1,1.25,0.5,0))
ylim <- range(c(Nsite.true,Nsite.msom))
col <- adjustcolor("black",alpha.f=0.25)
for(i in 1:n.obs.reps){
	for(j in 1:grid.t){
		
		counter <- i + (j-1)*n.obs.reps

		yaxt <- ifelse(j==1, "s", "n")
		xaxt <- ifelse(i==grid.t, "s", "n")
		
		plot(Nsite.true[,,j], Nsite.msom[,,j,i], xaxt=xaxt, xlab="", yaxt=yaxt, ylab="", ylim=ylim, xlim=ylim, pch=20,col=col, bty="n")
		
		# axis(side=1, labels=F)
		abline(a=0,b=1)
		axis(side=1, labels=F)
		axis(side=2, labels=F)
		box(col=box.cols[counter], lwd=2)
		
		if(i==1){mtext(paste0("t = ",j), side=2, line=0.5, font=2, cex=1)}
		if(j==1){mtext(paste0("rep = ",i), side=3, line=-0.1, font=2, cex=1, adj=0)}
	}

}
mtext("True", side=1, line=0.1, outer=TRUE, font=2, cex=1)
mtext("MSOM", side=2, line=0.65, outer=TRUE, font=2, cex=1)


# =========================================
# = Prepare Nsite Space-Time (diff scale) =
# =========================================
#' ####Maps of Richness (space and time)
#+ nsiteMap.diffScale.cap
nsiteMap.sameScale.cap <- "Maps of site- and year-specific species richness (`Nsite`) from the simulation of the True process (top row), simulation of the Observed process (middle row), and the MSOM estimates (bottom row). X-axis and Y-axis indicate position in 2 dimensional space. Colors indicate species richness (warm colors are higher richness than cool colors), averaged over the simulated replicate observations. The color scale is the same among all panels."


# ======================================
# = Plot Nsite Space-Time (same scale) =
# ======================================
#+ compareNsite-spaceTime-sameScale, fig.width=5.75, fig.height=3.5, fig.cap=nsiteMap.sameScale.cap
Nsite_spaceTime(j=2)


# =================================
# = Explain Both Nsite Space-Time =
# =================================
#' In these maps, the environmental variable $X$ changes linearly across the y-axis, and randomly (and much less) across the x-axis. The different columns represent separate years. The environmental variable changes linearly among years (the rate of change is the same for all x-y locations), and in this basic simulation that rate of change is actually just 0 units/year.  
#'   
#' Because so many things are static in this simulation the maps of `Nsite` are not much more informative than the scatter plots of `Nsite`. However, making the spatial aspect of richness visually explicit does emphasize that richness is highly dependent on the envionrmental variable --- looking at the occupancy response curves below indicates that for the community as a whole the probability of occypancy is highest in the middle of the environmental gradient, which also happens to be the middle of the "latitudinal" gradient in this figure. Within a latitudinal band, the MSOM doesn't do as good a job of parsing the variability in richness.


#' \FloatBarrier   
#' 
#' ***  
#' 



# ==============================
# = Occupancy Probability, Psi =
# ==============================
#' #<u>Occupancy Probability, $\psi$</u>


# =====================
# = Definition of Psi =
# =====================
#' ## Definition of $\psi$
#' The probability that a particular location $j$ will be occupied by species $i$ is $\psi$ (omitting subscripts). This probability is a function of the environment and 3 species-specific parameters. To calculate $\psi_i$ under a set of known conditions in an environmental variable $X$ at time $t$ and site $j$, we can express it as the log of the odds ratio (logit link) resulting from the the linear combination of three terms:  
#' $$
#' logit(\psi_{j,i,t})=
#' \left( \begin{array}{ccc}1 \\x_{j,t} \\x_{j,t}^2 \end{array} \right)^{\intercal}
#' \left( \begin{array}{ccc}a_{0,i} \\a_{3,i} \\a_{4,i} \end{array} \right)
#' $$  
#' and 
#' $$ \begin{array}{c}
#' a_{0,i} \sim \mathcal{N}(\mu_{a0},\sigma^2_ {a0})
#' \\a_{3,i} \sim \mathcal{N}(\mu_{a3},\sigma^2_ {a3})
#' \\a_{4,i} \sim \mathcal{N}(\mu_{a4},\sigma^2_ {a4})
#' \end{array}
#' $$
#' Thus, the parameters are hierarchical, and for this reason the response curves vary but are somewhat clustered around a central value.  
#'   
#' In my mind, $\psi$ is the Holy Grail of parameters to recover. It tells us the odds that a species will be in a certain place at a certain time. If I could know this perfectly, I would be very pleased (and we'd all be very famous).  
#'   
#' When this analysis (using an MSOM on the trawl data) was originally crafted, $\psi$ was more of a means to an end than it was the objective -- $\psi$ lets us get at richness, and we have hypotheses about how richness should change with climate that we'd like to test. But if you know what controls $\psi$, you know what controls richness.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ===================
# = Prepare Psi Agg =
# ===================
#' ##Scatter Plot of $\psi$
#+ psiPlot-fullScatter.cap
psiAggFig.cap <- "MSOM estimates of $\\psi$ ($\\hat{\\psi}$) vs. true values of $\\psi$ ($\\psi_{true}$). Each point is a $\\psi$ value for a particular site-species-year-replicate. The white and black line is the 1:1 line."

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


# ================
# = Plot Psi Agg =
# ================
#+ psiPlot-fullScatter, fig.width=3.5, fig.height=3.5, fig.cap=psiAggFig.cap
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


# ============================
# = Explain Psi Full Scatter =
# ============================
#' In a general sense, the MSOM can distinguish between instances (sites/ years) when a species is likely to be present, and when it's not. However, in every simulation I've done (varying many parameters that aren't compared in this document), the scatter plot of $\psi$ always makes it apparent that  
#'   
#'   1. There is a lot of variability around the 1:1 line  
#'   2. The residuals are not normal, and they are not independent  
#'     i. In general, I've found that $\hat{\psi}$ exhibits an upward bias, overestimating $\psi^{true}$  
#'     ii. Smoothly-curving excursions from the 1:1 line are often prominent  
#'   
#' These patterns are somewhat concerning. The curve-like sequence of residuals is probably a byproduct of slightly incorrect estimates of the parameters in the logistic regression ($[a_0, a_1, a_2]$), resulting in estimated [response curves](#occupancy-response-curves) that deviate non-randomly from the true response curve. For a heuristic of how these smoothe excursions can occur, in R try something as simple as `d <- rnorm(100); plot(dnorm(d), dt(d, 1))` to see the relationship between the density estimate from the correct distribution and that from the wrong distrubtion (the density is analogous to $\psi$); or for really crazy patterns, try `d <- rnorm(100); plot(dnorm(d), do.call(approxfun, density(d)[c("x","y")])(d))`. So the curves are explainable, but I cannot explain the consistent overestimation; I could understand how underestimating detectability ($p$) would result in overestimating $\psi$, but the MSOM appears to recover true $p$ values rather well (e.g., see [$p$ Scatter Figure](#scatter-plot-of-p)), so that's not a satistfying explanation.
#'   
#' In the next section I drill into $\psi$ a bit more to try and understand what causes the largest deviations from true values.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# =============================
# = Prepare Psi Split Scatter =
# =============================
# #' ##Scatter Plot of $\hat{\psi}$ vs $\psi_{true}$, split by year and replicate
#' ##Scatter Plots for Each  $\psi_{t,r}$
#+ psiPlot-splitScatter.cap
psiPlot.splitScatter.cap <- "True (horizontal axes) and MSOM estimates (vertical axes) of occupancy probabilities ($\\psi_{j,i,t,r}$) of species *i* occupying a location *j*. Years ($t$) are separated by rows, replicates ($r$) are separated by columns. The border color of each panel indicates the community-level mean probability of detection ($p_{\\mu}$; where [$p_{i} \\sim \\mathcal{N}(p_{\\mu},\\sigma^2)$](#definition-of-p)), with warm colors indicating high detectability, and cool colors low. The species-specific detectabilities are **not** re-randomized among replicates, but even when the probabilities associated with the observation process do not change, the outcome of the process can change. The year *t* of the simulated true process changes across the rows of panels, and the simulated replicate observation *r* changes across columns."


# ==========================
# = Plot Psi Split Scatter =
# ==========================
#+ psiPlot-splitScatter, fig.width=8*(n.obs.reps/8), fig.height=3.5, fig.cap=psiPlot.splitScatter.cap
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
		box(col=box.cols[counter], lwd=2)
		
		if(i==1){
			mtext(paste0("t = ",j), side=2, line=0, font=2, cex=1)
		}
		
		if(j==1){
			mtext(paste0("rep = ",i), side=3, line=-0.1, font=2, cex=1, adj=0)
		}
		
		abline(a=0,b=1, col=adjustcolor("black",alpha.f=0.5))
	}
}


# =============================
# = Explain Psi Split Scatter =
# =============================
#' The estimates and true values of $\psi$ are best correlated when $p$ is high. When the average species has a low chance of being detected (when $p_{\mu}$ is, say, 20%), the estimates of $\psi$ are a mess.  
#'   
#' Does this help explain an apparent positive bias in the aggregated scatter plot of $\psi$? Maybe. When detectability ($p_{\mu}$, the parameter indicated by panel colors) is low, that's when we run into trouble. With low detectability, you have fewer observations. You have a poorer sense of what's going on. So that adds uncertainty. Perhaps when detectability is super low, it's entirely too easy to conflate an absence with an undetected presence --- you start assuming that 0's are just because you didn't look hard enough, not because it's really absent. I'm not convinced by this logic, though; I'd want to see a better explanation. Alternatively, maybe the chains aren't converging yet; I didn't run diagnostics. So for now this is a mystery to me.  
#'   
#' *Note: what I refer to as $p$ here is the probability that a species will be detected if an occupied site is sampled, so the number of substrata sampled per site isn't relfected in $p$. In this simulation, `r round(n.ss.mu / (n.ss*grid.w*grid.h),2)*100`% of substrata were sampled, and while being below 100% doesn't this doesn't influence $p$, visiting few sites could add noise to various parameter estimates.*  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ==============================
# = Define Psi Response Curves =
# ==============================
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
#' Therefore, these curves are tantamount to values of $\psi$, except that $\psi$ generally pertains to a simulated, observed, or true occupancy probability, whereas the occupancy probability in the response curves is calculated over hypothetical conditions (i.e., over hypothetical values of the environmental gradient $X$).  This formulation yields values of $\psi$ for each species at all temperatures over the interval $(X_{min}, X_{max})$ (this implies that the number of rows in $\mathbf{X}$ is infinite, which it isn't, but it is quite large). Also, note that I'm making a distinction between $X$ (environmental variable) and $\mathbf{X}$ (the matrix, one column of which is $X$).  


# ===================================
# = Prepare True Psi Response Curve =
# ===================================
#' ####True Occupancy Response Curves
#+ responseCurve-true.cap
responseCurve.true.cap <- "True simulated response curves. Vertical axis is the value of $\\psi^{true}$, horizontal axis is the value of the environmental variable that, along with species-specific regression parameters, determines $\\psi^{true}$. The thick line is the among-species mean value of $\\psi^{true}$ at a given value of the environmental variable."


# ================================
# = Plot True Psi Response Curve =
# ================================
#+ responseCurve-true, fig.height=3.5, fig.width=3.5, fig.cap=responseCurve.true.cap
par(mar=c(1.75,1.75,0.1,0.1), ps=10, mgp=c(0.65,0.05,0), tcl=-0.15, cex=1)
plot(S.dens.X[[1]], ylim=0:1, type="l", col=adjustcolor("black",alpha.f=0.25), xlab="", ylab="")
invisible(sapply(S.dens.X[-1], lines, col=adjustcolor("black",alpha.f=0.25)))
psiCurve.true.mu <- apply(simplify2array(lapply(S.dens.X, function(x)x$y)), 1, mean)
lines(S.dens.X[[1]]$x, psiCurve.true.mu, lwd=3)
lines(S.dens.X[[1]]$x, psiCurve.true.mu, lwd=1, col="white", lty="dotted")
mtext("Environmental Variable", side=1, line=0.85)
mtext(bquote(psi^{true}), side=2, line=0.65)


# ===================================
# = Explain True Psi Response Curve =
# ===================================
#' In the response curve, the values of the environmental variable are an arbitrary gradient, and do not necessarily correspond to what was observed in the simulated environment (although they are intended to cover the same range). The formulation $logit(\psi_i)=\mathbf{X} \times \mathbf{a_i}$ is useful for producing the response curve, but it is inherently discrete. This is actually how I simulated the true $\psi$ in the model, but it's not how the MSOM analyzes it (although the difference is negligible because I use so many rows in $\mathbf{X}$).  

#' \FloatBarrier  
#' 
#' ***  
#' 

# ========================================
# = Prepare Estimated Psi Response Curve =
# ========================================
#' ####Estimated Occupancy Response Curves

#+ responseCurve-msom.cap
responseCurve.msom.cap <- " Response curves of species' probability of occupancy ($\\psi_{i}$, vertical axis) across the full range of temperatures in the simulation. The color of the boxes around each panel refer to the among-species average of the probability of detection; warm colors indicate that the mean detection probability is high, whereas cool colors indicate that $p$ was low. The year *t* of the simulated true process changes across the rows of panels, and the simulated replicate observation *r* changes across columns."


# =====================================
# = Plot Estimated Psi Response Curve =
# =====================================
#+ responseCurve-msom, include=TRUE, fig.height=3.5, fig.width=8*(n.obs.reps/8), fig.cap=responseCurve.msom.cap
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
				
		t.psi <- mapply(psiPred, 
			u.a0=u.a0.hat[,cntr], a3=a3.hat[,cntr], a4=a4.hat[,cntr], 
			MoreArgs=list(X=pred.X), 
			SIMPLIFY=F
		)
		psi.mus <- apply(simplify2array(t.psi)[,2,],1,mean)
		
		plot(t.psi[[1]], ylim=0:1, xlab="", ylab="", type="l", xaxt="n", yaxt="n", col=col, bty="n")
		invisible(lapply(t.psi[-1], lines, col=col))
		lines(pred.X, psi.mus, lwd=3, col="black")
		lines(pred.X, psi.mus, lwd=1, col="white", lty="dotted")
		
		axis(side=1, labels=F)
		axis(side=2, labels=F)
		box(col=box.cols[cntr], lwd=2)
		
		if(i==1){
			mtext(paste0("t = ",j), side=2, line=0, font=2, cex=1)
		}
		
		if(j==1){
			mtext(paste0("rep = ",i), side=3, line=-0.1, font=2, cex=1, adj=0)
		}
	}
}


# =========================================
# = Explain Estimated Psi Response Curves =
# =========================================
#' It is rather evident in these figures that the estimated response curves look less like the true response curves for low detection probabilities (low $p_{\mu}$). It is important to remember this effect when evaluating the esimates of `Nsite`, and especially when looking at the heat maps of `Nsite` in  [Figures 4 and 5](#site-specific-richness-nsite) because in those figures each grid cell represents the richness averaged over replicates, and the value of $p_\mu$ varies among replicates. Furthermore, the inaccurate response curves at low $p_{\mu}$ are consistent with the relationships between $\hat{\psi}$ and $\psi^{true}$ in [Figure 7](#scatter-plots-for-each-psi_tr).  
#'   
#' It is my suspicion that, at a large enough sample size of both preseneces and abseneces across a full gradient of temperatures, these deficiencies will diminish. However, this analysis, in its basic form, was not designed to test the influence of sample size (I've run separate versions of this model, not presented here, and can informally confirm my suspicion -- sample size matters a lot). Furthermore, most regions in the trawl data set have a few dozen sites (site being defined on a $1^{\circ}$ grid), so the $`r grid.h` \times `r grid.w`$ grid simulated here is approximately the spatial sample size we'd have to work with in the empirical analysis.  
#'   
#' Two ways to change the sample size in the empirical analysis would be to  
#'   
#'   1. reduce grid size to increase the number of sites  
#'     + could do a half-degree grid  
#'     + make substrata 1/4 degree  
#'     + downside is that each site would be represented in a fewer proportion of years  
#'   2. Use multiple years of data  
#'     + drastically increases sample size  
#'     + would require a new model  
#'     + the model would need to conider factors that change among years  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ===================================
# = Define Probability of Detection =
# ===================================
#' #<u>Probability of Detection, $p$</u>
#' ##Definition of $p$
#+ pDefinition, indent="    ",
format_t.noID.mus <- gsub(", (?=[0-9]{1}$)", ", and ", paste(t.noID.mus, collapse=", "), perl=T)
#' The probability of detection ($p$), is a species specific parameter in the MSOM model. The MSOM analyzes all years ($t$) and replicates ($r$) separately, so I am going to leave those subscripts out of this description. In the simulation, the probability of observing a species is a function of two independent factors:
#' 
#'   1. The probability that site $j$ is occupied by species $i$; this is $\psi_{j,i}$
#' 	+ $\psi_{j,i}$ is a function of species-specific niche and an environmental variable that changes over space and time
#' 	+ $Z_{j,i}$ is the species- and site-specific richness, which is a function of $\psi$ (given that we're only talking about species that are in the pool of possible species, determined by $w_i$)  
#'   2. A species-specific ($i$) chance of being identified (`taxChance`), given that it is present in a location that was sampled (i.e., detectability does not reflect the probability of sampling a place); this detectability parameter is $p_{i}$
#' 	+ Detectability changed between years.
#' 	+ In a given year, $logit(p_i) \sim \mathcal{N}(p_{\mu},\sigma^2)$. $p_{\mu}$ changed between years (taking on values of `r format_t.noID.mus`), $\sigma^2=`r t.noID.sd`$ in all years.
#' 	+ The value of $p$ only changes between species (and years), but the observation process occurs at the substratum ($k$) level. Thus, the parameter is really $p_{j,k,i}$, but for a given $i$, all $p_{j,k}$ are constant. I represent this probability as $p_i$ with the understanding that this value is repeated over space.
#' 	+ $Y_{j,i}$ is the observed version of $Z_{j,i}$.
#' 	+ $Y_{j,i} \sim Bern(p_i \times Z_{j,i})$.
#' 	    + *Note: Because $p$ is actually subscripted to $k$, the $Y$ are also actually subscripted to $k$. Maybe leaving these subscripts out is making things more confusing. I've only excluded them to emphasize how parameters are estimated.*
#' 	+ Our data about species presence/ absence correspond to $Y_{j,i}$. So it might be useful to think of the MSOM as estimating $\hat{Y}_{j,i}$, which is compared to the observed data $Y_{j,i}^{obs}$.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ======================================
# = Calculate Probability of Detection =
# ======================================   
#+ pCalculations  

# Options
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
# and JAGS returns a warning that p can't be tracked as a parameter
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
			
		}
	}
	p.hat <- lapply(sim.rich.cov, mini.get.pHat)
	p.hat <- array(unlist(p.hat,F,F), dim=c(ns, grid.t, n.obs.reps))
	return(p.hat)
}
p.hat <- get.pHat(sim.rich.cov, use.logit.p, agg.p)


# =================================
# = Prepare Agg Scatter Plot of P =
# =================================
#' ##Scatter Plot of $p$
#+ pPlot-fullScatter-cap
pPlot.fullScatter.cap <- "MSOM estimates (vertical axis) and true values of $p_i$, the species-specific ($i$) detection probability. Each point is subscripted by species $i$, year $t$, and observation replicate $r$."


# =========================
# = Plot Agg Scatter of P =
# =========================
#+ pPlot-fullScatter, fig.height=3.5, fig.width=3.5, fig.cap=pPlot.fullScatter.cap
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


# ============================
# = Explain Agg Scatter of P =
# ============================
#' The MSOM does a pretty good job of recovering $p$. This is true in other conditions I've simulated --- conditions where it failed miserably to recover $\psi$ (mainly because of small sample sizes). It's worth noting here that most of the points are either very close to 1 or very close to 0. In the next figures, the reason for this will become more apparent.  


#' \FloatBarrier   
#' 
#' ***  
#' 


# ===========================
# = Prepare P Split Scatter =
# ===========================
#' ##Scatter Plots for Each $p_{t,r}$
#+ pPlot-splitScatter.cap
pPlot.splitScatter.cap <- "Probability of detection ($p$), separated by year and replicate. True values ($p^{true}$) are on the horizontal axis, MSOM estimates ($\\hat{p}$) are on the vertical axis. The color of the panel borders corresponds to the value of $p_{\\mu}$, with warm colors being high and cool colors being low values of $p_{\\mu}$. In a given panel, each point is a different species. The rows are different years, the columns are the simulated replicate observations of those years. The same set of true values for $p$ is used for each replicate, just in a different order. If this simulation shows the same color panel box more than once per column, those panels share the same $p_{\\mu}$, but they have independent realizations of $p^{true}$. See the section entitled [The Simulation](#the-simulation) for further clarification."


# ========================
# = Plot P Split Scatter =
# ========================
#+ pPlot-splitScatter, fig.height=3.5, fig.width=8*(n.obs.reps/8), fig.cap=pPlot.splitScatter.cap
cols2ramp <- tim.colors(8)[-c(1,8)]
box.cols.index <- as.numeric(as.factor(c(t(matrix(taxChance, nrow=grid.t)))))
box.cols <- colorRampPalette(cols2ramp)(lu(taxChance))[box.cols.index]
lims <- range(c(p.hat, p.true))
col <- adjustcolor("black",alpha.f=0.15)
 
par(mfrow=c(grid.t,n.obs.reps), mar=c(0.25,0.25,0.1,0.1), ps=6, mgp=c(0.75,0.1,0), tcl=-0.05, cex=1, oma=c(0,0.25,0.5,0))

for(j in 1:dim(psi.true)[3]){
	for(i in 1:dim(psi.true)[4]){
		counter <- i + (j-1)*dim(p.true)[3]
		plot(p.true[,j,i],p.hat[,j,i], ylim=lims, xlim=lims, xlab="", ylab="", cex=1, xaxt="n",yaxt="n", col=col,pch=20,bty="n")
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



# ===========================
# = Explain P Split Scatter =
# ===========================
#' The grouping in this figure is a bit odd because the panels with the highest detectability also don't show very much range (because both axes are sitting at the high detectability!). However, in general, it looks like the estimates of $p$ are pretty good overall. There are some weird points, but overall, both the multi-panel and the combined scatter plot indicate that we are recovering $p$ fairly reliably.  
#'   
#' In the previous section I pointed out that points group close to 0 or 1. That's just due to the different values of $p_{\mu}$. I also wonder what a real value for $p$ would look like. I have no idea which of these values are reasonable. 


#' \FloatBarrier   
#' 
#' ***  
#' 


# ===============
# = Define LMER =
# ===============
#' #<u>Assessment with Mixed Effects Models</u>
#' ##Describe Motivation for Mixed Effects Models
#' 
#' **Motivation**: MSOM skill might differ across dimensions, trying to figure out
#' what patterns I should expect to pick out (spatial patterns in richness, temporal?)
#' E.g., Is the correlation between MSOM and True the same comparing
#' across sites as comparing across years? Species, reps, also.
#'   
#'   
#' **Motivation**: What factors influence MSOM skill in a given dimension?
#' E.g., Skill in finding differences in $\psi$ across species may depend on $p$,
#' the chance of being identified. If $p$ changes among years, might also explain
#'   
#' Read more about [specifying mixed effects models using `lmer` in R here](http://stats.stackexchange.com/questions/31569/questions-about-how-random-effects-are-specified-in-lmer)
#'   
#' This example is looking at $\psi$, probability of an individual species being present


# ================
# = Prepare LMER =
# ================
#' ## Example LMER Analysis for $\psi$
#+ exploratoryLMER, echo=TRUE
# Just exploration/ starting point
library(car)
library(lme4)

# psi
# psi true
dat.psi.true <- reshape2:::melt.array(
	psi.true, 
	varnames=c("site","spp","time","rep"), 
	value.name="psi.true", 
	as.is=T
)
# psi hat
dat.psi.hat <- reshape2:::melt.array(
	psi.hat, 
	varnames=c("site","spp","time","rep"), 
	value.name="psi.hat", 
	as.is=T
)

# p
# p true
dat.p.true <- reshape2:::melt.array(
	aperm(array(p.true, dim=c(dim(p.true), dim(psi.true)[1])),c(4,1,2,3)), 
	varnames=c("site","spp","time","rep"), 
	value.name="p.true", 
	as.is=T
)
# p hat
dat.p.hat <- reshape2:::melt.array(
	aperm(array(p.hat, dim=c(dim(p.hat), dim(psi.hat)[1])),c(4,1,2,3)), 
	varnames=c("site","spp","time","rep"), 
	value.name="p.hat", 
	as.is=T
)

# n.hauls
n.hauls <- sapply(big.out.obs, function(x)attributes(x)$n.haul)
n.hauls.dim <- c(grid.w*grid.h, n.obs.reps, ns, grid.t)
dat.n.hauls <- reshape2:::melt.array(
	aperm(array(n.hauls, dim=n.hauls.dim), c(1,3,4,2)), 
	varnames=c("site","spp","time","rep"), 
	value.name="n.hauls", 
	as.is=T
)

# grid.X
# same structure (dims) as n.hauls
temp <- values(grid.X)
temp.dim <- c(grid.w*grid.h, n.obs.reps, ns, grid.t)
dat.temp <- reshape2:::melt.array(
	aperm(array(temp, dim=temp.dim), c(1,3,4,2)), 
	varnames=c("site","spp","time","rep"), 
	value.name="temp", 
	as.is=T
)

# tax chance
tax.chance <- simplify2array(
	lapply(big.out.obs, function(x)(attributes(x)$obs.params)$tax.chance)
)
tax.chance.dim <- c(grid.t, ns, n.obs.reps, grid.w*grid.h)
dat.tax.chance <- reshape2:::melt.array(
	aperm(array(tax.chance, dim=tax.chance.dim), c(4,2,1,3)), 
	varnames=c("site","spp","time","rep"), 
	value.name="tax.chance", 
	as.is=T
)


mod.dat <- cbind(
	dat.psi.true, 
	psi.hat=dat.psi.hat[,"psi.hat"],
	p.true=dat.p.true[,"p.true"], 
	p.hat=dat.p.hat[,"p.hat"], 
	n.hauls=dat.n.hauls[,"n.hauls"],
	tax.chance=dat.tax.chance[,"tax.chance"],
	temp=dat.temp[,"temp"]
)
mod.dat[,"psi.error"] <- mod.dat[,"psi.hat"]-mod.dat[,"psi.true"]
mod.dat[,"p.error"] <- mod.dat[,"p.hat"] - mod.dat[,"p.true"]

mod.dat$site <- as.factor(mod.dat$site)
mod.dat$spp <- as.factor(mod.dat$spp)
mod.dat$time <- as.factor(mod.dat$time)
mod.dat$rep <- as.factor(mod.dat$rep)


# ====================
# = Do LMER Analysis =
# ====================
mod1 <- lmer(psi.error~temp+(1|spp)+(1|site), data=mod.dat)
mod2 <- lmer(psi.error~n.hauls+(1|spp)+(1|site), data=mod.dat)
mod3 <- lmer(psi.error~p.error+(1|spp)+(1|site), data=mod.dat)
# mod4 <- lmer(psi.error~n.hauls-1+(1|spp)+(n.hauls-1|spp)+(1|site), data=mod.dat)
mod4 <- lmer(psi.error~temp+(1|spp)+(temp-1|spp)+(1|site), data=mod.dat)
mod5 <- lmer(psi.error~p.error+(1|spp)+(p.error-1|spp)+(1|site), data=mod.dat)
mod6 <- lmer(psi.error~p.error+(p.error|spp)+(1|site), data=mod.dat)

# Calculate covariance matrix (for spp)
mod6.varcor.spp <- attr(summary(mod6)$varcor$spp, "correlation")
mod6.varcor.spp <- format(mod6.varcor.spp, digits=2)
mod6.varcor.spp[!lower.tri(mod6.varcor.spp)] <- ""

mod.cap <- c(
	"Mixed effect models assessing sensitivity of $\\psi_{\\epsilon}$ to simulation conditions"
)

#+ lmer-table, results="asis"
# table for the models
texreg(list(mod1, mod2, mod3, mod4, mod5, mod6), booktabs=TRUE, caption.above=TRUE, caption=mod.cap, sideways=F, digits=3, use.packages=FALSE)

# print.xtable.booktabs(mod6.varcor.spp) # table for the covariance matrix


# ================
# = Explain LMER =
# ================
#' The goal with the mixed effects models was to understand what causes errors in $\psi$. I focused on $\psi$ because it has all the information needed to understand variability in richness, but it has more information than the actual richness (richness is a community level statistic, $\psi$ is species-specific). In these models, the response variable is $\hat{\psi} - \psi^{true}$, which we'll call $\psi_{\epsilon}$. If we understand the source of variability in $\psi_{\epsilon}$, then we can understand what leads to inaccuracies in our model.  
#'   
#' When analyzing the trawl data, we will not know $\psi_{\epsilon}$ -- we can obtain model residuals, but these are distinct from $\psi_{\epsilon}$, because calculating $\psi_{\epsilon}$ requires knowing $\psi^{true}$ which is a latent, unobserved variable. An empirical analysis would also lack some of the explantory variables made available to us in the simulation. However, if we can explain variability in $\psi_{\epsilon}$ using simulated information that will also be available to the trawl analysis, then we can build intuition about the sources of error in our estimate of species richness even when we don't know the true value. And that is the fundamental goal of this document.  
#'   
#' I'll highlight some of the things I learned from this analysis:  
#'   
#'   1. Neither the environmental variable (`temp`) nor the number of subsites sampled per site (`n.hauls`) were strongly related to $\psi_{\epsilon}$
#'     i. But `temp` might be a better predictor if transformed into an absolute value
#'     ii. When the model does not contain a site-specific random intercept (not shown here), `n.hauls` acounts for more variability  
#'   2.  The `spp` random intercept explains much more variability than `site` equivalent  
#'   2. `p.error` explains a ton of variability
#'     i. inversely related to $\psi_{\epsilon}$; intuition: if you have perfect detectability but you think it's terrible, you'll overestimate the true value
#'      ii. model 6 is a worse fit than model 5, meaning that adding covariance structure between `spp`-specific intercept and `spp`-specific `p.error` does not explain much variability
#'   
#'   As stated above, a lot of variance is explained by the model term `(p.error|spp)`, which allows the parameter associated with `p.error` and the intercept (both fixed effects) to vary randomly among species. The interpretation of these model terms is that  
#'   
#'   + Each species gets to draw it's own intercept ("Variance: spp.1.(Intercept)" in the table) from a parent distribution of intercepts
#'   + A unit of error in the estimate of $p$ has an influence on $\psi_{\epsilon}$ that, similar to the intercept, varies among species ("Variance: spp.p.error" in the table).
#'   
#' Therefore, the effect that a bad estimate of $p$ has on $\psi_{\epsilon}$ is not the same among species. It is not clear what causes some species to be more sensitive to a poorly estimated $p$ than others; one possibility is that $p$ is poorly estimated for species that have not been observed much, and it is this lack of observation that is also responsible for generating uncertainty in $\hat{\psi}$. Regardless, a bad (good) estimate of $p$ is a good predictor of a bad (good) estimate of $\hat{\psi}$.  

#' \FloatBarrier   
#' 
#' ***  
#' 


# ==============
# = Conclusion =
# ==============
#' #<u>Conclusion</u>  
#'   
#' ##Discussion of Results
#'  Overall, the MSOM performed well. It is definitely data-hungry in the sense that it needs to observe 1's and 0's for each species in many places under different conditions. This may seem counter to the goal of the MSOM -- to make efficient use of hard-won data. Would other richness methods be better? But remember, what we're getting is site-specific richness, and even species-specific presences and absences. Also, we don't have the "replicates" in the trawl data set needed for the other richness methods.  
#'   
#' This seems obvious in retrospect, but Figure 1 (Boxplots) shows us that the sensitivity of our richness estimates to detectability is highly nonlinear. It'll be important to gauge where we think we are along that spectrum.  Regardless, we were never too far off the real richness --- e.g., when 29 species exist, we estimate 24. On the upside, we're not *too* much worse off if we want to know $\psi$ instead of just R. Of greater concern is the tendency to overestimate R or $\psi$ when $p$ is low However, these low values for $p$ might be *really* low, so perhaps that "problem" is not in a relevant region of parameter space.
#'   
#' ##Next Steps
#' I think the next important step is to decide if we want to analyze all of the years together. It'll require a more complicated model, but it'll give a lot more statistical power (and better estimates). I'm leaning towards doing this.  
#'   
#' We also need to decide what regions from the trawl data set we'll want to use for this analysis. Right now the Alaskan regions are the front runners (just Eastern Berring Sea, or Aleutians and Gulf of Alaska as well?) simply because they have pdf file ranking each species on a scale 1-3 for each year according how likely it was to be identified. This isn't $p$ per se (because other factors affect detectability as well), but this could give insight into a big contributor to temporal shifts in $p$.  
#'   
#' We may also want to upgrade the realism of the simulation. I'm already set up to link years together if we decide to do a multi-year MSOM (MY MSOM?).  
#'   
#' We should also think about what exactly our story/ selling points will be. I tried to word the introduction to this document as something that might sound like a paper Introduction. But this is tough when you don't actually know what you're introducing. I think the key to this paper may to emphasize an ecological finding the Alaskan data sets, but back up that finding with a careful evaluation of the method we're using. My goal would be to highlight how it's important but difficult to understand biodiversity dynamics, and then discover something about biodiversity dynamics while showing that the MSOM is a valid tool for doing so with trawl (and similar) data.  
#'   
#'   
#' ## Concluding Remarks
#'  I think we have a good group of people to discover some cool stuff in the trawl data set. What we have in this document is just the prelude -- I think the real exciting stuf will come once we have estimates of richness for the trawl data that we trust.  I'm looking foward to hearing your thoughts and working with all of you on this project!
#'   

#' \FloatBarrier   
#' 
#' ***  
#' 


# ==========================================
# = Report Generation Checks, Summary Info =
# ==========================================
#' #<u>Report Generation Notes</u>


# ================
# = Session Info =
# ================
#' ##R Session Information
#+ session.info, include=TRUE, echo=F
sessionInfo()


# =================
# = Date Compiled =
# =================
#' ##Date Document Last Compiled
#+ dateCompiled, include=TRUE, echo=F
message("Last compiled on: ",as.character(Sys.Date()),"\n\n")

#' 
#' ***  
#' 