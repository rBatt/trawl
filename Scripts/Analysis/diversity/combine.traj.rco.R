



# =================
# = Load Packages =
# =================
library(data.table)
library(fields)
library(raster)
library(reshape2)



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



# ==========================
# = Load Processed Results =
# ==========================
load("./trawl/Results/Richness/rco.RData")
load("./trawl/Results/Richness/rco.s.RData")
load("./trawl/Results/HadISST/HadISST_trajectoriesImage.shelf.RData")


# =========================================
# = Aggregate Climate Variables over Time =
# =========================================
# agInd <- rep(1:(nlayers(trajStop)/n.per.yr), each=n.per.yr)
# trajStop.ag <- stackApply(trajStop, agInd, mean)
# trajStop.ag[cover.type.s<4] <- trajStop.ag[cover.type.s<4] - 1



# =========================================
# = Add Trajectory end points to Richness =
# =========================================
# trajStop.ag2 <- aggregate(trajStop.ag, c(2,2), sum)
# traj.dt0 <- data.table(coordinates(trajStop.ag2) , values(trajStop.ag2))
# setnames(traj.dt0, names(traj.dt0), c("lon", "lat", 1968:2013))

#
# traj.dt <- melt(traj.dt0, id.vars=c("lon","lat"), variable.name="year",value.name="nEnd")
#
# setkey(traj.dt, lon, lat, year)
# setkey(rbo.Z, lon, lat, year)
#
# richTraj <- traj.dt[rbo.Z]
#

# ====================================
# = Climate Trajectory Ending Values =
# ====================================
# Basically a summary of what happened up to to the final time step – so no time series here
climTraj <- data.table(
	coordinates(n.end), 
	nFinal=values(n.end), 
	nFT=values(n.ft), 
	nDenom=values(n.denom), 
	nStart=values(n.start), 
	timeTrend=values(aggregate(timeTrend, c(2,2), mean)),
	climV=values(aggregate(climV, c(2,2), mean))
)
setnames(climTraj, c("x","y"), c("lon","lat"))
climTraj[,categ:=burrow.cat(nStart, nFinal, nFT)]

# ============================
# = Combine Climate w/ rco.s =
# ============================
# to compare climate velocity, temp trends, and richness trends per site
# these do not contain a time component
# they are no species-specific
# they are per-stratum, though

setkey(rco.s, lon, lat)
setkey(climTraj, lon, lat)

cT.rcoS <- merge(rco.s, climTraj)



cT.rcoS[,plot(data.frame(slope.N, slope.Nsite, mu.N, mu.Nsite, slope.btemp, timeTrend, lon, lat, climV, nFT,nFinal, nStart), col=as.factor(s.reg), cex=0.5)]




save(cT.rcoS, file="./trawl/Results/Richness/cT.rcoS.RData")
