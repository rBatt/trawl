
# ==================
# = Load Libraries =
# ==================
library(circular)
library(raster)
library(SDMTools)
library(maps)


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


# ================
# = Load Results =
# ================
load("./trawl/Results/HadISST/HadISST_tempGrads.RData")
load("./trawl/Results/HadISST/HadISST_categories.shelf.RData")
load("./trawl/Results/HadISST/HadISST_trajectoriesImage.shelf.RData")

# =========================
# = Set up Figure Options =
# =========================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.75,0.5,0))

# ===============
# = Make figure =
# ===============
# NOTE: Redundant with the plot in plotHadISST.R
# Set up figure dimensions
# dev.new(width=3.5, height=7.5)
png("./trawl/Figures/HadISST_Figures/HadISST_tempGrads.shelf.png", res=200, width=3.5, height=7.5, units="in")
par(mfrow=c(5,1), mar=c(2,2,2,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")

# Plot average temperature
plot(sst.mu, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Avg~temperature~(degree*C)), side=3, line=-2.75, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Plot temporal gradient
par(cex=1)
plot(timeTrend, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Time~gradient~(degree*C/yr)), side=3, line=-2.75, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Plot spatial gradient
par(cex=1)
# plot(spatGrad.slope, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
plot(spatSlope, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Space~gradient~(degree*C/km)), side=3, line=-2.75, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Plot spatial angle
par(cex=1)
plot(spatGrad.aspect, col=circular.colors(256), smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Space~Angle~(360*degree==N)), side=3, line=-2.75, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Plot velocity
par(cex=1)
plot(climV, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Velocity~(km/yr)), side=3, line=-2.75, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

dev.off()


# ==============================
# = Plot Trajectory Categories =
# ==============================
# dev.new(width=5, height=7.5)
png("./trawl/Figures/HadISST_Figures/HadISST_categories.shelf.png", res=200, width=5, height=8.5, units="in")
par(mfrow=c(5,1), mar=c(2,2,0.5,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")

# Source
plot(cSource, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Source
mtext(bquote(Source), side=3, line=-2.5, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Divergence
par(cex=1)
plot(cDivergence, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Sink
mtext(bquote(Divergence), side=3, line=-2.5, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Corridor
par(cex=1)
plot(cCorridor, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Corridor
mtext(bquote(Corridor), side=3, line=-2.5, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Convergence
par(cex=1)
plot(cConvergence, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Divergence
mtext(bquote(Convergence), side=3, line=-2.5, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

# Sink
par(cex=1)
plot(cSink, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Convergence
mtext(bquote(Sink), side=3, line=-2.5, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))

dev.off()



# =======================
# = Combined Categories =
# =======================
cCat <- cSource*1 + cDivergence*2 + cCorridor*3 + cConvergence*4 + cSink*5
axargs2 <- list(mgp=c(0.75,0.5,0), at=0:5, labels=c("none","source","diverge","corridor","converge","sink"))
col5 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")


# dev.new(width=9.5, height=3)
smplt <- c(0.9,0.92, 0.15,0.8)
bgplt <- c(0.03,0.89,0.1,0.95)
axargs <- list(mgp=c(0.75,0.5,0))

png("./trawl/Figures/HadISST_Figures/HadISST_combinedCategories.shelf.png", res=200, width=9.75, height=3, units="in")
par(mfrow=c(1,1), mar=c(2,2,0.5,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")
plot(cCat, smallplot=smplt, bigplot=bgplt, axis.args=axargs2, col=col5)
invisible(map(add=TRUE, fill=FALSE, col="black", lwd=0.5))
dev.off()





# =============================
# = Plot Climate Trajectories =
# =============================
width <- map.w(c(ymin(trajLon),ymax(trajLon)), c(xmin(trajLon),xmax(trajLon)), 3)
png("./trawl/Figures/HadISST_Figures/HadISST_trajectories.shelf.png", res=200, width=width, height=3, units="in")
par(mfrow=c(1,1), mar=c(2,2,0.5,0.1), ps=8, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")
plot.traj(trajLon, trajLat, col="slategray", thin.rate=2, nearB=0.01, thinDots=F, adjArr=0.5, cex=0.1, xlab="", ylab="")
dev.off()


# =====================================
# = Plot Trajectories over Categories =
# =====================================
# dev.new(width=9.5*2, height=3*2)
png("./trawl/Figures/HadISST_Figures/HadISST_cats.trajs.shelf.png", res=200, width=9, height=3.72, units="in")
# dev.new(width=9, height=3.72)
par(mfrow=c(1,1), mar=c(2,2,0.5,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")
plot(cCat, smallplot=smplt, bigplot=bgplt, axis.args=axargs2, col=col5, interpolate=TRUE, xlab="", ylab="")
plot.traj(trajLon, trajLat, add=TRUE, col="white", thin.rate=4, nearB=0.01, adjArr=0.5, thinDots=TRUE, xlab="", ylab="")
dev.off()



# =========================
# = Plot Climate Velocity =
# =========================
smplt <- c(0.94,0.96, 0.15,0.8)
bgplt <- c(0.03,0.93,0.1,0.95)
axargs <- list(mgp=c(0.75,0.5,0))
png("./trawl/Figures/HadISST_Figures/climateVeloicty.png", res=200, width=6.9, height=3, units="in")
par(mfrow=c(1,1), mar=c(2,2,0.5,0.1), ps=8, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times", cex=1)
plot(climV, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Climate~Velocity~(km~year^-1)), side=3, line=-2.75, cex=1, xpd=FALSE)
invisible(map(add=TRUE, fill=FALSE, col="black"))
dev.off()





wd.old <- getwd()
setwd("./trawl/Figures/HadISST_Figures/")
saveGIF(
	{
		ani.options(inverval=0.05)

	    for(i in seq(1, nlayers(trajLon), by=5)){
			par(mar=rep(0.5,4), oma=rep(0,4))
	    	plot(subset(trajLon, i), subset(trajLat, i), ylab="", xlab="", xaxt="n", yaxt="n", col="blue", bty="n", cex=0.25, pch=20)
	    }
	
	},
	ani.height=400,
	ani.width=map.w(c(ymin(trajLon),ymax(trajLon)),c(xmin(trajLon),xmax(trajLon)),400),
	movie.name="climateTrajectory_dots_easySee.gif",
)
setwd(wd.old)