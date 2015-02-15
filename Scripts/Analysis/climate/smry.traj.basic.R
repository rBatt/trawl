



# ==================
# = Load libraries =
# ==================
library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)
library(raster)
library(SDMTools)


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


# =======================
# = Load trawl2 Dataset =
# =======================
load("./trawl/Results/HadISST/HadISST_trajectoriesImage.shelf.RData")
load("./trawl/Results/Richness/rbo.N.RData")
load("./trawl/Results/Richness/rbo.Z.RData")

# =======================
# = Load trawl2 Dataset =
# =======================
load("./trawl/Data/trawl2.RData")


# ======================
# = Remove unknown spp =
# ======================
trawl2.veri <- trawl2[(correctSpp)&!is.na(common)&is.species(spp),]


# ============================
# = Aggregate Stops per Year =
# ============================

agInd <- rep(1:(nlayers(trajStop)/n.per.yr), each=n.per.yr)
trajStop.ag <- stackApply(trajStop, agInd, mean)

trajStop.ag[cover.type.s<4] <- trajStop.ag[cover.type.s<4] - 1
#
# heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
# for(i in 1:nlayers(trajStop.ag)){
# 	plot(sqrt(subset(trajStop.ag,i)), col=heat.cols, zlim=c(0,10))
# }


# =========================================
# = Add Trajectory end points to Richness =
# =========================================
trajStop.ag2 <- aggregate(trajStop.ag, c(2,2), sum)
traj.dt0 <- data.table(coordinates(trajStop.ag2) , values(trajStop.ag2))
setnames(traj.dt0, names(traj.dt0), c("lon", "lat", 1968:2013))
traj.dt <- melt(traj.dt0, id.vars=c("lon","lat"), variable.name="year",value.name="nEnd")

setkey(traj.dt, lon, lat, year) 
setkey(rbo.Z, lon, lat, year)

richTraj <- traj.dt[rbo.Z]



# =======================
# = Aggregate By Region =
# =======================
richTraj.reg <- richTraj[,list(nEnd=mean(nEnd),N=mean(N),n.slope=mean(n.slope),Z=mean(Z),z.slope=mean(z.slope)), by=c("s.reg","year")]
richTraj.reg[,year:=as.numeric(as.character(year))]
setkey(richTraj.reg, s.reg, year)


richTraj.reg[,Z.sc:=scale(Z)[,1], by="s.reg"]
richTraj.reg[,del.Z.sc:=c(0,diff(Z.sc)), by="s.reg"]



# ========================
# = Plot Stops over Time =
# ========================

# =================
# = Plot Smooth Z =
# =================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
# smooZ[,z.col:=heat.cols[cut(Z, 256)]]
lim.z <- range(values(trajStop.ag))
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.75,0.5,0))

old.wd <- getwd()
setwd("./trawl/Figures/HadISST_Figures/")
saveGIF(
	{
		ani.options(inverval=0.1)
		for(i in 1:nlayers(trajStop.ag)){
			par(mfrow=c(1,1), mar=c(2,2,0.5,0.1), ps=10, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")
			plot(sqrt(subset(trajStop.ag,i)), col=heat.cols, zlim=c(-1,10), smallplot=smplt, bigplot=bgplt, axis.args=axargs)
			text(-100,40, labels=(1968:2013)[i], cex=2)
			invisible(map(add=TRUE, fill=FALSE, lwd=0.5))
		}
	
	},
	ani.height=400,
	ani.width=map.w(c(ymin(trajStop.ag),ymax(trajStop.ag)),c(xmin(trajStop.ag),xmax(trajStop.ag)),400),
	movie.name="climate_trajectory_numEnd.gif",
)
setwd(old.wd)








richTraj[,
	j={
		
		mean(nEnd)
		
	},
	by=c("s.reg","year")
]






# png("./trawl/Figures/Diversity/msom.basic.N.ts.png", width=4, height=8, res=200, units="in")
dev.new(width=4, height=8)
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		
		plot(year, nEnd, type="o", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]
# dev.off()

dev.new(width=4, height=8)
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		
		plot(year, N, type="o", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]



dev.new(width=4, height=8)
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		
		plot(year, Z, type="o", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]






richTraj.reg[,plot(nEnd, del.Z.sc, col=as.factor(s.reg))]
richTraj.reg[,plot(nEnd, Z.sc, col=as.factor(s.reg))]
richTraj.reg[,plot(nEnd, Z, col=as.factor(s.reg))]



dev.new(width=4, height=8)
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		
		plot(year, Z.sc, type="o", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]




# ==============================
# = Richness by nEnd by Region =
# ==============================
dev.new(width=4, height=8)
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		
		plot(nEnd, Z, type="p", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]


# ===================================================
# = Trend of Richness vs. Trend of Trajectories End =
# ===================================================
par(mfrow=c(1,1), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		
		nEnd.slope <- .SD[,{lm(nEnd~year)$coef[2]}, by="s.reg"][,V1]
		print(blah)
		Z.slope <- .SD[,{lm(Z~year)$coef[2]}, by="s.reg"][,V1]
		plot(nEnd.slope, Z.slope)
	}
]



# =========================================================
# = Correlation between nEnd and Richnes vs Slope of nEnd =
# =========================================================
# idea being that nEnd might only correlated with richness if a lot changed in nEnd
richTraj.reg[,
	j={
		
		cor.endZ <- .SD[,{cor(nEnd,Z)}, by="s.reg"][,V1]
		endSlope <- .SD[,{lm(nEnd~year)$coef[2]}, by="s.reg"][,V1]
		plot(endSlope, cor.endZ)
		cor(endSlope, cor.endZ)
	}
]


# =======================================================
# = Plot observed richness vs. estimated/ true richenss =
# =======================================================

obsZ <- trawl2.veri[,list(obs.Z=lu(spp)), by=c("s.reg","year","stratum")][,list(mean.obsZ=mean(obs.Z)), by=c("s.reg","year")]



richTraj.reg <- richTraj.reg[obsZ]

richTraj.reg[,plot(mean.obsZ, Z)]

dev.new(width=4, height=8)
par(mfrow=c(6,2), mar=c(1.5,1.5,0.1,1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
richTraj.reg[,
	j={
		par(mar=c(1.5,1.5,0.1,1.5), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
		plot(year, mean.obsZ, type="l", xlab="", ylab="")
		
		par(new=TRUE)
		# par(mar=c(1.5,1.5,0.1,1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
		plot(year, Z, type="l", xlab="", ylab="", xaxt="n", yaxt="n", col="red")
		axis(side=4)
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]



col11 <- rainbow(11) #c("#a6cee3","#1f78b4","#b2df8a","#33a02c","#fb9a99","#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99")
ylim <- richTraj.reg[,range(c(mean.obsZ,Z))]
xlim <- richTraj.reg[,range(year)]
ureg <- richTraj.reg[,unique(s.reg)]

richTraj.reg[,
	{
		for(i in 1:length(ureg)){
			if(i==1){
				.SD[s.reg==ureg[i],plot(year, mean.obsZ, type="l", ylim=ylim, xlim=xlim, col=col11[i], lwd=3)]
				.SD[s.reg==ureg[i],lines(year, Z, col=col11[i], lwd=3)]
				.SD[s.reg==ureg[i],lines(year, Z, lwd=1)]
			}else{
				.SD[s.reg==ureg[i],lines(year, mean.obsZ, col=col11[i], lwd=3)]
				.SD[s.reg==ureg[i],lines(year, Z, col=col11[i], lwd=3)]
				.SD[s.reg==ureg[i],lines(year, Z, lwd=1)]
			}
		}	
	}

]



rcols <- rainbow(11)