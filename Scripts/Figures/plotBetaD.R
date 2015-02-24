


# ==================
# = Load libraries =
# ==================
library(maps)
library(data.table)

# =========================
# = Load Analysis Results =
# =========================
load("~/Documents/School&Work/pinskyPost/trawl/Results/trawl.betaD.RData")
# trawl.betaD.RData contains 4 objects:
	# 1) beta.turn.space
	# 2) beta.var.space
	# 3) beta.turn.time
	# 4) beta.var.time


# ===========================
# = Load Scripts/ Functions =
# ===========================
# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))

# Load plottign functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))

# Load statistics functions
stat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

# divData2 <- divData[,list(stemp=meanna(stemp), btemp=meanna(btemp), depth=meanna(depth), wtcpue=meanna(wtcpue)), by=c("s.reg","stratum","spp","year","common")]


# ================================
# = Beta D as variance over time =
# ================================
# Create colors
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
beta.var.time[,var.time.col:=heat.cols[cut(var.time, 256)]]

# New device
# dev.new(height=4, width=beta.var.time[,map.w(lat,lon,4)])
png(height=4, width=beta.var.time[,map.w(lat,lon,4)], file="./trawl/Figures/betaD_temporalVariance.png", units="in", res=200)
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
beta.var.time[,plot(lon, lat, col=var.time.col, pch=21, cex=1, type="n")] # set plot region
invisible(beta.var.time[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
beta.var.time[,points(lon, lat, bg=var.time.col, pch=21, cex=1)] # add points

# Key
beta.var.time[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
beta.var.time[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
beta.var.time[,text(-167, y=seq(30,40, length.out=4), round(seq(min(var.time), max(var.time), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
beta.var.time[,text(-162.5, 41.5, bquote(Temporal~Variance))] # add label for key

#checking to make sure I get colors right
# dev.new(); beta.var.time[,plot(var.time, col=var.time.col)] # a plot of all the variances, with their colors
# beta.var.time[,quantile(1:256, probs=seq(0,1,length.out=4))] # this gives the indices of heat.cols where tick marks are located
# beta.var.time[,abline(h=round(seq(min(var.time), max(var.time), length.out=4),2), col=heat.cols[c(1,86,171,256)])] # these lines should match the colors through which they're drawn
dev.off()



# ========================================
# = Beta as diversity turnover over time =
# ========================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

beta.turn.time[,turn.time.col:=heat.cols[cut(exp(turn.time), 256)]]


# dev.new(height=4, width=beta.turn.time[,map.w(lat,lon,4)])
png(height=4, width=beta.var.time[,map.w(lat,lon,4)], file="./trawl/Figures/betaD_temporalTurnover.png", units="in", res=200)
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

beta.turn.time[,plot(lon, lat, bg=turn.time.col, pch=21, cex=1, type="n")]
invisible(beta.turn.time[,map(add=TRUE, fill=TRUE, col="lightgray")])

beta.turn.time[,points(lon, lat, bg=turn.time.col, pch=21, cex=1)]

beta.turn.time[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]

beta.turn.time[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
beta.turn.time[,text(-167, y=seq(30,40, length.out=4), round(exp(seq(min(turn.time), max(turn.time), length.out=4)),4), adj=1, cex=1, col="black")]

#checking to make sure I get colors right
# dev.new(); beta.turn.time[,plot(turn.time, col=turn.time.col)] # a plot of all the variances, with their colors
# beta.turn.time[,quantile(1:256, probs=seq(0,1,length.out=4))] # this gives the indices of heat.cols where tick marks are located
# beta.turn.time[,abline(h=round(seq(min(turn.time), max(turn.time), length.out=4),2), col=heat.cols[c(1,86,171,256)])] # these lines should match the colors through which they're drawn

beta.turn.time[,text(-162.5, 41.5, bquote(Temporal~Turnover~(log[e]~scale)))]
dev.off()

# =========================================
# = Beta diversity as variance over space =
# =========================================
# dev.new(width=5, height=7)
png(height=7, width=5, file="./trawl/Figures/betaD_spatialVariance.png", units="in", res=200)
par(mfrow=c(5,2), mar=c(1.75,1.5,1,1), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.var.space[,
	{
		plot(year, var.space, type="l", main=s.reg, xlab="", ylab="")
		},
	by="s.reg"
]
mtext("Year", side=1, line=-1, outer=TRUE, font=2)
mtext("Spatial Variance", side=2, line=-0.5, outer=TRUE, font=2)
dev.off()



# ============================================
# = Beta diversity as turnover through space =
# ============================================
# dev.new(width=5, height=7)
png(height=7, width=5, file="./trawl/Figures/betaD_spatialTurnover.png", units="in", res=200)
par(mfrow=c(5,2), mar=c(1.75,1.5,1,1), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.turn.space[,
	{
		plot(year, turn.space, type="l", main=s.reg, xlab="", ylab="")
		},
	by="s.reg"
]
mtext("Year", side=1, line=-1, outer=TRUE, font=2)
mtext("Spatial Turnover", side=2, line=-0.5, outer=TRUE, font=2)
dev.off()




