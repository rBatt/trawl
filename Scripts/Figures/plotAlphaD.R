

load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/alphaD.RData")

library(maps)



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


# =========================================
# = Plot trend in alpha diversity (slope) =
# =========================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

alphaD[,alpha.trend.col:=heat.cols[cut(alpha.trend, 256)]]

# dev.new(height=4, width=alphaD[,map.w(lat,lon,4)])
pdf(height=4, width=alphaD[,map.w(lat,lon,4)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/alphaD_temporalTrend.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

alphaD[,plot(lon, lat, col=alpha.trend.col, pch=21, cex=1, type="n")]
invisible(alphaD[,map(add=TRUE, fill=TRUE, col="lightgray")])

alphaD[,points(lon, lat, bg=alpha.trend.col, pch=21, cex=1)]

alphaD[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]

alphaD[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
alphaD[,text(-167, y=seq(30,40, length.out=4), round(seq(min(alpha.trend), max(alpha.trend), length.out=4),4), adj=1, cex=1, col="black")]

alphaD[,text(-162.5, 41.5, bquote(alpha~diversity~trend))]
dev.off()





# ===============================================
# = Plot prediction for current alpha diversity =
# ===============================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

alphaD[,alpha.now.col:=heat.cols[cut(alpha.now, 256)]]

# dev.new(height=4, width=alphaD[,map.w(lat,lon,4)])
pdf(height=4, width=alphaD[,map.w(lat,lon,4)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/alphaD_now.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

alphaD[,plot(lon, lat, col=alpha.now.col, pch=21, cex=1, type="n")]
invisible(alphaD[,map(add=TRUE, fill=TRUE, col="lightgray")])

alphaD[,points(lon, lat, bg=alpha.now.col, pch=21, cex=1)]

alphaD[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]

alphaD[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
alphaD[,text(-167, y=seq(30,40, length.out=4), round(seq(min(alpha.now), max(alpha.now), length.out=4),4), adj=1, cex=1, col="black")]

alphaD[,text(-162.5, 41.5, bquote(alpha~diversity~now))]
dev.off()

# dev.new()
# alphaD[,plot((alpha.now), (alpha.trend))]
# alphaD[,summary(lm(alpha.trend~alpha.now))]
#
#
# alpha.trend <- alphaD[,alpha.trend]
# alpha.now <- alphaD[,alpha.now]
# alpha.bc <- (boxcox(lm(alpha.now~alpha.trend)))
#
# alpha.lambda <- alpha.bc$x[which.max(alpha.bc$y)]
#
# plot(alpha.trend, alpha.now^alpha.lambda)
#
# alpha.now.xfrm <- alpha.now^alpha.lambda
# summary(lm(alpha.now.xfrm~alpha.trend))


