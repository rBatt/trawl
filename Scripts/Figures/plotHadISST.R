
library(circular)
library(raster)
library(SDMTools)


load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/HadISST_tempGrads.RData")

heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.75,0.5,0))

# dev.new(width=5, height=7.5)
png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/HadISST_Figures/HadISST_tempGrads.png", res=200, width=5, height=7.5, units="in")
par(mfrow=c(4,1), mar=c(2,2,0.5,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")

plot(sst.mu, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Average~temperature~(degree*C)), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(timeTrend, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Temporal~gradient~(degree*C/yr)), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(spatGrad.slope, col=heat.cols, smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Spatial~gradient~(degree*C/km)), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(spatGrad.aspect, col=circular.colors(256), smallplot=smplt, bigplot=bgplt, axis.args=axargs)
mtext(bquote(Angle~of~spatial~gradient~(360*degree==N)), side=3, line=-1.5, cex=1, xpd=FALSE)
dev.off()



# dev.new(width=5, height=7.5)
png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/HadISST_Figures/HadISST_categories.png", res=200, width=5, height=8.5, units="in")
par(mfrow=c(5,1), mar=c(2,2,0.5,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")


plot(n.end==0, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Source
mtext(bquote(Source), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(n.end>0.45 & n.start<0.15, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Sink
mtext(bquote(Sink), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(n.ft>0.7 & n.end>0, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Corridor
mtext(bquote(Corridor), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(n.end>n.start & !(n.ft>0.7 & n.end>0) & !(n.end>0.45 & n.start<0.15), smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Divergence
mtext(bquote(Divergence), side=3, line=-1.5, cex=1, xpd=FALSE)

par(cex=1)
plot(n.end<n.start & !(n.ft>0.7 & n.end>0) & n.end!=0, smallplot=smplt, bigplot=bgplt, axis.args=axargs) # Convergence
mtext(bquote(Convergence), side=3, line=-1.5, cex=1, xpd=FALSE)

dev.off()

