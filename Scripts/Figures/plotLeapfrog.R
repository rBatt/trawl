
# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/frog_shifts.RData")
library(data.table)
library(maps)

# ===========================
# = Load Scripts/ Functions =
# ===========================
# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))
lu <- function(x) length(unique(x))

# Load plottign functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))

# Load statistics functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))




abline.mod <- function(x,y){
	abmod <- lm(y~x)
	# Plot Lines
	if(!is.na(abmod$coef[2])){
		pval <- summary(abmod)$coef[2,4]
		if(pval<0.05){
			abline(abmod, col="red", lwd=2)
		}else{
			abline(abmod, col="red", lwd=0.5)
		
		}
		if(pval<0.005){
			abline(abmod, col="red", lwd=2)
			abline(abmod, col="white", lty="dashed", lwd=1)
		}
	}
}

# ==============================================
# = Begin plotting everything you can think of =
# ==============================================

# ==============
# = Trajectory =
# ==============
# comMatch[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
# setkey(comMatch, s.reg, stratum)
# dev.new()
# par(mfrow=c(1,1), mar=c(1.5,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
# comMatch[,
# 	j={
#
#
# 		strat.cols1 <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(lu(stratum))
# 		strat.cols1.5 <- rgb(t(col2rgb(strat.cols1)), alpha=100, maxColorValue=255)
# 		strat.cols2 <- rgb(t(col2rgb(strat.cols1)), alpha=45, maxColorValue=255)
# 		strat.chars <- as.character(as.numeric(unique(stratum)))
# 		names(strat.cols1) <- unique(stratum)
# 		names(strat.cols2) <- unique(stratum)
# 		names(strat.chars) <- unique(stratum)
#
# 		# ==================================
# 		# = Plot 1: Community Trajectories =
# 		# ==================================
#
# 		plot(com.lon.t, com.lat.t, type="n")
# 		.SD[,
# 			j={
# 				lines(com.lon.t, com.lat.t, type="l", col=strat.cols2[stratum])
#
# 			},
# 			by=c("stratum")
# 		]
# 		.SD[,
# 			points(jitter(rev(com.lon.t)[1], factor=0.15), jitter(rev(com.lat.t)[1], factor=0.15), col="black", bg=strat.cols1.5[stratum], pch=21, cex=2),
# 			by=c("stratum")
# 		]
#
# 		points(unique(strat.lon.0), unique(strat.lat.0), col=strat.cols1, bg=strat.cols2, cex=3, pch=21)
# 		text(unique(strat.lon.0), unique(strat.lat.0), col="black", cex=1, labels=strat.chars, font=1)
#
#
# 	},
#
# 	by=c("s.reg")
# ]




gray2 <- rgb(t(col2rgb("black")), alpha=150, maxColorValue=255)

# ===============================
# = Community Lat vs. Stemp Lat =
# ===============================
shifts[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(shifts, s.reg, stratum)

# Create community / stemp velocity device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comStemp.vel.dev <- dev.cur()

# Create com/stemp residual from 1:1 device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comStemp.res11.dev <- dev.cur()

# Create com/stemp residual from regression line device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comStemp.resFit.dev <- dev.cur()

# Create net change device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
netChange.dev <- dev.cur()

# Create mean dt change device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
tChange.dev <- dev.cur()


shifts[,
	j={		
		com.lat.shift <- .SD[,list(comShift=mean(com.lat.t-strat.lat.0, na.rm=TRUE)), by=c("stratum")][,comShift]
		stemp.lat.shift <- .SD[,list(stempShift=mean(stemp.lat.t-strat.lat.0, na.rm=TRUE)), by=c("stratum")][,stempShift] # stemp.lat.t- strat.lat.0
		dComp.t.strat <- .SD[,list(comShift=mean(dComp.t, na.rm=TRUE)), by=c("stratum")][,comShift] # should maybe divid this by the absolute value of dStemp.t.strat, because the best matching community could change a lot if the best matching temperature changed a lot
		dStemp.t.strat <- .SD[,list(comShift=mean(dStemp.t, na.rm=TRUE)), by=c("stratum")][,comShift]
		t.strata <- .SD[,unique(year)]
		
		dStemp.net.strat <- .SD[,list(netStemp=mean(dStemp.net, na.rm=TRUE)), by=c("stratum")][,netStemp]
		dComp.net.strat <- .SD[,list(netComp=mean(dComp.net, na.rm=TRUE)), by=c("stratum")][,netComp]
		
		dStemp.t.strat <- .SD[,list(tStemp=mean(dStemp.t, na.rm=TRUE)), by=c("stratum")][,tStemp]
		dComp.t.strat <- .SD[,list(tComp=mean(dComp.t, na.rm=TRUE)), by=c("stratum")][,tComp]
		
		
		mod.mat <- matrix(c(com.lat.shift,stemp.lat.shift),ncol=2)
		# sing.check <- det(t(mod.mat)%*%mod.mat) < 1E-9
		na.check <- sum(complete.cases(mod.mat)) > 2
		# Regressions
		if(na.check){
			t.mod <- lm(com.lat.shift~stemp.lat.shift)
			resFit.com.lat.shift <- abs(com.lat.shift - as.numeric(predict(t.mod)))
			res11.com.lat.shift <- abs(com.lat.shift - stemp.lat.shift)
		}
		
		print(s.reg)
		print(summary(t.mod))		
		
		# First Figure
		dev.set(comStemp.vel.dev)
		# Plot Blank
		if(na.check){
			plot(stemp.lat.shift, com.lat.shift, ylab="", xlab="", pch=21, bg=gray2, col=NA, type="n", main=s.reg)
			abline.mod(stemp.lat.shift, com.lat.shift)
			r2 <- round(summary(t.mod)$r.squ,2)
			legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		# Plot Lines	
		abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
		# Plot Real
		if(na.check){
			points(stemp.lat.shift, com.lat.shift, pch=21, bg=gray2, col=NA)
		}else{
			points(1, xaxt="n",yaxt="n", pch="NA", bg=gray2, col=NA)	
		}
		
		
		
		# Second Figure
		dev.set(comStemp.res11.dev)
		if(na.check){
			plot(res11.com.lat.shift, dComp.t.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(res11.com.lat.shift, dComp.t.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		
		
		# Third Figure
		dev.set(comStemp.resFit.dev)
		if(na.check){
			plot(resFit.com.lat.shift, dComp.t.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(resFit.com.lat.shift, dComp.t.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		

		# Fourth Figure
		dev.set(netChange.dev)
		if(na.check){
			plot(dStemp.net.strat, dComp.net.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(dStemp.net.strat, dComp.net.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		# Fifth Figure
		dev.set(tChange.dev)
		if(na.check){
			plot(dStemp.t.strat, dComp.t.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(dStemp.t.strat, dComp.t.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		
		
		# print(summary(lm(com.lat.shift~stemp.lat.shift)))
		# print(summary(lm(com.lat.shift~stemp.lat.shift*strat.lat.0)))
	
	},
	
	by=c("s.reg")
]

# First Figure
dev.set(comStemp.vel.dev)
mtext(bquote(Com~(degree*N/yr)), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Stemp~(degree*N/yr)), side=1, line=-0.15, outer=TRUE)
# dev.off(comStemp.vel.dev)


# Second Figure
dev.set(comStemp.res11.dev)
mtext(bquote(Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Deviation~from~Stemp~Velocity~(degree*N/yr)), side=1, line=-0.15, outer=TRUE)
# dev.off(comStemp.res11.dev)


# Third Figure
dev.set(comStemp.resFit.dev)
mtext(bquote(Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Deviation~from~Predicted~Velocity~~(degree*N/yr)), side=1, line=-0.15, outer=TRUE)
# dev.off(comStemp.resFit.dev)

# Fourth Figure
dev.set(netChange.dev)
mtext(bquote(Net~Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Net~Stemp~Change~(degree*C)), side=1, line=-0.15, outer=TRUE)
# dev.off(netChange.dev)

# Fifth Figure
dev.set(tChange.dev)
mtext(bquote(Between-Year~Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Between-Year~Stemp~Change~(degree*C)), side=1, line=-0.15, outer=TRUE)
# dev.off(tChange.dev)





# ===============================
# = Community Lat vs. Btemp Lat =
# ===============================
shifts[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(shifts, s.reg, stratum)

# Create community / btemp velocity device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comBtemp.vel.dev <- dev.cur()

# Create com/btemp residual from 1:1 device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comBtemp.res11.dev <- dev.cur()

# Create com/btemp residual from regression line device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comBtemp.resFit.dev <- dev.cur()

# Create net change device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
netChange.dev <- dev.cur()

# Create mean dt change device
dev.new(width=3.5, height=6.5)
# png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
tChange.dev <- dev.cur()


shifts[,
	j={		
		com.lat.shift <- .SD[,list(comShift=mean(com.lat.t-strat.lat.0, na.rm=TRUE)), by=c("stratum")][,comShift]
		btemp.lat.shift <- .SD[,list(btempShift=mean(btemp.lat.t-strat.lat.0, na.rm=TRUE)), by=c("stratum")][,btempShift] # btemp.lat.t- strat.lat.0
		dComp.t.strat <- .SD[,list(comShift=mean(dComp.t, na.rm=TRUE)), by=c("stratum")][,comShift] # should maybe divid this by the absolute value of dBtemp.t.strat, because the best matching community could change a lot if the best matching temperature changed a lot
		dBtemp.t.strat <- .SD[,list(comShift=mean(dBtemp.t, na.rm=TRUE)), by=c("stratum")][,comShift]
		t.strata <- .SD[,unique(year)]
		
		dBtemp.net.strat <- .SD[,list(netBtemp=mean(dBtemp.net, na.rm=TRUE)), by=c("stratum")][,netBtemp]
		dComp.net.strat <- .SD[,list(netComp=mean(dComp.net, na.rm=TRUE)), by=c("stratum")][,netComp]
		
		dBtemp.t.strat <- .SD[,list(tBtemp=mean(dBtemp.t, na.rm=TRUE)), by=c("stratum")][,tBtemp]
		dComp.t.strat <- .SD[,list(tComp=mean(dComp.t, na.rm=TRUE)), by=c("stratum")][,tComp]
		
		
		mod.mat <- matrix(c(com.lat.shift,btemp.lat.shift),ncol=2)
		# sing.check <- det(t(mod.mat)%*%mod.mat) < 1E-9
		na.check <- sum(complete.cases(mod.mat)) > 2
		# Regressions
		if(na.check){
			t.mod <- lm(com.lat.shift~btemp.lat.shift)
			resFit.com.lat.shift <- abs(com.lat.shift - as.numeric(predict(t.mod)))
			res11.com.lat.shift <- abs(com.lat.shift - btemp.lat.shift)
		}
		
		print(s.reg)
		print(summary(t.mod))		
		
		# First Figure
		dev.set(comBtemp.vel.dev)
		# Plot Blank
		if(na.check){
			plot(btemp.lat.shift, com.lat.shift, ylab="", xlab="", pch=21, bg=gray2, col=NA, type="n", main=s.reg)
			abline.mod(btemp.lat.shift, com.lat.shift)
			r2 <- round(summary(t.mod)$r.squ,2)
			legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		# Plot Lines	
		abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
		# Plot Real
		if(na.check){
			points(btemp.lat.shift, com.lat.shift, pch=21, bg=gray2, col=NA)
		}else{
			points(1, xaxt="n",yaxt="n", pch="NA", bg=gray2, col=NA)	
		}
		
		
		
		# Second Figure
		dev.set(comBtemp.res11.dev)
		if(na.check){
			plot(res11.com.lat.shift, dComp.t.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(res11.com.lat.shift, dComp.t.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		
		
		# Third Figure
		dev.set(comBtemp.resFit.dev)
		if(na.check){
			plot(resFit.com.lat.shift, dComp.t.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(resFit.com.lat.shift, dComp.t.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		

		# Fourth Figure
		dev.set(netChange.dev)
		if(na.check){
			plot(dBtemp.net.strat, dComp.net.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(dBtemp.net.strat, dComp.net.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		# Fifth Figure
		dev.set(tChange.dev)
		if(na.check){
			plot(dBtemp.t.strat, dComp.t.strat, ylab="",xlab="", main=s.reg, lwd=1, pch=21, bg=gray2, col=NA)
			abline.mod(dBtemp.t.strat, dComp.t.strat)
			
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=s.reg)	
		}
		
		
		
		# print(summary(lm(com.lat.shift~btemp.lat.shift)))
		# print(summary(lm(com.lat.shift~btemp.lat.shift*strat.lat.0)))
	
	},
	
	by=c("s.reg")
]

# First Figure
dev.set(comBtemp.vel.dev)
mtext(bquote(Com~(degree*N/yr)), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Btemp~(degree*N/yr)), side=1, line=-0.15, outer=TRUE)
# dev.off(comBtemp.vel.dev)


# Second Figure
dev.set(comBtemp.res11.dev)
mtext(bquote(Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Deviation~from~Btemp~Velocity~(degree*N/yr)), side=1, line=-0.15, outer=TRUE)
# dev.off(comBtemp.res11.dev)


# Third Figure
dev.set(comBtemp.resFit.dev)
mtext(bquote(Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Deviation~from~Predicted~Velocity~~(degree*N/yr)), side=1, line=-0.15, outer=TRUE)
# dev.off(comBtemp.resFit.dev)

# Fourth Figure
dev.set(netChange.dev)
mtext(bquote(Net~Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Net~Btemp~Change~(degree*C)), side=1, line=-0.15, outer=TRUE)
# dev.off(netChange.dev)

# Fifth Figure
dev.set(tChange.dev)
mtext(bquote(Between-Year~Community~Change), side=2, line=-0.15, outer=TRUE)
mtext(bquote(Between-Year~Btemp~Change~(degree*C)), side=1, line=-0.15, outer=TRUE)
# dev.off(tChange.dev)





# ===============================
# = Community Lat vs. Btemp Lat =
# ===============================
shifts[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(shifts, s.reg, stratum)
# dev.new(width=4, height=6)
png("~/Desktop/communityShift_vs_botTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.5,0.75,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
shifts[,
	j={

		
		strat.cols1 <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(lu(stratum))
		strat.cols1.5 <- rgb(t(col2rgb(strat.cols1)), alpha=100, maxColorValue=255)
		strat.cols2 <- rgb(t(col2rgb(strat.cols1)), alpha=45, maxColorValue=255)
		gray2 <- rgb(t(col2rgb("gray")), alpha=85, maxColorValue=255)
		strat.chars <- as.character(as.numeric(unique(stratum)))
		names(strat.cols1) <- unique(stratum)
		names(strat.cols2) <- unique(stratum)
		names(strat.chars) <- unique(stratum)
		
		com.lat.shift <- com.lat.t-strat.lat.0
		btemp.lat.shift <- btemp.lat.t-strat.lat.0
		
		if(!all(is.na(btemp.lat.shift))){
			plot(btemp.lat.shift, com.lat.shift, ylab=bquote(Com~(degree*N/yr)), xlab=bquote(Btemp~(degree*N/yr)), pch=21, bg=gray2, col=NA, main=s.reg)
		}else{
			plot(1, ylab=bquote(Com~(degree*N/yr)), xlab=bquote(Btemp~(degree*N/yr)), xaxt="n",yaxt="n", pch="NA", bg=gray2, col=NA, main=s.reg)	
		}
				
		pval <- summary(lm(com.lat.shift~btemp.lat.shift))$coef[2,4]
		
		
		
		if(pval<0.05){
			abline(a=0, b=1, col="red", lwd=3)
		}else{
			abline(a=0, b=1, col="red")
		}
		if(pval<0.005){
			abline(a=0, b=1, col="red", lwd=3)
			abline(a=0, b=1, col="white", lty="dashed", lwd=1.5)
		}
		
		
		
		
		
		print(summary(lm(com.lat.shift~btemp.lat.shift)))
		print(summary(lm(com.lat.shift~btemp.lat.shift*strat.lat.0)))
	
	},
	
	by=c("s.reg")
]
dev.off()







# ===============================================
# = Change in Community vs. Change in Btemp Lat =
# ===============================================
shifts[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(shifts, s.reg, stratum)
# dev.new(width=4, height=6)
png("~/Desktop/deltaCommunity_vs_botTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.5,0.75,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
shifts[,
	j={

		
		strat.cols1 <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(lu(stratum))
		strat.cols1.5 <- rgb(t(col2rgb(strat.cols1)), alpha=100, maxColorValue=255)
		strat.cols2 <- rgb(t(col2rgb(strat.cols1)), alpha=45, maxColorValue=255)
		gray2 <- rgb(t(col2rgb("gray")), alpha=85, maxColorValue=255)
		strat.chars <- as.character(as.numeric(unique(stratum)))
		names(strat.cols1) <- unique(stratum)
		names(strat.cols2) <- unique(stratum)
		names(strat.chars) <- unique(stratum)
		
		btemp.lat.shift <- btemp.lat.t-strat.lat.0
		
		if(!all(is.na(btemp.lat.shift))){
			plot(btemp.lat.shift, dComp.t, ylab=bquote(delta*Com~(degree*N/yr)), xlab=bquote(Btemp~(degree*N/yr)), pch=21, bg=gray2, col=NA, main=s.reg)
		}else{
			plot(1, ylab=bquote(delta*Com~(degree*N/yr)), xlab=bquote(Btemp~(degree*N/yr)), xaxt="n",yaxt="n", pch="NA", bg=gray2, col=NA, main=s.reg)	
		}
				
		pval <- summary(lm(dComp.t~btemp.lat.shift))$coef[2,4]
		
		
		
		if(pval<0.05){
			abline(a=0, b=1, col="red", lwd=3)
		}else{
			abline(a=0, b=1, col="red")
		}
		if(pval<0.005){
			abline(a=0, b=1, col="red", lwd=3)
			abline(a=0, b=1, col="white", lty="dashed", lwd=1.5)
		}
		
		print(summary(lm(dComp.t~btemp.lat.shift)))
		print(summary(lm(dComp.t~btemp.lat.shift*strat.lat.0)))
	
	},
	
	by=c("s.reg")
]
dev.off()






# =======================================
# = Plot map of arrivals and departures =
# =======================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

# Community Arrivals
shifts[,comArrive.net.final:=comArrive.net[which.max(year)], by=c("s.reg","stratum")]

shifts[,comArrive.net.final.col:=heat.cols[cut(comArrive.net.final, 256)]]

dev.new(height=4, width=shifts[,map.w(strat.lat.0,strat.lon.0,4)])
# pdf(height=4, width=shifts[,map.w(strat.lat.0,strat.lon.0,4)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/shifts_now.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")

shifts[,plot(strat.lon.0, strat.lat.0, col=comArrive.net.final.col, pch=21, cex=1, type="n")]
invisible(shifts[,map(add=TRUE, fill=FALSE, col="black")])

shifts[,points(strat.lon.0, strat.lat.0, col=comArrive.net.final.col, pch=21, cex=1)]

shifts[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]

shifts[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
shifts[,text(-167, y=seq(30,40, length.out=4), round(seq(min(comArrive.net.final), max(comArrive.net.final), length.out=4),4), adj=1, cex=1, col="black")]

shifts[,text(-162.5, 41.5, bquote("#"~Community~Arrivals))]




# Stemp Arrivals
shifts[,stempArrive.net.final:=stempArrive.net[which.max(year)], by=c("s.reg","stratum")]

shifts[,stempArrive.net.final.col:=heat.cols[cut(stempArrive.net.final, 256)]]

dev.new(height=4, width=shifts[,map.w(strat.lat.0,strat.lon.0,4)])
# pdf(height=4, width=shifts[,map.w(strat.lat.0,strat.lon.0,4)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/shifts_now.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")

shifts[,plot(strat.lon.0, strat.lat.0, col=stempArrive.net.final.col, pch=21, cex=1, type="n")]
invisible(shifts[,map(add=TRUE, fill=FALSE, col="black")])

shifts[,points(strat.lon.0, strat.lat.0, col=stempArrive.net.final.col, pch=21, cex=1)]

shifts[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]

shifts[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
shifts[,text(-167, y=seq(30,40, length.out=4), round(seq(min(stempArrive.net.final), max(stempArrive.net.final), length.out=4),4), adj=1, cex=1, col="black")]

shifts[,text(-162.5, 41.5, bquote("#"~Stemp~Arrivals))]


