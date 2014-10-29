
# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/frog_shifts.RData")
load("~/Documents/School&Work/pinskyPost/trawl/Results/trawl.betaD.RData")
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
stat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# =================
# = Set Constants =
# =================
gray2 <- rgb(t(col2rgb("gray")), alpha=150, maxColorValue=255)
black2 <- rgb(t(col2rgb("black")), alpha=150, maxColorValue=255)
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
SaveFigs <- c(FALSE,TRUE)[2]

# pdf("~/Desktop/temp_dComp.t.t0_time.pdf", width=3.5, height=3.5)
# ==================================================
# = Create shifts.strat by averaging across years  =
# ==================================================
shifts.strat <- shifts[,
	j={	
		# Burrow Categories
		stemp.cat.0 <- table(stemp.cat)
		btemp.cat.0 <- table(btemp.cat)
		years.elapsed0 <- (year-min(year, na.rm=TRUE))
		years.elapsed <- years.elapsed0[-1]
		
		dBtemp.t.t0 <- btemp - btemp[1]
		dStemp.t.t0 <- stemp - stemp[1]
		
		# par(mfrow=c(2,1), mar=c(2,2,0.1,0.5), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10, family="Times")
		# plot(years.elapsed0, btemp, type="o", pch=19, col="blue", ylab="Temperature", xlab="Years since beginning", main=paste(region, stratum), ylim=range(c(btemp,stemp), na.rm=TRUE))
		# lines(years.elapsed0, stemp, type="o", pch=19, col="red")
		# # par(new=TRUE)
		# plot(years.elapsed0, dComp.t.t0.matched, type="o", pch=19, ylab="Community distance", xlab="Years since beginning", ylim=range(c(dComp.t.t0.matched, dComp.t.t0), na.rm=TRUE))
		# lines(years.elapsed0, dComp.t.t0, lty="dashed")
		
		list(
			lat=mean(strat.lat.0, na.rm=TRUE),
			lon=mean(strat.lon.0, na.rm=TRUE),
			
			# Lat-lon shifts
			com.dLon.t1.t=mean(com.dLon.t1.t/dYear.t1.t, na.rm=TRUE),
			com.dLat.t1.t=mean(com.dLat.t1.t/dYear.t1.t, na.rm=TRUE),
		
			btemp.dLon.t1.t=mean(btemp.dLon.t1.t/dYear.t1.t, na.rm=TRUE),
			btemp.dLat.t1.t=mean(btemp.dLat.t1.t/dYear.t1.t, na.rm=TRUE),
		
			stemp.dLon.t1.t=mean(stemp.dLon.t1.t/dYear.t1.t, na.rm=TRUE),
			stemp.dLat.t1.t=mean(stemp.dLat.t1.t/dYear.t1.t, na.rm=TRUE),
			
			
			# Most frequent burrow category
			# Do certain categories move in Lon/Lat faster? So if strat is divergence, does that mean its t1.t matches tend to be far away?
			# Also, do certain categories have a positive or negative *trend* in alphaD (need to merge in original alphaD analysis)?
			# Regions that are corridors might be the regions with high average temporal beta diversity (need to merge in original betaD analysis)
			stemp.cat=names(stemp.cat.0)[which.max(stemp.cat.0)],
			btemp.cat=names(btemp.cat.0)[which.max(btemp.cat.0)],
			
			
			# Average Burrow Trajectory
			# Could plot a map of each stratum's average % in each of the 3 cateogories (3 maps)
			# Instead of using categorical predictors for changes in alpha and beta and distance, like above, could use these values in regressions
			stemp.N.start=mean(stemp.N.start, na.rm=TRUE),
			stemp.N.end=mean(stemp.N.end, na.rm=TRUE),
			stemp.N.ft=mean(stemp.N.ft, na.rm=TRUE),
			
			btemp.N.start=mean(btemp.N.start, na.rm=TRUE),
			btemp.N.end=mean(btemp.N.end, na.rm=TRUE),
			btemp.N.ft=mean(btemp.N.ft, na.rm=TRUE),
			
			
			# Change from origin
			# Could make a map of each stratum's average minimum distance from its origin
			# Could also regress distance from origin against distance that the community traveled – are community's w/ a geographically distance match very different?
			# Should probably ask if the temperature in the originally community's match is very different from the original community's temperature
			dComp.t.t0.matched.perYear=mean(dComp.t.t0.matched[-1]/years.elapsed, na.rm=TRUE),
			com.dLon.t0.match=mean(com.dLon.t0.match[-1]/years.elapsed, na.rm=TRUE),
			com.dLat.t0.match=mean(com.dLat.t0.match[-1]/years.elapsed, na.rm=TRUE),
			
			
			# Alpha D Values
			# Does a good match in the community  mean good or bad things for alpha?
			# Do communities that do a good job of following temperature have higher alpha diversity?
			# Do certain trajectory categories have community matches that are good or bad for alpha diversity in the destination relative to the current?
			alphaD.t=mean(alphaD.t, na.rm=TRUE), # average alpha diversity in each region. Does trajectory category affect average alpha?
			dAlphaD.t1.t=mean(dAlphaD.t1.t/dYear.t1.t, na.rm=TRUE), # rate of change in alpha diversity as the community moves.
			alphaD.slope=summary(lm(alphaD.t~years.elapsed0))$coef[2,1], # does trajectory category affect trend in alpha?
			
			# Correlation of community and temperature shifts
			com.stemp.lon.cor=cor((com.dLon.t1.t/dYear.t1.t), (stemp.dLon.t1.t/dYear.t1.t), use="na.or.complete"),
			com.stemp.lat.cor=cor((com.dLat.t1.t/dYear.t1.t), (stemp.dLat.t1.t/dYear.t1.t), use="na.or.complete"),
			com.btemp.lon.cor=cor((com.dLon.t1.t/dYear.t1.t), (btemp.dLon.t1.t/dYear.t1.t), use="na.or.complete"),
			com.btemp.lat.cor=cor((com.dLat.t1.t/dYear.t1.t), (btemp.dLat.t1.t/dYear.t1.t), use="na.or.complete"),
			
			# Closest community analog
			dComp.t.t0.matched=mean(dComp.t.t0.matched[-1], na.rm=TRUE),
			dComp.t.t0.matched.slope=summary(lm(dComp.t.t0.matched[-1]~years.elapsed))$coef[2,1],
			
			# Rate of temperature change
			stemp.slope=tryCatch(summary(lm(stemp~years.elapsed0))$coef[2,1], error=function(cond)as.numeric(NA)),
			btemp.slope=summary(lm(btemp~years.elapsed0))$coef[2,1]
			
			
			
		)
	}, 
	by=c("s.reg","region", "stratum")
]

shifts.strat <- merge(shifts.strat, beta.var.time[,list(s.reg,stratum,var.time)], by=c("s.reg","stratum"))

# =============================================
# = Community vs. Climate Velocity: By Region =
# =============================================
# First, do the figures separated by region
shifts.strat[,
	j={
		# ====================
		# = Latitude + Stemp =
		# ====================
		if(SaveFigs){
			dev.new.lf("velocity_com_stemp_lat", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_com_stemp_lat.png")
		}else{
			dev.new.lf("velocity_com_stemp_lat")
		}
		
		.SD[
			j={
				mod.mat <- matrix(c(com.dLat.t1.t,stemp.dLat.t1.t),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(com.dLat.t1.t~stemp.dLat.t1.t)
					# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLat.t1.t - stemp.dLat.t1.t)
				}
				
				if(na.check){
					plot(stemp.dLat.t1.t, com.dLat.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(stemp.dLat.t1.t, com.dLat.t1.t)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(stemp.dLat.t1.t, com.dLat.t1.t, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(Community~Velocity~(degree*North/yr)), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Surface~Temp~Velocity~(degree*North/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
		# ====================
		# = Latitude + Btemp =
		# ====================
		if(SaveFigs){
			dev.new.lf("velocity_com_btemp_lat", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_com_btemp_lat.png")
		}else{
			dev.new.lf("velocity_com_btemp_lat")
		}
		.SD[
			j={
				mod.mat <- matrix(c(com.dLat.t1.t,btemp.dLat.t1.t),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(com.dLat.t1.t~btemp.dLat.t1.t)
					# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLat.t1.t - btemp.dLat.t1.t)
				}
				
				if(na.check){
					plot(btemp.dLat.t1.t, com.dLat.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(btemp.dLat.t1.t, com.dLat.t1.t)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(btemp.dLat.t1.t, com.dLat.t1.t, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(Community~Velocity~(degree*North/yr)), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Bottom~Temp~Velocity~(degree*North/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
		
		# ====================
		# = Longitude + Stemp =
		# ====================
		if(SaveFigs){
			dev.new.lf("velocity_com_stemp_laon", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_com_stemp_lon.png")
		}else{
			dev.new.lf("velocity_com_stemp_lon")
		}
		.SD[
			j={
				mod.mat <- matrix(c(com.dLon.t1.t,stemp.dLon.t1.t),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(com.dLon.t1.t~stemp.dLon.t1.t)
					# resFit <- abs(com.dLon.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLon.t1.t - stemp.dLon.t1.t)
				}
				
				if(na.check){
					plot(stemp.dLon.t1.t, com.dLon.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(stemp.dLon.t1.t, com.dLon.t1.t)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(stemp.dLon.t1.t, com.dLon.t1.t, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(Community~Velocity~(degree*East/yr)), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Surface~Temp~Velocity~(degree*East/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
		
		# ====================
		# = Longitude + Btemp =
		# ====================
		if(SaveFigs){
			dev.new.lf("velocity_com_btemp_lon", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_com_btemp_lon.png")
		}else{
			dev.new.lf("velocity_com_btemp_lon")
		}
		.SD[
			j={
				mod.mat <- matrix(c(com.dLon.t1.t,btemp.dLon.t1.t),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(com.dLon.t1.t~btemp.dLon.t1.t)
					# resFit <- abs(com.dLon.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLon.t1.t - btemp.dLon.t1.t)
				}
				
				if(na.check){
					plot(btemp.dLon.t1.t, com.dLon.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(btemp.dLon.t1.t, com.dLon.t1.t)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(btemp.dLon.t1.t, com.dLon.t1.t, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(Community~Velocity~(degree*East/yr)), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Bottom~Temp~Velocity~(degree*East/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
	},
]


# =======================================
# = Btemp & Stemp Latitude: All Regions =
# =======================================
shifts.strat[,
	j={
		if(SaveFigs){
			dev.new.lf2("velocity_com_temp_lat_allRegs", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_com_temp_lat_allRegs.png")
		}else{
			dev.new.lf2("velocity_com_temp_lat_allRegs")
		}
		
		# =========
		# = Stemp =
		# =========
		mod.mat <- matrix(c(com.dLat.t1.t,stemp.dLat.t1.t),ncol=2)
		na.check <- sum(complete.cases(mod.mat)) > 2
		# Regressions
		if(na.check){
			t.mod <- lm(com.dLat.t1.t~stemp.dLat.t1.t)
			# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
			# res11 <- abs(com.dLat.t1.t - stemp.dLat.t1.t)
		}
		
		if(na.check){
			plot(stemp.dLat.t1.t, com.dLat.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n")
			abline.mod(stemp.dLat.t1.t, com.dLat.t1.t)
			r2 <- round(summary(t.mod)$r.squ,2)
			legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.05,-0.01))
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n")	
		}

		# Plot Lines	
		abline(a=0, b=1, col="blue", lty="dashed", lwd=1)

		# Plot Real
		if(na.check){
			points(stemp.dLat.t1.t, com.dLat.t1.t, pch=21, bg=black2, col=NA)
		}else{
			points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
		}
		mtext(bquote(Surface~Temp~Velocity~(degree*North/yr)), side=1, line=1, outer=FALSE)
		
		# =========
		# = Btemp =
		# =========
		mod.mat <- matrix(c(com.dLat.t1.t,btemp.dLat.t1.t),ncol=2)
		na.check <- sum(complete.cases(mod.mat)) > 2
		# Regressions
		if(na.check){
			t.mod <- lm(com.dLat.t1.t~btemp.dLat.t1.t)
			# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
			# res11 <- abs(com.dLat.t1.t - btemp.dLat.t1.t)
		}
		
		if(na.check){
			plot(btemp.dLat.t1.t, com.dLat.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n")
			abline.mod(btemp.dLat.t1.t, com.dLat.t1.t)
			r2 <- round(summary(t.mod)$r.squ,2)
			legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.05,-0.01))
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n")	
		}

		# Plot Lines	
		abline(a=0, b=1, col="blue", lty="dashed", lwd=1)

		# Plot Real
		if(na.check){
			points(btemp.dLat.t1.t, com.dLat.t1.t, pch=21, bg=black2, col=NA)
		}else{
			points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
		}
		mtext(bquote(Bottom~Temp~Velocity~(degree*North/yr)), side=1, line=1, outer=FALSE)
	},
]
mtext(bquote(Community~Velocity~(degree*North/yr)), side=2, line=-1.25, outer=TRUE)
if(SaveFigs){dev.off()}



# =======================================
# = Btemp & Stemp Longitude: All Regions =
# =======================================
shifts.strat[,
	j={
		if(SaveFigs){
			dev.new.lf2("velocity_com_temp_lon_allRegs", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_com_temp_lon_allRegs.png")
		}else{
			dev.new.lf2("velocity_com_temp_lon_allRegs")
		}
		
		# =========
		# = Stemp =
		# =========
		mod.mat <- matrix(c(com.dLon.t1.t,stemp.dLon.t1.t),ncol=2)
		na.check <- sum(complete.cases(mod.mat)) > 2
		# Regressions
		if(na.check){
			t.mod <- lm(com.dLon.t1.t~stemp.dLon.t1.t)
			# resFit <- abs(com.dLon.t1.t - as.numeric(predict(t.mod)))
			# res11 <- abs(com.dLon.t1.t - stemp.dLon.t1.t)
		}
		
		if(na.check){
			plot(stemp.dLon.t1.t, com.dLon.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n")
			abline.mod(stemp.dLon.t1.t, com.dLon.t1.t)
			r2 <- round(summary(t.mod)$r.squ,2)
			legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.05,-0.01))
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n")	
		}

		# Plot Lines	
		abline(a=0, b=1, col="blue", lty="dashed", lwd=1)

		# Plot Real
		if(na.check){
			points(stemp.dLon.t1.t, com.dLon.t1.t, pch=21, bg=black2, col=NA)
		}else{
			points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
		}
		mtext(bquote(Surface~Temp~Velocity~(degree*East/yr)), side=1, line=1, outer=FALSE)
		
		# =========
		# = Btemp =
		# =========
		mod.mat <- matrix(c(com.dLon.t1.t,btemp.dLon.t1.t),ncol=2)
		na.check <- sum(complete.cases(mod.mat)) > 2
		# Regressions
		if(na.check){
			t.mod <- lm(com.dLon.t1.t~btemp.dLon.t1.t)
			# resFit <- abs(com.dLon.t1.t - as.numeric(predict(t.mod)))
			# res11 <- abs(com.dLon.t1.t - btemp.dLon.t1.t)
		}
		
		if(na.check){
			plot(btemp.dLon.t1.t, com.dLon.t1.t, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n")
			abline.mod(btemp.dLon.t1.t, com.dLon.t1.t)
			r2 <- round(summary(t.mod)$r.squ,2)
			legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.05,-0.01))
		}else{
			plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n")	
		}

		# Plot Lines	
		abline(a=0, b=1, col="blue", lty="dashed", lwd=1)

		# Plot Real
		if(na.check){
			points(btemp.dLon.t1.t, com.dLon.t1.t, pch=21, bg=black2, col=NA)
		}else{
			points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
		}
		mtext(bquote(Bottom~Temp~Velocity~(degree*East/yr)), side=1, line=1, outer=FALSE)
	},
]
mtext(bquote(Community~Velocity~(degree*East/yr)), side=2, line=-1.25, outer=TRUE)
if(SaveFigs){dev.off()}





# shifts.strat[,table(stemp.cat, s.reg)]

# Quick summary of the btemp categories
# shifts.strat[,round(prop.table(table(btemp.cat, s.reg),2),3)*100]
# dev.new()
# shifts.strat[,barplot(round(prop.table(table(btemp.cat, s.reg),2),3)*100, legend.text=TRUE, beside=F, args.legend=list(ncol=2))]
#
#


# =============================
# = Map of Burrows Categories =
# =============================
categ.cols <- c(
	"Source"="mediumblue",
	"Sink"="red",
	"Corridor"="magenta1",
	"Convergence"="yellow",
	"Divergence"="cyan",
	"Balanced"="white"
	)


shifts.strat[,btemp.categ.col:=categ.cols[btemp.cat]]

if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_burrowsCategory.btemp.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
shifts.strat[,plot(lon, lat, col=btemp.categ.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=btemp.categ.col, pch=21, cex=1)]
legend("bottomleft", legend=names(categ.cols), pt.bg=categ.cols, pch=21, text.font=2, cex=1.5)
if(SaveFigs){dev.off()}



# ==========================================
# = Map of Community Velocity in ºLatitude =
# ==========================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_com_lat.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
comVel.lat.col <- shifts.strat[,heat.cols[cut(com.dLat.t1.t, 256)]]
shifts.strat[,plot(lon, lat, col=comVel.lat.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=comVel.lat.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(com.dLat.t1.t), max(com.dLat.t1.t), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Community~Velocity~degree*North/yr))
if(SaveFigs){dev.off()}



# ==========================================
# = Map of Community Velocity in ºLongitude =
# ==========================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_com_lon.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
comVel.lon.col <- shifts.strat[,heat.cols[cut(com.dLon.t1.t, 256)]]
shifts.strat[,plot(lon, lat, col=comVel.lon.col, pch=21, cex=1, type="n")]
# invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
map(add=TRUE, fill=FALSE, col="black")
shifts.strat[,points(lon, lat, col=comVel.lon.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black") # tick marks
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(com.dLon.t1.t), max(com.dLon.t1.t), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Community~Velocity~degree*East/yr))
if(SaveFigs){dev.off()}





# ======================================
# = Map of Stemp Velocity in ºLatitude =
# ======================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_stemp_lat.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
stempVel.lat.col <- shifts.strat[,heat.cols[cut(stemp.dLat.t1.t, 256)]]
shifts.strat[,plot(lon, lat, col=stempVel.lat.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=stempVel.lat.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(stemp.dLat.t1.t, na.rm=TRUE), max(stemp.dLat.t1.t, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Surface~Temperature~Velocity~degree*North/yr))
if(SaveFigs){dev.off()}


# ======================================
# = Map of Btemp Velocity in ºLatitude =
# ======================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_btemp_lat.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
btempVel.lat.col <- shifts.strat[,heat.cols[cut(btemp.dLat.t1.t, 256)]]
shifts.strat[,plot(lon, lat, col=btempVel.lat.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=btempVel.lat.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(btemp.dLat.t1.t, na.rm=TRUE), max(btemp.dLat.t1.t, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Bottom~Temperature~Velocity~degree*North/yr))
if(SaveFigs){dev.off()}



# ======================================
# = Map of Stemp Velocity in ºLongitude =
# ======================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_stemp.lon.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
stempVel.lon.col <- shifts.strat[,heat.cols[cut(stemp.dLon.t1.t, 256)]]
shifts.strat[,plot(lon, lat, col=stempVel.lon.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=stempVel.lon.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(stemp.dLon.t1.t, na.rm=TRUE), max(stemp.dLon.t1.t, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Surface~Temperature~Velocity~degree*East/yr))
if(SaveFigs){dev.off()}


# ======================================
# = Map of Btemp Velocity in ºLatitude =
# ======================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_btemp_lon.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
btempVel.lon.col <- shifts.strat[,heat.cols[cut(btemp.dLon.t1.t, 256)]]
shifts.strat[,plot(lon, lat, col=btempVel.lon.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=btempVel.lon.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(btemp.dLon.t1.t, na.rm=TRUE), max(btemp.dLon.t1.t, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Bottom~Temperature~Velocity~degree*East/yr))
if(SaveFigs){dev.off()}


# ==========================================
# = Map of Cor(btempLatitude, comLatitude) =
# ==========================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/veloicty_map_corr_com_btemp_lat.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
com.btemp.lat.cor.col <- shifts.strat[,heat.cols[findInterval(com.btemp.lat.cor, seq(-1,1,length.out=256))]]
shifts.strat[,plot(lon, lat, col=com.btemp.lat.cor.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=com.btemp.lat.cor.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
text(-167, y=seq(30,40, length.out=4), round(seq(-1, 1, length.out=4),4), adj=1, cex=1, col="black")
text(-162.5, 41.5, bquote(Cor(Btemp*","~Community)~Velocities~(degree*North/yr)))
if(SaveFigs){dev.off()}


# ==========================================
# = Map of Cor(stempLatitude, comLatitude) =
# ==========================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_corr_com_stemp_lat.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")

com.stemp.lat.cor.col <- shifts.strat[,heat.cols[findInterval(com.stemp.lat.cor, seq(-1,1,length.out=256))]]
shifts.strat[,plot(lon, lat, col=com.stemp.lat.cor.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=com.stemp.lat.cor.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
text(-167, y=seq(30,40, length.out=4), round(seq(-1, 1, length.out=4),4), adj=1, cex=1, col="black")
text(-162.5, 41.5, bquote(Cor(Stemp*","~Community)~Velocities~(degree*East/yr)))
if(SaveFigs){dev.off()}
	


# ==========================================
# = Map of Cor(btempLongitude, comLongitude) =
# ==========================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/veloicty_map_corr_com_btemp_lon.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
com.btemp.lon.cor.col <- shifts.strat[,heat.cols[findInterval(com.btemp.lon.cor, seq(-1,1,length.out=256))]]
shifts.strat[,plot(lon, lat, col=com.btemp.lon.cor.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=com.btemp.lon.cor.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
text(-167, y=seq(30,40, length.out=4), round(seq(-1, 1, length.out=4),4), adj=1, cex=1, col="black")
text(-162.5, 41.5, bquote(Cor(Btemp*","~Community)~Velocities~(degree*East/yr)))
if(SaveFigs){dev.off()}


# ==========================================
# = Map of Cor(stempLongitude, comLongitude) =
# ==========================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/velocity_map_corr_com_stemp_lon.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
com.stemp.lon.cor.col <- shifts.strat[,heat.cols[findInterval(com.stemp.lon.cor, seq(-1,1,length.out=256))]]
shifts.strat[,plot(lon, lat, col=com.stemp.lon.cor.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=com.stemp.lon.cor.col, pch=21, cex=1)]
shifts.strat[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
text(-167, y=seq(30,40, length.out=4), round(seq(-1, 1, length.out=4),4), adj=1, cex=1, col="black")
text(-162.5, 41.5, bquote(Cor(Stemp*","~Community)~Velocities~(degree*East/yr)))
if(SaveFigs){dev.off()}


# ======================================================================================
# = Map of mean difference between original community and closest contemprorary analog =
# ======================================================================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_meanDiff_origCom_analog.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")

dComp.t.t0.matched.col <- shifts.strat[,heat.cols[cut(dComp.t.t0.matched, 256)]]
shifts.strat[,plot(lon, lat, col=dComp.t.t0.matched.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=dComp.t.t0.matched.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(dComp.t.t0.matched), max(dComp.t.t0.matched), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Mean~Dissimilarity~"b/w"~Original~"&"~Analogs))
if(SaveFigs){dev.off()}



# ==============================================================================
# = Map of rate of change of dissimilarity between original and closest analog =
# ==============================================================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_regional_rateLoss_origCom_analog.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
dComp.t.t0.matched.slope.col <- shifts.strat[,heat.cols[cut(dComp.t.t0.matched.slope, 256)]]
shifts.strat[,plot(lon, lat, col=dComp.t.t0.matched.slope.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=dComp.t.t0.matched.slope.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(dComp.t.t0.matched.slope), max(dComp.t.t0.matched.slope), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Rate~of~Regional~Loss~of~Original~Local~Community))
if(SaveFigs){dev.off()}




# ===========================
# = Average Alpha Diversity =
# ===========================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_mean_alphaD.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
alphaD.t.col <- shifts.strat[,heat.cols[cut(alphaD.t, 256)]]
shifts.strat[,plot(lon, lat, col=alphaD.t.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=alphaD.t.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(alphaD.t), max(alphaD.t), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Average~alpha~Diversity))
if(SaveFigs){dev.off()}



# =====================================
# = Rate of change in Alpha diversity =
# =====================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_slope_alphaD.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
alphaD.slope.col <- shifts.strat[,heat.cols[cut(alphaD.slope, 256)]]
shifts.strat[,plot(lon, lat, col=alphaD.slope.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=alphaD.slope.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(alphaD.slope, na.rm=TRUE), max(alphaD.slope, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Rate~of~Change~"in"~alpha~Diversity))
if(SaveFigs){dev.off()}



# ==================
# = Beta Diveristy =
# ==================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_betaD.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
var.time.col <- shifts.strat[,heat.cols[cut(var.time, 256)]]
shifts.strat[,plot(lon, lat, col=var.time.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=var.time.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(var.time), max(var.time), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(beta~Diversity))
if(SaveFigs){dev.off()}


# ======================================
# = Surface Temperature Rate of Change =
# ======================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_Stemp_slope.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
stemp.slope.col <- shifts.strat[,heat.cols[cut(stemp.slope, 256)]]
shifts.strat[,plot(lon, lat, col=stemp.slope.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=stemp.slope.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(stemp.slope, na.rm=TRUE), max(stemp.slope, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Rate~of~Change~"in"~Surface~Temp))
if(SaveFigs){dev.off()}


# =====================================
# = Bottom Temperature Rate of Change =
# =====================================
if(SaveFigs){
	png("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/map_Btemp_slope.png", res=300, height=4, width=shifts.strat[,map.w(lat,lon,4)], units="in")
}else{
	dev.new(height=4, width=shifts.strat[,map.w(lat,lon,4)])
}
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1, bg="lightgray")
btemp.slope.col <- shifts.strat[,heat.cols[cut(btemp.slope, 256)]]
shifts.strat[,plot(lon, lat, col=btemp.slope.col, pch=21, cex=1, type="n")]
invisible(shifts.strat[,map(add=TRUE, fill=FALSE, col="black")])
shifts.strat[,points(lon, lat, col=btemp.slope.col, pch=21, cex=1)]
segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
shifts.strat[,text(-167, y=seq(30,40, length.out=4), round(seq(min(btemp.slope, na.rm=TRUE), max(btemp.slope, na.rm=TRUE), length.out=4),4), adj=1, cex=1, col="black")]
text(-162.5, 41.5, bquote(Rate~of~Change~"in"~Bottom~Temp))
if(SaveFigs){dev.off()}



# =================================================
# = Diversity vs. Temperature rate of change =
# =================================================
# First, do the figures separated by region
shifts.strat[,
	j={
		# ====================
		# = BetaD vs Stemp Slope =
		# ====================
		if(SaveFigs){
			dev.new.lf("betaD_stempSlope", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/betaD_stempSlope.png")
		}else{
			dev.new.lf("velocity_com_stemp_lat")
		}
		.SD[
			j={
				mod.mat <- matrix(c(var.time,stemp.slope),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(var.time~stemp.slope)
					# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLat.t1.t - stemp.dLat.t1.t)
				}
				
				if(na.check){
					plot(stemp.slope, var.time, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(stemp.slope, var.time)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(stemp.slope, var.time, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(beta~Diversity), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Surface~Temp~Change~(degree*C/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
		
		
		# ====================
		# = betaD vs. btempSlope =
		# ====================
		if(SaveFigs){
			dev.new.lf("betaD_btempSlope", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/betaD_btempSlope.png")
		}else{
			dev.new.lf("betaD_btempSlope")
		}
		.SD[
			j={
				mod.mat <- matrix(c(var.time,btemp.slope),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(var.time~btemp.slope)
					# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLat.t1.t - btemp.dLat.t1.t)
				}
				
				if(na.check){
					plot(btemp.slope, var.time, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(btemp.slope, var.time)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(btemp.slope, var.time, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(beta~Diversity), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Bottom~Temp~Change~(degree*C/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
		
		# ====================
		# = alphaD.slope vs stemp.slope =
		# ====================
		if(SaveFigs){
			dev.new.lf("alphaD.slope_stempSlope", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/alphaD.slope_stempSlope.png")
		}else{
			dev.new.lf("alphaD.slope_stempSlope")
		}
		.SD[
			j={
				mod.mat <- matrix(c(alphaD.slope,stemp.slope),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(alphaD.slope~stemp.slope)
					# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLat.t1.t - stemp.dLat.t1.t)
				}
				
				if(na.check){
					plot(stemp.slope, alphaD.slope, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(stemp.slope, alphaD.slope)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(stemp.slope, alphaD.slope, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(alpha~Diversity~Change~(alpha/yr)), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Surface~Temp~Change~(degree*C/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
		
		# ====================
		# = alphaD vs. btempSlope =
		# ====================
		if(SaveFigs){
			dev.new.lf("alphaD.slope_btempSlope", fileName="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/FishBaste_Figures/alphaD.slope_btempSlope.png")
		}else{
			dev.new.lf("alphaD.slope_btempSlope")
		}
		.SD[
			j={
				mod.mat <- matrix(c(alphaD.slope,btemp.slope),ncol=2)
				na.check <- sum(complete.cases(mod.mat)) > 2
				# Regressions
				if(na.check){
					t.mod <- lm(alphaD.slope~btemp.slope)
					# resFit <- abs(com.dLat.t1.t - as.numeric(predict(t.mod)))
					# res11 <- abs(com.dLat.t1.t - btemp.dLat.t1.t)
				}
				
				if(na.check){
					plot(btemp.slope, alphaD.slope, ylab="", xlab="", pch=21, bg=black2, col=NA, type="n", main=region)
					abline.mod(btemp.slope, alphaD.slope)
					r2 <- round(summary(t.mod)$r.squ,2)
					legend("topleft", bty="n", legend=bquote(R^2==.(r2)), inset=c(-0.15,-0.1))
				}else{
					plot(1, ylab="", xlab="", xaxt="n",yaxt="n", pch="NA", col=NA, type="n", main=region)	
				}
		
				# Plot Lines	
				abline(a=0, b=1, col="blue", lty="dashed", lwd=1)
		
				# Plot Real
				if(na.check){
					points(btemp.slope, alphaD.slope, pch=21, bg=black2, col=NA)
				}else{
					points(1, xaxt="n",yaxt="n", pch="NA", bg=black2, col=NA)	
				}
			},
			by=c("s.reg","region")
		]
		mtext(bquote(alpha~Diversity~Change~(alpha/yr)), side=2, line=-0.15, outer=TRUE)
		mtext(bquote(Bottom~Temp~Change~(degree*C/yr)), side=1, line=-0.15, outer=TRUE)
		if(SaveFigs){dev.off()}
	},
]









