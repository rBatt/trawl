library(data.table)
library(maps)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions/map.w.R")
# =========================
# = Load Data and Scripts =
# =========================
# Load trawl data
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/divData.RData")


tempData0 <- divData[,list(s.reg, stratum, year, lat, lon, depth, stemp, btemp)]

tempData <- tempData0[,c("stemp","btemp"):=list(mean(stemp, na.rm=TRUE), mean(btemp, na.rm=TRUE)), by=c("stratum","year","lat","lon","depth")]
setkey(tempData, s.reg, stratum, year, lat, lon, depth, stemp, btemp)
tempData <- unique(tempData)
tempData <- tempData[,
	{
	blah <- data.frame(X=lon, Y=lat) 
	attr(blah, "projection")="LL"
	blah2 <- convUL(blah)
	list(s.reg=s.reg, stratum=stratum, year=year, lat=blah2[,"Y"], lon=blah2[,"X"], depth=depth, stemp=stemp, btemp=btemp, lat.old=lat, lon.old=lon)
	
	}

]



Bfun <- function(x,y){
	if(sum(is.finite(y))<3){
		matrix(rep(as.numeric(NA),4),ncol=2)
	}
	if(length(unique(x[is.finite(x)]))<3){
		matrix(rep(as.numeric(NA),4),ncol=2)
	}
	tryCatch(summary(lm(y~x))$coef, error=function(cond){matrix(rep(as.numeric(NA),4),ncol=2)})
}

# vel000 <- tempData[,c("mu.lat","mu.")]


vel00.s <-  tempData[,
	j={	
		m.CperYR <- Bfun(year, stemp)
		CperYR <- m.CperYR[2,1] # estimate of slope
		CperYR.se <- m.CperYR[2,2] # s.e. of slope
		
		m.lat <- Bfun(lat, stemp)
		m.lon <- Bfun(lon, stemp)
		m.dep <- Bfun(depth, stemp)
		
		b.lat <- m.lat[2,1]
		b.lon <- m.lon[2,1]
		b.dep <- m.dep[2,1]
		
		se.lat <- m.lat[2,2]
		se.lon <- m.lon[2,2]
		se.dep <- m.dep[2,2]
		
		vlat <- CperYR/b.lat
		vlon <- CperYR/b.lon
		vdep <- CperYR/b.dep
		
		# propagation of variance:
		# f = A/B
		# variance(f) = f^2 * ( (sigA/A)^2 + (sigB/B)^2 - 2*((cov(AB)/(AB))) )
		# assume covariance is 0
		termA <- (CperYR.se/CperYR)^2 # (sigA/A)^2 in the above equation for propagating variance
		
		list(
			# velocities
			vlat = vlat,
			vlon = vlon,
			vdep = vdep,
			
			# precisions
			vlat.prec = 1/((vlat)^2 * ( termA + (se.lat/b.lat)^2 )),
			vlon.prec = 1/((vlon)^2 * ( termA + (se.lon/b.lon)^2 )),
			vdep.prec = 1/((vdep)^2 * ( termA + (se.dep/b.dep)^2 )),
			
			lat=mean(lat, na.rm=TRUE),
			lon=mean(lon, na.rm=TRUE),
			lat.old=mean(lat.old, na.rm=TRUE),
			lon.old=mean(lon.old, na.rm=TRUE)
			)
	},

	by=c("s.reg","stratum")
]
vel00.s[,"lvl":="surface"]


vel00.b <-  tempData[,
	j={	
		m.CperYR <- Bfun(year, btemp)
		CperYR <- m.CperYR[2,1] # estimate of slope
		CperYR.se <- m.CperYR[2,2] # s.e. of slope
		
		m.lat <- Bfun(lat, btemp)
		m.lon <- Bfun(lon, btemp)
		m.dep <- Bfun(depth, btemp)
		
		b.lat <- m.lat[2,1]
		b.lon <- m.lon[2,1]
		b.dep <- m.dep[2,1]
		
		se.lat <- m.lat[2,2]
		se.lon <- m.lon[2,2]
		se.dep <- m.dep[2,2]
		
		vlat <- CperYR/b.lat
		vlon <- CperYR/b.lon
		vdep <- CperYR/b.dep
		
		# propagation of variance:
		# f = A/B
		# variance(f) = f^2 * ( (sigA/A)^2 + (sigB/B)^2 - 2*((cov(AB)/(AB))) )
		# assume covariance is 0
		termA <- (CperYR.se/CperYR)^2 # (sigA/A)^2 in the above equation for propagating variance
		
		list(
			# velocities
			vlat = vlat,
			vlon = vlon,
			vdep = vdep,
			
			# precisions
			vlat.prec = 1/((vlat)^2 * ( termA + (se.lat/b.lat)^2 )),
			vlon.prec = 1/((vlon)^2 * ( termA + (se.lon/b.lon)^2 )),
			vdep.prec = 1/((vdep)^2 * ( termA + (se.dep/b.dep)^2 )),
			
			lat=mean(lat, na.rm=TRUE),
			lon=mean(lon, na.rm=TRUE),
			lat.old=mean(lat.old, na.rm=TRUE),
			lon.old=mean(lon.old, na.rm=TRUE)
			)
	},

	by=c("s.reg","stratum")
]
vel00.b[,"lvl":="bottom"]



vel0 <- rbind(vel00.s, vel00.b)
vel0[,velocity:=list(sqrt(vlat^2+vlon^2))]

vel <- vel0







# ================================
# = Beta D as variance over time =
# ================================
# Create colors
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
vel[,vel.col:=heat.cols[cut(velocity, 256)]]

# New device
# dev.new(height=4, width=beta.var.time[,map.w(lat,lon,4)])
pdf(height=4, width=vel[,map.w(lat.old,lon.old,4)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/climateVelocity.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")

# Plot
vel[,plot(lon.old, lat.old, col=vel.col, pch=21, cex=1, type="n")] # set plot region
invisible(vel[,map(add=TRUE, fill=FALSE, col="black")]) # add map
vel[,points(lon.old, lat.old, col=vel.col, pch=21, cex=1)] # add points

# Key
vel[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
vel[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
vel[,text(-167, y=seq(30,40, length.out=4), round(seq(min(velocity, na.rm=TRUE), max(velocity, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
vel[,text(-162.5, 41.5, bquote(Climate~Velocity))] # add label for key
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")


dev.off()











