

# =================
# = Load Packages =
# =================
library(data.table)
library(fields)
library(beanplot)
library(RColorBrewer)

# ===============================
# = Guess appropriate directory =
# ===============================
if(any(grepl('mpinsky', list.files(path='~/../')))){ # if on Malin's MacBook Air
	setwd("~/Documents/Rutgers/Batt community and climate/")
} else { # if not
	if(Sys.info()["sysname"]=="Linux"){ # if on Amphiprion
		setwd("~/Documents/School&Work/pinskyPost")
	}else{ # if on Ryan's laptop. Need a more specific test here
		setwd("~/Documents/School&Work/pinskyPost")
	}
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
load("./trawl/Data/trawl2.RData")
trawl2.veri <- trawl2[(correctSpp)&!is.na(common)&is.species(spp),]
trawl2.veri <- trawl2.veri[s.reg!="wcann" | (s.reg=="wcann" & year > 2003)]
trawl2.veri[s.reg=="wcann" | s.reg=="wctri", s.reg:="wc"]


# ==========================
# = Load Processed Results =
# ==========================
load("./trawl/Results/Richness/rco.RData") # richness cov model output; very large data.table – variables per year, stratum, and spp. Many values are repeated.
load("./trawl/Results/Richness/rco.s.RData") # richness cov model output, average to stratum values (avgd over spp and year)
load("./trawl/Results/Richness/rco.sy.RData") # richness cov model output, aggregated to values per stratum/ year (avgd over spp)
load("./trawl/Results/Richness/ao.rco.RData") # the list of the richness covariate model output (all out . richness cov out)
load("./trawl/Results/Richness/cT.rcoS.RData") # combines final climate trajectory states w/ per-site trends and averages of richness, btemp, depth

load("./trawl/Results/Richness/cT.rcoS.noAnn.RData") # combines final climate trajectory states w/ per-site trends and averages of richness, btemp, depth

load("./trawl/Results/trawl.betaD.RData") # load beta diversity

cT.rcoS[,categ:=factor(categ, levels=c("None","Source","Divergence","Corridor","Convergence","Sink"))]


# rco[,spp.depth:=weighted.mean(depth, w=Z, na.rm=TRUE), by=c("s.reg","spp")]
# regKey <- c("ai"="Aleutian Islands", "ebs"="Eastern Bering Sea", "gmex"="Gulf of Mexico", "goa"="Gulf of Alaska", "neus"="Northeast US", "newf"="Newfoundland", "sa"="US South Atlantic", "sgulf"="S. Gulf of St. Lawrence", "shelf"="Scotian Shelf", "wcann"="West Coast (ann)", "wctri"= "West Coast (tri)")

regKey <- c("ai"="Aleutian Islands", "ebs"="Eastern Bering Sea", "gmex"="Gulf of Mexico", "goa"="Gulf of Alaska", "neus"="Northeast US", "newf"="Newfoundland", "sa"="US South Atlantic", "sgulf"="S. Gulf of St. Lawrence", "shelf"="Scotian Shelf", "wc"="West Coast")


# ==========================
# = Load and add in Beta D =
# ==========================
beta.var.time[,stratum:=NULL]
beta.turn.time[,stratum:=NULL]

setkey(beta.var.time, s.reg, lon, lat)
setkey(beta.turn.time, s.reg, lon, lat)

cT.rcoS.b0 <- merge(cT.rcoS, beta.var.time, all=TRUE)
cT.rcoS.b <- merge(cT.rcoS.b0, beta.turn.time, all=TRUE)

setkey(cT.rcoS.noAnn, s.reg, lon, lat)
cT.rcoS.noAnn.b0 <- merge(cT.rcoS.noAnn, beta.var.time, all.x=TRUE)
cT.rcoS.noAnn.b <- merge(cT.rcoS.noAnn.b0, beta.turn.time, all.x=TRUE)


# ===================================
# = Get Temperature Response Curves =
# ===================================
tempResponse <- function(u.a0, a1, a2, a3, a4, tg){ # calculate the probability of occurrence given paremeters; uses tempGrad
	opt.depth <- pmax(-a3/(2*a4), 0)
	# sppD[is.na(sppD)] <- mean(sppD, na.rm=TRUE)
	# plogis(u.a0 + a1*tg + a2*tg^2 + a3*sppD + a4*sppD^2)
	# plogis(u.a0 + a1*tg + a2*tg^2 + a3*0 + a4*0^2)
	plogis(u.a0 + a1*tg + a2*tg^2 + a3*opt.depth + a4*opt.depth^2)
}


# NOTE:  RUNNING THIS COMMAND FOR MORE THAN 1 REGION PER R SESSION HAS BEEN RESULTING IN AN ERROR FOR ME! SEGFAULT MEMORY ERROR. DO 1 AT A TIME
chooseReg <- "goa"
auto.dim <- rco[s.reg==chooseReg,auto.mfrow(lu(year))]
png(paste0("./trawl/Figures/Diversity/msomCov/",chooseReg,"_tempResponse.png"), width=(auto.dim[2]/auto.dim[1])*7, height=7, res=200, units="in")
# dev.new(width=(auto.dim[2]/auto.dim[1])*7, height=7)
par(mfrow=auto.dim, mar=c(1,1,1.0,0.1), oma=c(0.5,0.5,1,0), cex=1, tcl=-0.1, mgp=c(1,0.1,0), ps=8)

rco[s.reg==chooseReg,
	j={
		# dev.new(height=7, width=(auto.dim[2]/auto.dim[1])*7)

		tempGrad <- seq(-2,max(btemp), by=0.25)
		tempRange <- .SD[,range(btemp, na.rm=TRUE), by="year"]
		print(tempRange)
		
		.SD[stratum==unique(stratum)[1],
			j={
				t.tempRange <- tempRange[year==.BY[[1]], V1]
				# print(t.tempRange)
				# print(year)
				# est.tempResponse <- mapply(tempResponse, u.a0, a1, a2, a3, a4, spp.depth, MoreArgs=list(tg=tempGrad))
				est.tempResponse <- mapply(tempResponse, u.a0, a1, a2, a3, a4, MoreArgs=list(tg=tempGrad))
				# print(any(is.na(spp.depth)))
				# print(head(est.tempResponse))
				quant.tempResponse <- apply(est.tempResponse, 1, quantile, c(0.25, 0.5, 0.75))

				plot(tempGrad, est.tempResponse[,1], ylim=c(0,1), type="l", col=MyGray, xlab="", ylab="", cex=1)
				
				plotOthers <- function(x, tg){
					lines(tg, x, col=MyGray)
				}
				apply(est.tempResponse[,-1], 2, plotOthers, tg=tempGrad)
				
				# legend("topleft", legend=unique(year), text.font=2, bty="o", inset=c(-0.12,-0.05), bg="white", box.col="white")
				# legend("topleft", legend=unique(year), text.font=2, bty="n", inset=c(-0.12,-0.05), cex=1)
				# legend("topleft", legend=unique(year), text.font=1, bty="n", inset=c(-0.12,-0.05), cex=0.85, text.col="white")
				
				# text(0.5, 0.2, labels=unique(year), font=2)
				
				lines(tempGrad, quant.tempResponse[3,], col="salmon2", lwd=1.5)
				lines(tempGrad, quant.tempResponse[2,], col="khaki", lwd=1.5)
				lines(tempGrad, quant.tempResponse[1,], col="steelblue", lwd=1.5)
				
				t01 <- t.tempRange #range(btemp, na.rm=TRUE) #quantile(unique(btemp), c(0.25,0.75), na.rm=TRUE)
				arrows(t01[1], 0.01, t01[2], 0.01, col="red", angle=90, code=3, length=0.05)
				mtext(side=3, line=-0.1, unique(year), adj=0, font=2, cex=1.1)
			},
			by=c("year")
		]
		
		mtext(side=2, line=-0.75, "Probability of Occurrence", outer=TRUE)
		mtext(side=1, line=-0.75, bquote(Temperature~(phantom()*degree*C)), outer=TRUE)
		mtext(side=3, line=0, regKey[.BY[[1]]], outer=TRUE, cex=1.5, font=2)
		
		# rm(list=ls())
	},
	by=c("s.reg")
]
dev.off()



# ====================================== 
# = Richness vs. Temperature vs. Depth =
# ======================================
probs.from.cov <- function(depth,temp, u.a0, a1, a2, a3, a4){
	stopifnot(length(depth)==1 & length(temp)==1)
	# sum(anti.logit(u.a0 + a1*temp + a2*temp^2 + a3*depth + a4*depth^2))
	sum(plogis(u.a0 + a1*temp + a2*temp^2 + a3*depth + a4*depth^2))
}
n.from.cov <- function(depth, temp, aList){
	mapply(probs.from.cov, depth, temp, MoreArgs=aList)
}

MyGray2 <- rgb(t(col2rgb("black")), alpha=5, maxColorValue=255)
MyWhite <- rgb(t(col2rgb("white")), alpha=5, maxColorValue=255)
smplt <- c(0.76,0.78, 0.2,0.85)
bgplt <- c(0.1,0.75,0.15,0.90)
axargs <- list(mgp=c(0.25,0.15,0))
l.a <- list(ps=8, cex=1)

# quickRco <- rco[s.reg%in%c("shelf","wc")] # just wanted to pick up where i left off after an error in shelf

rco[!(s.reg=="shelf"&year==2011), # have to drop shelf in 2011 b/c there's no temperature data
	j={
		depthGrad <- seq(min(depth, na.rm=TRUE),max(depth, na.rm=TRUE), length.out=100)
		tempGrad <- seq(min(btemp, na.rm=TRUE),max(btemp, na.rm=TRUE), length.out=100)
		
		auto.dim <- auto.mfrow(lu(year))
		
		# zlim <- range(Nsite, na.rm=TRUE)*c(0.5, 1.15)
		xlim <- range(depthGrad)
		ylim <- range(tempGrad)
		
		# z.lim <- .SD[,
# 			j={
# 				get1 <- stratum==unique(stratum)[1]
# 				t.aList <- list(u.a0=u.a0[get1], a1=a1[get1], a2=a2[get1], a3=a3[get1], a4=a4[get1])
# 				depth.temp.N <- outer(depthGrad, tempGrad, n.from.cov, aList=t.aList)
# 				list(min(depth.temp.N), max(depth.temp.N))
# 			},
# 			by=c("year")
# 		][,list(V1,V2)]
# 		# print(z.lim)
#
# 		zlim <- range(z.lim)
#
		# print(zlim)
		
		# zlim <- range(outer(xlim, ylim, n.from.cov, aList=list(u.a0=range(u.a0), a1=range(a1), a2=range(a2), a3=range(a3), a4=range(a4))))*lu(stratum)
		
		# dev.new(width=(auto.dim[2]/auto.dim[1])*7, height=7)
		# figArea <- 49
		png(paste0("./trawl/Figures/Diversity/msomCov/",unique(s.reg),"_depth.temp.N.png"), width=auto.dim[2]+3, height=auto.dim[1]+2, res=200, units="in")
		par(mfrow=auto.dim, mar=c(2,2,2,3), oma=c(1,1,1.5,0.5), cex=1, mgp=c(0.25,0.15,0), tcl=-0.1, ps=8, xaxs="i", yaxs="i")
		
		.SD[,
			j={
				get1 <- stratum==unique(stratum)[1]
				t.aList <- list(u.a0=u.a0[get1], a1=a1[get1], a2=a2[get1], a3=a3[get1], a4=a4[get1])
				depth.temp.N <- outer(depthGrad, tempGrad, n.from.cov, aList=t.aList)
				
				# Heat map
				# image.plot(x=depthGrad, y=tempGrad, z=depth.temp.N, smallplot=smplt, bigplot=bgplt, axis.args=axargs, xlim=xlim, ylim=ylim, zlim=zlim, ylab="", xlab="")
				image.plot(x=depthGrad, y=tempGrad, z=depth.temp.N, smallplot=smplt, bigplot=bgplt, axis.args=axargs, xlim=xlim, ylim=ylim, ylab="", xlab="", legend.mar=1, legend.line=1)
				box()
				par(cex=1, ps=8)

				points(depth, btemp, col=MyWhite, pch=20, cex=0.25)
				
				mtext(side=3, line=-0.1, .BY[[1]], font=2, outer=F, adj=0.05, cex=1, ps=8)
			},
			by=c("year")
		]
		
		mtext(side=1, line=0.25, "Depth (m)", outer=TRUE)
		mtext(side=2, line=0.25, "Temperature", outer=TRUE)
		mtext(side=3, line=0.25, regKey[.BY[[1]]], font=2, cex=1.5, outer=TRUE)
		
		dev.off()
		
		print(unique(s.reg))
		
	},
	by=c("s.reg")
]

#
#
# # depthGrad <- seq(min(prepd.cov2[[3]]),max(prepd.cov2[[3]]), length.out=100)
# # tempGrad <- seq(min(prepd.cov1[[3]]),max(prepd.cov1[[3]]), length.out=100)
# depthGrad <- seq(50,500, length.out=100)
# tempGrad <- seq(0,10, length.out=100)
#
# t.out.med <- all.out[[1]]$median
# t.aList <- list(u.a0=t.out.med$u.a0, a1=t.out.med$a1, a2=t.out.med$a2, a3=t.out.med$a3, a4=t.out.med$a4)
# depth.temp.N <- outer(depthGrad, tempGrad, n.from.cov, aList=t.aList)
#
# MyGray2 <- rgb(t(col2rgb("black")), alpha=150, maxColorValue=255)
# MyWhite <- rgb(t(col2rgb("white")), alpha=50, maxColorValue=255)
# smplt <- c(0.9,0.92, 0.2,0.9)
# bgplt <- c(0.1,0.89,0.125,0.95)
# axargs <- list(mgp=c(0.5,0.25,0))
#
# dev.new(width=3.5, height=3.5)
# par(mar=c(1.25,1.5,0.1,1), oma=c(0,0,0,0), cex=1, mgp=c(1,0.15,0), tcl=-0.1, ps=8, xaxs="i", yaxs="i")
#
# # Heat map of
# image.plot(x=depthGrad, y=tempGrad, z=depth.temp.N, smallplot=smplt, bigplot=bgplt, axis.args=axargs, xlim=range(depthGrad), ylim=range(tempGrad), ylab="Temperature", xlab="Depth (m)")
# box()
#
# points(prepd.cov2[[3]], prepd.cov1[[3]], col=MyGray2, bg=MyWhite, pch=21)

# Plot kernel density for tempreature (avg temp in each stratum)
# par(new=TRUE)
# temp.dens <- density(prepd.cov1[[i]], from=min(tempGrad), to=max(tempGrad))
# plot(y=temp.dens$x, x=temp.dens$y, col="white", type="l", ylim=range(tempGrad), bty="n", xlab="", ylab="", xaxt="n", yaxt="n")




# ================
# = Richness Map =
# ================
# heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
#
# i=1
# useNum <- gsub("[a-z]+_[0-9]{4}_([0-9]){1}$", "\\1", names(all.out), perl=TRUE)
# t.ao <- all.out[[i]] #[[useNum[i]]]
# dev.new(height=1.5, width=map.w(t.ao$ID$lat, t.ao$ID$lon, height=1.5))
# par(mar=c(1.5,1.5,0.2,1), oma=c(0,0,0,0), cex=1, mgp=c(0.75,0.1,0), tcl=-0.1, ps=8, xaxs="r", yaxs="r")
# t.cols <- heat.cols[cut(t.ao$median$Nsite, 256)]
# plot(t.ao$ID$lon, t.ao$ID$lat, pch=21, bg=t.cols)
# map(add=TRUE,fill=FALSE)






# =========================================
# = Plot Model Stratum Richness over Time =
# =========================================
setkey(rco.sy, s.reg, year, stratum)

auto.dim <- rco.sy[,auto.mfrow(lu(s.reg), tall=F)]
# dev.new(width=4, height=(auto.dim[1]/auto.dim[2])*4)
# dev.new(width=(auto.dim[2]/auto.dim[1])*4, height=4)
png(width=(auto.dim[2]/auto.dim[1])*4, height=4, file="./trawl/Figures/Diversity/strat_rich_model_cov.png", units="in", res=200)
par(mfrow=auto.dim, mar=c(1.5,1,1.0,0.1), oma=c(0.5,1,1.5,0.1), cex=1, tcl=-0.1, mgp=c(1,0.1,0), ps=8)

MyGray2 <- rgb(t(col2rgb("black")), alpha=100, maxColorValue=255)

for(i in 1:length(rco.sy[,unique(s.reg)])){
	
	t.sreg <- rco.sy[,unique(s.reg)[i]]
	t.r <- rco.sy[s.reg==t.sreg]
	
	maxR <- t.r[,max(Nsite)]
	minR <- t.r[,min(Nsite)]
	maxY <- t.r[,max(year)]
	minY <- t.r[,min(year)]
	
	u.strat <- t.r[,unique(stratum)]
	t.r[
		stratum==u.strat[1],
		plot(year, Nsite, ylim=c(minR, maxR), xlim=c(minY,maxY), main=regKey[as.character(unique(s.reg))], type="l", col=MyGray2, xlab="", ylab="")
	]
	for(j in 2:length(u.strat)){
		t.r[stratum==u.strat[j],lines(year,Nsite, col=MyGray2)]
	}
	t.r[,mean(Nsite), by="year"][,lines(year,V1, col="red",lwd=2)]
	
}
mtext("# Species", side=2, line=0, outer=TRUE, cex=1.25)
abline(v=2004, lty="dotted", lwd=2)
mtext(bquote(underline(Model~~Richness)), side=3, line=0.25, outer=TRUE, cex=1.5)
dev.off()


# =========================================
# = Plot Stratum Dummy Richness over Time =
# =========================================

dummy.rich <- trawl2.veri[,list(dummy.rich=lu(spp)), by=c("s.reg","stratum","year")]

setkey(dummy.rich, s.reg, year, stratum)

auto.dim <- dummy.rich[,auto.mfrow(lu(s.reg), tall=F)]
# dev.new(width=4, height=(auto.dim[1]/auto.dim[2])*4)
# dev.new(width=(auto.dim[2]/auto.dim[1])*4, height=4)
png(width=(auto.dim[2]/auto.dim[1])*4, height=4, file="./trawl/Figures/Diversity/strat_rich_observed.png", units="in", res=200)
par(mfrow=auto.dim, mar=c(1.5,1,1.0,0.1), oma=c(0.5,1,1.5,0.1), cex=1, tcl=-0.1, mgp=c(1,0.1,0), ps=8)

MyGray2 <- rgb(t(col2rgb("black")), alpha=100, maxColorValue=255)

for(i in 1:length(dummy.rich[,unique(s.reg)])){
	
	t.sreg <- dummy.rich[,unique(s.reg)[i]]
	t.r <- dummy.rich[s.reg==t.sreg]
	
	maxR <- t.r[,max(dummy.rich)]
	minR <- t.r[,min(dummy.rich)]
	maxY <- t.r[,max(year)]
	minY <- t.r[,min(year)]
	
	u.strat <- t.r[,unique(stratum)]
	t.r[
		stratum==u.strat[1],
		plot(year, dummy.rich, ylim=c(minR, maxR), xlim=c(minY,maxY), main=regKey[as.character(unique(s.reg))], type="l", col=MyGray2, xlab="", ylab="")
	]
	for(j in 2:length(u.strat)){
		t.r[stratum==u.strat[j],lines(year,dummy.rich, col=MyGray2)]
	}
	t.r[,mean(dummy.rich), by="year"][,lines(year,V1, col="red",lwd=2)]
	
}
mtext("# Species", side=2, line=0, outer=TRUE, cex=1.25)
abline(v=2004, lty="dotted", lwd=2)
mtext(bquote(underline(Observed~~Richness)), side=3, line=0.25, outer=TRUE, cex=1.5)
dev.off()

# ==================
# = Dummy Rich Map =
# ==================

dummyLLs <- t(simplify2array(strsplit(dummy.rich[,stratum], " ")))
dummy.lon <- as.numeric(dummyLLs[,1])
dummy.lat <- as.numeric(dummyLLs[,2])
dummy.rich[,c("lon","lat"):=list(dummy.lon, dummy.lat)]

dummy.rich.mu <- dummy.rich[,list(stratum=unique(stratum), lon=unique(lon), lat=unique(lat), dummy.rich=mean(dummy.rich, na.rm=TRUE)), by=c("s.reg","stratum")]


png(height=4, width=dummy.rich.mu[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/avg_richness_observed.png", res=200, units="in")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
dummy.rich.mu[,z.col:=heat.cols[cut(dummy.rich, 256)]]
dummy.rich.mu[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")]
invisible(dummy.rich.mu[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
dummy.rich.mu[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points


# Key
dummy.rich.mu[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
dummy.rich.mu[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
dummy.rich.mu[,text(-167, y=seq(30,40, length.out=4), round(seq(min(dummy.rich, na.rm=TRUE), max(dummy.rich, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
dummy.rich.mu[,text(-162.5, 41.5, bquote(Mean~Observed~Richness~(number~spp)))] # add label for key

dev.off()



# ===================================
# = Plot Map of Site Richness Trend =
# ===================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
cT.rcoS[,z.col:=heat.cols[cut(slope.Nsite, 256)]]

# New device
# dev.new(height=4, width=cT.rcoS[,map.w(lat,lon,4)])
png(height=4, width=cT.rcoS[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/richness_slope_stratum_Nsite_cov.png", res=200, units="in")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
cT.rcoS[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")] # set plot region
invisible(cT.rcoS[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
cT.rcoS[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points

# Key
cT.rcoS[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
cT.rcoS[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
cT.rcoS[,text(-167, y=seq(30,40, length.out=4), round(seq(min(slope.Nsite, na.rm=TRUE), max(slope.Nsite, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
cT.rcoS[,text(-162.5, 41.5, bquote(Richness~Trend~(spp~~year^-1)))] # add label for key

dev.off()



# ===========================================
# = Plot Map of Richness Trend per Richness =
# ===========================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
cT.rcoS[,z.col:=heat.cols[cut(slope.Nsite/mu.Nsite, 256)]]

# New device
# dev.new(height=4, width=cT.rcoS[,map.w(lat,lon,4)])
png(height=4, width=cT.rcoS[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/richness_slope_stratum_Nsite_perN_cov.png", units="in", res=200)
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
cT.rcoS[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")] # set plot region
invisible(cT.rcoS[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
cT.rcoS[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points

# Key
cT.rcoS[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
cT.rcoS[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
cT.rcoS[,text(-167, y=seq(30,40, length.out=4), round(seq(min(slope.Nsite/mu.Nsite, na.rm=TRUE), max(slope.Nsite/mu.Nsite, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
cT.rcoS[,text(-162.5, 41.5, bquote('%'~Richness~Trend~(spp~~yr^-1~spp^-1)))] # add label for key

dev.off()




# ===========================================
# = Plot Map of Richness Trend per Richness =
# ===========================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
cT.rcoS[,z.col:=heat.cols[cut(mu.Nsite, 256)]]

# New device
# dev.new(height=4, width=cT.rcoS[,map.w(lat,lon,4)])
png(height=4, width=cT.rcoS[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/avg_richness_mu.Nsite_cov.png", units="in", res=200)
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
cT.rcoS[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")] # set plot region
invisible(cT.rcoS[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
cT.rcoS[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points

# Key
cT.rcoS[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
cT.rcoS[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
cT.rcoS[,text(-167, y=seq(30,40, length.out=4), round(seq(min(mu.Nsite, na.rm=TRUE), max(mu.Nsite, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
cT.rcoS[,text(-162.5, 41.5, bquote(Avg.~Richness))] # add label for key

dev.off()


# ===========================================
# = Category – Richness Hypothesis Beanplot =
# ===========================================
set.seed(1)
catNames <- c("None","Source","Divergence","Corridor","Convergence","Sink")
hypoDat <- data.frame(
	"categ"=factor(rep(catNames, each=100), levels=catNames),
	"rich"=c(
			rnorm(100, mean=0),
			rnorm(100, mean=-5),
			rnorm(100, mean=0, sd=2),
			rlnorm(100,meanlog=1),
			rnorm(100, mean=0, sd=2),
			rnorm(100, mean=-2, sd=1.5)
		)
)

png(width=3.5, height=3.5, file="./trawl/Figures/BioClimate/richTrend_category_bean_Hypothesis.png", res=200, units="in")
par(mfrow=c(1,1), mar=c(2.5,2.5,0.5,0.5), ps=10, cex=1, mgp=c(2, 0.4, 0), tcl=-0.1, family="Times", lwd=1, xpd=F)   
beanplot(rich~categ, data=hypoDat, ylab="", yaxt="n", xaxt="n", border=bLine, col=beanCol, ll=0.01, beanlinewd=1.5)
axis(side=2)
axis(side=1, labels=FALSE)
# axis(side=1, at=1:6, labels=unique(cT.rcoS[,categ]))
mtext(bquote(Richness~Trend~~(spp~~year^-1)), side=2, line=1.5)
text(x=(1:6), y=par("usr")[3]*1.075, labels=c("None","Source","Diverge","Corridor","Converge","Sink"), srt=45, offset=0, pos=2, xpd=TRUE)
legend("topleft", legend="Hypothesis", bty="n", cex=1.5, inset=c(-0.12,-0.05))
dev.off()





# ===========================================
# = Boxplots of Burrows Categs and Richness =
# ===========================================
boxplot(cT.rcoS[,slope.Nsite]~cT.rcoS[,categ])


boxplot(cT.rcoS[,slope.Nsite/mu.Nsite]~cT.rcoS[,categ])



# source("/Users/Battrd/Documents/School&Work/WiscResearch/FatTails/Scripts/Functions/fatPlot_Functions.R")

library(beanplot)

col5 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
bLine <- col5
bFill <- rgb(t(col2rgb(col5, alpha=TRUE)), alpha=125, maxColorValue=255)
beanCol <- list(c(bFill[1]),
				c(bFill[2]),
				c(bFill[3]),
				c(bFill[4]),
				c(bFill[5]),
				c(bFill[6])
				)
				
		


png(width=3.5, height=3.5, file="./trawl/Figures/BioClimate/richTrend_category_bean.png", res=200, units="in")
par(mfrow=c(1,1), mar=c(2.5,2.5,0.5,0.5), ps=10, cex=1, mgp=c(2, 0.4, 0), tcl=-0.1, family="Times", lwd=1, xpd=F)   
beanplot(cT.rcoS[,slope.Nsite]~cT.rcoS[,categ], ylab="", yaxt="n", xaxt="n", border=bLine, col=beanCol, ll=0.01, beanlinewd=1.5)
axis(side=2)
axis(side=1, labels=FALSE)
# axis(side=1, at=1:6, labels=unique(cT.rcoS[,categ]))
mtext(bquote(Richness~Trend~~(spp~~year^-1)), side=2, line=1.5)
text(x=(1:6), y=par("usr")[3]*1.075, labels=c("None","Source","Diverge","Corridor","Converge","Sink"), srt=45, offset=0, pos=2, xpd=TRUE)
dev.off()






# =======================================================
# = Beanplots of richness and categories in each region =
# =======================================================
col5 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
bLine <- col5
bFill <- rgb(t(col2rgb(col5, alpha=TRUE)), alpha=125, maxColorValue=255)
beanCol <- list(c(bFill[1]),
				c(bFill[2]),
				c(bFill[3]),
				c(bFill[4]),
				c(bFill[5]),
				c(bFill[6])
				)
				

usreg <- cT.rcoS[,unique(s.reg)]


png(width=4, height=8, file="./trawl/Figures/BioClimate/richTrend_category_bean_byRegion_rcoModel.png", res=200, units="in")
par(mfrow=c(5,2), mar=c(2.5,1.5,1,0.5), oma=c(0,1,0,0), ps=8, cex=1, mgp=c(2, 0.4, 0), tcl=-0.1, family="Times", lwd=1, xpd=F)
for(i in 1:length(usreg)){
	t.sreg <- usreg[i]
	sub.sreg <- cT.rcoS[,s.reg==t.sreg]
	beanplot(cT.rcoS[sub.sreg,slope.Nsite]~cT.rcoS[sub.sreg,categ], ylab="", yaxt="n", xaxt="n", border=bLine, col=beanCol, ll=0.01, beanlinewd=1.5)
	axis(side=2)
	axis(side=1, labels=FALSE)
	# axis(side=1, at=1:6, labels=unique(cT.rcoS[,categ]))
	ytext<-par("usr")[3] - 0.1*diff(range(cT.rcoS[sub.sreg,slope.Nsite]))
	text(x=(1:6), y=ytext, labels=c("None","Source","Diverge","Corridor","Converge","Sink"), srt=45, offset=0, pos=2, xpd=TRUE)
	mtext(regKey[t.sreg], side=3, line=0, adj=0, font=2)
}
mtext(bquote(Model~Richness~Trend~~(spp~~year^-1)), side=2, outer=TRUE, line=-1)
dev.off()



# =======================================================
# = Beanplots of OBSERVED richness and categories in each region =
# =======================================================
col5 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
bLine <- col5
bFill <- rgb(t(col2rgb(col5, alpha=TRUE)), alpha=125, maxColorValue=255)
beanCol <- list(c(bFill[1]),
				c(bFill[2]),
				c(bFill[3]),
				c(bFill[4]),
				c(bFill[5]),
				c(bFill[6])
				)
				

usreg <- cT.obsR[,unique(s.reg)]



png(width=4, height=8, file="./trawl/Figures/BioClimate/richTrend_category_bean_byRegion_Observed.png", res=200, units="in")
par(mfrow=c(5,2), mar=c(2.5,1.5,1,0.5), oma=c(0,1,0,0), ps=8, cex=1, mgp=c(2, 0.4, 0), tcl=-0.1, family="Times", lwd=1, xpd=F)
for(i in 1:length(usreg)){
	t.sreg <- usreg[i]
	sub.sreg <- cT.obsR[,s.reg==t.sreg]
	beanplot(cT.obsR[sub.sreg,slope.dr]~cT.obsR[sub.sreg,categ], ylab="", yaxt="n", xaxt="n", border=bLine, col=beanCol, ll=0.01, beanlinewd=1.5)
	axis(side=2)
	axis(side=1, labels=FALSE)
	# axis(side=1, at=1:6, labels=unique(cT.obsR[,categ]))
	ytext<-par("usr")[3] - 0.1*diff(range(cT.obsR[sub.sreg,slope.dr]))
	text(x=(1:6), y=ytext, labels=c("None","Source","Diverge","Corridor","Converge","Sink"), srt=45, offset=0, pos=2, xpd=TRUE)
	mtext(regKey[t.sreg], side=3, line=0, adj=0, font=2)
}
mtext(bquote(Observed~Richness~Trend~~(spp~~year^-1)), side=2, outer=TRUE, line=-1)

dev.off()




# ============================================
# = Beanplots of Beta D Variance vs Category =
# ============================================

col5 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
bLine <- col5
bFill <- rgb(t(col2rgb(col5, alpha=TRUE)), alpha=125, maxColorValue=255)
beanCol <- list(c(bFill[1]),
				c(bFill[2]),
				c(bFill[3]),
				c(bFill[4]),
				c(bFill[5]),
				c(bFill[6])
				)
				
		

png(width=3.5, height=3.5, file="./trawl/Figures/BioClimate/betaVar_category_bean.png", res=200, units="in")
par(mfrow=c(1,1), mar=c(2.5,2.5,0.5,0.5), ps=10, cex=1, mgp=c(2, 0.4, 0), tcl=-0.1, family="Times", lwd=1, xpd=F)   
beanplot(cT.rcoS.noAnn.b[,var.time]~cT.rcoS.noAnn.b[,categ], ylab="", yaxt="n", xaxt="n", border=bLine, col=beanCol, ll=0.01, beanlinewd=1.5)
axis(side=2)
axis(side=1, labels=FALSE)
# axis(side=1, at=1:6, labels=unique(cT.rcoS[,categ]))
mtext(bquote(Community~~Variance~~(beta)), side=2, line=1.5)
fig.range <- diff(range(par("usr")[3:4]))
fig.min <- par("usr")[3]
fig.txt <- fig.min-(0.1*abs(fig.min))
text(x=(1:6), y=fig.txt, labels=c("None","Source","Diverge","Corridor","Converge","Sink"), srt=45, offset=0, pos=2, xpd=TRUE)
dev.off()







# =================================================
# = Ternary Plot of Categ Components and Richness =
# =================================================
# heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
# tern.cols <- cT.rcoS[,heat.cols[cut(slope.Nsite, 256)]]
#
# tern.vals <- as.matrix(cT.rcoS[,list(nStart, nFT, nFinal)])
#
# ternaryplot(tern.vals, pch=20, cex=0.5, col=tern.cols, labels="outside")



# P(b + c/2, c * sqrt(3)/2)



# =======================================
# = Bottom temperature timeseries plots =
# =======================================
rescale <- function(x, minx=-1, maxx=1, na.rm=TRUE){
	return((maxx-minx)*(x-min(x, na.rm=na.rm))/(max(x-min(x, na.rm=na.rm), na.rm=na.rm)) + minx)
}

temp.ts = copy(rco.sy)
temp.ts[,c('num', 'nameID', 'N', 'Nsite', 'stratum', 'lon', 'lat', 'depth'):=NULL]
temp.ts <- temp.ts[,list(mu.btemp=mean(btemp, na.rm=TRUE)), by=c('s.reg', 'year')]
temp.ts <- temp.ts[,list(year, mu.btemp, mu.btemp.rs=rescale(mu.btemp)), by=s.reg] # rescaled -1 to 1

cols = brewer.pal(lu(temp.ts$s.reg), 'Paired')
regs = unique(temp.ts$s.reg)

# quartz(width=7, height=4)
png("./trawl/Figures/BioClimate/btemp_vs_year.png", width=7, height=4, units="in", res=200)
par(mfrow=c(1,2), mai=c(0.6, 0.6, 0.5, 0.1), mgp=c(1.7,0.5,0), las=1, tcl=-0.2, cex.axis=0.9, cex.lab=1.1)
temp.ts[,plot(year, mu.btemp, type='n', pch=16, cex=0.5, ylim=c(1,9), ylab='Bottom temperature °C', xlab='Year', main='Northern')] # for colder temps
for(i in 1:lu(temp.ts$s.reg)){ # there's probably a more efficient way to do this in data.table
	#temp.ts[s.reg==regs[i],lines(year, mu.btemp, col=cols[i], lwd=2)] # raw
	temp.ts[s.reg==regs[i],lines(lowess(y=mu.btemp, x=year, f=1/5), col=cols[i], lwd=2)] # lowess smoother
	#temp.ts[s.reg==regs[i], abline(lm(mu.btemp.rs ~ year), col=cols[i])] # linear trend
}

temp.ts[,plot(year, mu.btemp, type='n', pch=16, cex=0.5, ylim=c(19,25), ylab='', xlab='Year', main='Southern')] # for warmer temps
for(i in 1:lu(temp.ts$s.reg)){ # there's probably a more efficient way to do this in data.table
	#temp.ts[s.reg==regs[i],lines(year, mu.btemp, col=cols[i], lwd=2)]
	temp.ts[s.reg==regs[i],lines(lowess(y=mu.btemp, x=year, f=1/5), col=cols[i], lwd=2)]
}
legend('bottomright', legend=regKey, col=cols, lty=1, cex=0.7, bty='n')

dev.off()



# quartz(width=4, height=4)
png("./trawl/Figures/BioClimate/btemp_vs_year_EBS.png", width=7, height=4, units="in", res=200)
par(mfrow=c(1,1), mai=c(0.6, 0.6, 0.5, 0.1), mgp=c(1.7,0.5,0), las=1, tcl=-0.2, cex.axis=0.9, cex.lab=1.1)
temp.ts[s.reg=='ebs',plot(lowess(y=mu.btemp, x=year, f=1/5), type='l', col=cols[3], lwd=8, ylab='Bottom temperature °C', xlab='Year', main='EBS')] 

dev.off()


# ===================================
# = Richness vs. Bottom Temperature =
# ===================================
png("./trawl/Figures/BioClimate/Nsite_vs_btemp.png", width=3.5, height=3.5, units="in", res=200)
par(mar=c(2,2,0.1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1)
rco.sy[,plot(btemp, Nsite, col=rainbow(lu(s.reg))[as.factor(s.reg)], type="n",pch=20, xlab="Bottom Temperature", ylab="Site Richness")]
rco.sy[,points(btemp, Nsite, col=rainbow(lu(s.reg))[as.factor(s.reg)], cex=0.25, pch=20)]
dev.off()





# ==========================================
# = Richness vs. Surface Temperature Trend =
# ==========================================

setkey(cT.rcoS.noAnn, s.reg, lon, lat)
usreg <- cT.rcoS.noAnn[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/richTrend_vs_surfTrend_roc_noAnn_noColor_noTrend.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn[,plot((timeTrend), (slope.Nsite), pch=19, xlab="", ylab="")]
mtext(bquote(Surface~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
dev.off()




# ==========================================
# = Richness vs. Surface Temperature Trend =
# ==========================================

setkey(cT.rcoS, s.reg, lon, lat)
usreg <- cT.rcoS[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/richTrend_vs_surfTrend_roc.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS[,plot((timeTrend), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Surface~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS[s.reg==usreg[i], lm(slope.Nsite~timeTrend)]
	t.x <- cT.rcoS[s.reg==usreg[i],range(timeTrend, na.rm=TRUE)]
	t.y <- cT.rcoS[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(timeTrend=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}

dev.off()

# cT.rcoS[,Anova(lmer(slope.Nsite~timeTrend+(1|s.reg)))]
# cT.rcoS[,summary(lmer(slope.Nsite~timeTrend+(1|s.reg)))]

cT.rcoS.noAnn[,Anova(lmer(slope.Nsite~timeTrend+(1|s.reg)))]
cT.rcoS.noAnn[,summary(lmer(slope.Nsite~timeTrend+(1|s.reg)))]



# ======================================================================
# = Richness vs. Surface Temperature Trend: Correct WC by Dropping Ann =
# ======================================================================
qSlope <- function(x, y){if(length(x)<2){return(NA_real_)}else{lm(y~x)$coef[2]}}

rco.s.wc <- copy(rco.sy)
rco.s.wc[,nameID:=NULL]
rco.s.wc <- rco.s.wc[s.reg=="wc"&year<=2003,
	list(
		mu.btemp=mean(btemp, na.rm=TRUE),
		slope.btemp=qSlope(year,btemp),
		mu.depth=mean(depth, na.rm=TRUE),
		slope.depth=qSlope(year, depth),
		mu.N=mean(N, na.rm=TRUE),
		slope.N=qSlope(year, N),
		mu.Nsite=mean(Nsite, na.rm=TRUE),
		slope.Nsite=qSlope(year, Nsite)
	),
	by=c("s.reg","stratum","lon","lat")
]

setkey(rco.s.wc, lon, lat)
setkey(climTraj, lon, lat)
cT.rcoS.wc <- merge(rco.s.wc, climTraj)


# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/richTrend_vs_surfTrend_roc_drop_WC_Ann.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS[,plot((timeTrend), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Surface~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS[s.reg==usreg[i], lm(slope.Nsite~timeTrend)]
	t.x <- cT.rcoS[s.reg==usreg[i],range(timeTrend, na.rm=TRUE)]
	t.y <- cT.rcoS[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(timeTrend=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}


# Add in first part of WC sampling only (to exclude potential method bias)
cT.rcoS.wc[,points((timeTrend), (slope.Nsite), col=col.reg["wc"], cex=1.5, pch=19, xlab="", ylab="")]
cT.rcoS.wc[,points((timeTrend), (slope.Nsite), col="white", cex=0.75, pch=19, xlab="", ylab="")]
t.lm <- cT.rcoS.wc[s.reg==usreg[i], lm(slope.Nsite~timeTrend)]
t.x <- cT.rcoS.wc[s.reg==usreg[i],range(timeTrend, na.rm=TRUE)]
t.y <- cT.rcoS.wc[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
t.new <- predict(t.lm, newdata=data.frame(timeTrend=t.x))
segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=6, col="black")
segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=5, col=col.reg["wc"])
segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col="white", lty="dashed")


dev.off()


# ========================================
# = Richness vs. Surf Temp Trend: No Ann =
# ========================================

setkey(cT.rcoS.noAnn, s.reg, lon, lat)
usreg <- cT.rcoS.noAnn[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/richTrend_vs_surfTrend_roc_noAnn.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn[,plot((timeTrend), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Surface~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(slope.Nsite~timeTrend)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(timeTrend, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(timeTrend=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
dev.off()


# ===============================================
# = Richness vs. Surf Temp Trend: noAnn, noMean =
# ===============================================
setkey(cT.rcoS.noAnn, s.reg, lon, lat)
usreg <- cT.rcoS.noAnn[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

cT.rcoS.noAnn[,slope.Nsite.noMu:=slope.Nsite-meanna(slope.Nsite), by="s.reg"]

# dev.new(width=5.5, height=5)
# png("./trawl/Figures/BioClimate/richTrend_vs_surfTrend_roc_noAnn_noMean.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn[,plot((timeTrend), (slope.Nsite.noMu), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Surface~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(slope.Nsite.noMu~timeTrend)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(timeTrend, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(timeTrend=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
# dev.off()




# ======================================================================
# = Richness vs. BOTTOM Temperature Trend: Correct WC by Dropping Ann =
# ======================================================================
# qSlope <- function(x, y){if(length(x)<2){return(NA_real_)}else{lm(y~x)$coef[2]}}
#
# rco.s.wc <- copy(rco.sy)
# rco.s.wc[,nameID:=NULL]
# rco.s.wc <- rco.s.wc[s.reg=="wc"&year<=2003,
# 	list(
# 		mu.btemp=mean(btemp, na.rm=TRUE),
# 		slope.btemp=qSlope(year,btemp),
# 		mu.depth=mean(depth, na.rm=TRUE),
# 		slope.depth=qSlope(year, depth),
# 		mu.N=mean(N, na.rm=TRUE),
# 		slope.N=qSlope(year, N),
# 		mu.Nsite=mean(Nsite, na.rm=TRUE),
# 		slope.Nsite=qSlope(year, Nsite)
# 	),
# 	by=c("s.reg","stratum","lon","lat")
# ]
#
# setkey(rco.s.wc, lon, lat)
# setkey(climTraj, lon, lat)
# cT.rcoS.wc <- merge(rco.s.wc, climTraj)


# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/richTrend_vs_bottomTrend_roc_noAnn.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn[,plot((slope.btemp), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Bottom~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg], bg="transparent")

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(slope.Nsite~slope.btemp)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(slope.btemp, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(slope.btemp=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}

dev.off()




# ===============================
# = Beta D vs Bottom Temp trend =
# ===============================

# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/betaVarTrend_vs_bottomTrend_roc_noAnn.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn.b[,plot((slope.btemp), (var.time), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Bottom~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Community~~Variance), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg], bg="transparent")

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn.b[s.reg==usreg[i], lm(var.time~slope.btemp)]
	t.x <- cT.rcoS.noAnn.b[s.reg==usreg[i],range(slope.btemp, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn.b[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(slope.btemp=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}

dev.off()




# ===============================
# = Beta D vs Bottom Temp trend =
# ===============================

# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/betaTurnTrend_vs_bottomTrend_roc_noAnn.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn.b[,plot((slope.btemp), (turn.time), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Bottom~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Community~~Turnover), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg], bg="transparent")

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn.b[s.reg==usreg[i], lm(turn.time~slope.btemp)]
	t.x <- cT.rcoS.noAnn.b[s.reg==usreg[i],range(slope.btemp, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn.b[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(slope.btemp=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}

dev.off()






# ================================================================
# = Richness Trend vs. Surface Temp Trend, Colors are Categories =
# ================================================================
setkey(cT.rcoS.noAnn, s.reg, lon, lat)
cT.rcoS.noAnn[,categ:=factor(categ, levels=c("None","Source","Divergence","Corridor","Convergence","Sink"))]
ucateg <- cT.rcoS.noAnn[,unique(categ)]
col5 <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c")
col.cat <- col5
names(col.cat) <- ucateg
catNames <- c("None","Source","Divergence","Corridor","Convergence","Sink")

# dev.new(width=5.5, height=5)
png("./trawl/Figures/BioClimate/richTrend_vs_surfTrend_roc_noWCann_colorCateg.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS.noAnn[,plot((timeTrend), (slope.Nsite), col=col.cat[ucateg], pch=19, xlab="", ylab="")]
mtext(bquote(Surface~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=ucateg, pch=19, col=col.cat[ucateg])

for(i in 1:length(usreg)){
	# cT.rcoS.noAnn[,points((timeTrend), (slope.Nsite), col=col.cat[ucateg[i]], cex=1.5, pch=19, xlab="", ylab="")]
	# cT.rcoS.noAnn[,points((timeTrend), (slope.Nsite), col="white", cex=0.75, pch=19, xlab="", ylab="")]
	t.lm <- cT.rcoS.noAnn[categ==ucateg[i], lm(slope.Nsite~timeTrend)]
	t.x <- cT.rcoS.noAnn[categ==ucateg[i],range(timeTrend, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[categ==ucateg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(timeTrend=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.cat[ucateg[i]])
}


# =====================================
# = Barplot of # Category Per Region =
# =====================================
regKey2 <- structure(c("Aleutian\nIslands", "E. Bering\nSea", "Gulf of\nMexico", "Gulf of\nAlaska", "Northeast US","Newfoundland", "US South\nAtlantic", "S. Gulf of\nSt. Lawrence", "Scotian\nShelf", "West\nCoast"), .Names =c("ai", "ebs", "gmex", "goa", "neus", "newf", "sa", "sgulf", "shelf", "wc"))
rowProp <- function(x)apply(x, 1, function(x)x/sum(x))
propCategReg <- rowProp(table(cT.rcoS.noAnn[,s.reg], cT.rcoS.noAnn[,categ]))
colnames(propCategReg) <- regKey[colnames(propCategReg)]
# propCategReg <- melt(propCategReg, measure.vars=colnames(propCategReg))

richSlopes <- t(cT.rcoS.noAnn[,mean(slope.Nsite), by="s.reg"][,V1])
propCategReg <- rbind(propCategReg, richSlopes/max(richSlopes))
rownames(propCategReg) <- c(rownames(propCategReg)[1:6], "Richness Slope")

png(width=8.5, height=3, file="./trawl/Figures/BioClimate/region_category_slopeRich_barplot.png", res=200, units="in")
par(mfrow=c(1,1), mar=c(2.5,2.5,0.5,0.5), ps=8, cex=1, mgp=c(3, 0.75, 0), tcl=-0.5, family="Times", lwd=1, xpd=F)

barplot(
	propCategReg, 
	beside=TRUE, 
	names.arg=regKey2, 
	col=c(col5, "black"), 
	legend.text=c(levels(ucateg),"Richness Slope"), 
	args.legend=list(x="topright", bty="n", inset=c(0.075,-0.075), ncol=2), 
	ylim=c(-0.2, 1)
)

dev.off()

cT.rcoS.noAnn[,Anova(lmer(slope.Nsite~categ+(1|s.reg)))]



# =================================
# = Richness vs. Climate Velocity =
# =================================
setkey(cT.rcoS, s.reg, lon, lat)
usreg <- cT.rcoS[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

# dev.new(width=5.5, height=5)
# png("./trawl/Figures/BioClimate/richTrend_vs_climateV_roc.png", width=5.5, height=5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

cT.rcoS[,plot((climV), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Climate~Velocity~~(km~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS[s.reg==usreg[i], lm(slope.Nsite~climV)]
	t.x <- cT.rcoS[s.reg==usreg[i],range(climV, na.rm=TRUE)]
	t.y <- cT.rcoS[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(climV=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
# dev.off()

# Yuck

cT.rcoS[,Anova(lmer(slope.Nsite~climV+(1|s.reg)))]
cT.rcoS[,summary(lm(slope.Nsite~climV))]



# ================================
# = Richness Trend vs. Lon & Lat =
# ================================
png(width=9, height=4, file="./trawl/Figures/BioClimate/richnessTrend_vs_LonLat.png", res=200, units="in")
par(mfrow=c(1,2), mar=c(2.5,2.5,0.5,0.5), ps=8, cex=1, mgp=c(3, 0.2, 0), tcl=-0.25, family="Times", lwd=1, xpd=F)


cT.rcoS.noAnn[,plot((lon), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Longitude), side=1, line=1.25)
mtext(bquote(Richness~Trend~~(species~~year^-1)), side=2, line=1.25)
legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg], cex=0.75, ncol=1, bg=NA)

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(slope.Nsite~lon)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(lon, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lon=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(cT.rcoS.noAnn[,slope.Nsite]~cT.rcoS.noAnn[,lon]), lwd=3, lty="dashed")


cT.rcoS.noAnn[,plot((lat), (slope.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25)
# mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
# legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(slope.Nsite~lat)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(lat, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(slope.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lat=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(cT.rcoS.noAnn[,slope.Nsite]~cT.rcoS.noAnn[,lat]), lwd=3, lty="dashed")

dev.off()






# ================================
# = Richness Mean vs. Lon & Lat =
# ================================
png(width=9, height=4, file="./trawl/Figures/BioClimate/richnessMean_vs_LonLat.png", res=200, units="in")
par(mfrow=c(1,2), mar=c(2.5,2.5,0.5,0.5), ps=8, cex=1, mgp=c(3, 0.2, 0), tcl=-0.25, family="Times", lwd=1, xpd=F)


cT.rcoS.noAnn[,plot((lon), (mu.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Longitude), side=1, line=1.25)
mtext(bquote(Mean~Richness), side=2, line=1.25)
legend("bottomleft", legend=regKey[usreg], pch=19, col=col.reg[usreg], cex=0.75, ncol=2, bg=NA)

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(mu.Nsite~lon)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(lon, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(mu.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lon=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(cT.rcoS.noAnn[,mu.Nsite]~cT.rcoS.noAnn[,lon]), lwd=3, lty="dashed")




cT.rcoS.noAnn[,plot((lat), (mu.Nsite), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25)

for(i in 1:length(usreg)){
	t.lm <- cT.rcoS.noAnn[s.reg==usreg[i], lm(mu.Nsite~lat)]
	t.x <- cT.rcoS.noAnn[s.reg==usreg[i],range(lat, na.rm=TRUE)]
	t.y <- cT.rcoS.noAnn[s.reg==usreg[i],range(mu.Nsite, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lat=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(cT.rcoS.noAnn[,mu.Nsite]~cT.rcoS.noAnn[,lat]), lwd=3, lty="dashed")

dev.off()




# ac.x <- seq(3, 10, by=0.01)
# ac.y <- 1/ (1 + exp(-1.5*ac.x))
# png("~/Desktop/curve.png", bg="transparent")
# par(mar=rep(0,4), oma=rep(0,4), lwd=4, col="white")
# plot(ac.x, ac.y, xlab="", ylab="", xaxt="n", yaxt="n", type="l", bty="l")
# dev.off()


# ==========================
# = Dummy Rich vs. Lon/Lat =
# ==========================
# prep data and colors
qSlope <- function(x, y){if(length(x)<2){return(NA_real_)}else{lm(y~x)$coef[2]}}
dummy.rich <- trawl2.veri[,list(dummy.rich=lu(spp)), by=c("s.reg","stratum","year")]
ll <- t(dummy.rich[,(strsplit(stratum, split=" "))])
names(ll) <- NULL
dummy.rich[,c("lon","lat"):=list(as.numeric(ll[,1]), as.numeric(ll[,2]))]
dummy.rich <- dummy.rich[s.reg!="wc" | (s.reg=="wc" & year<=2003)]
dr <- dummy.rich[,list(mu.dr=mean(dummy.rich), slope.dr=qSlope(year, dummy.rich)), by=c("s.reg","stratum","lon","lat")]

setkey(cT.rcoS.noAnn, s.reg, lon, lat)
usreg <- cT.rcoS.noAnn[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg


# Two figures: lon on left, lat on right, with trend lines
png(width=9, height=4, file="./trawl/Figures/BioClimate/richnessMean_vs_LonLat_observed.png", res=200, units="in")
par(mfrow=c(1,2), mar=c(2.5,2.5,0.5,0.5), ps=8, cex=1, mgp=c(3, 0.2, 0), tcl=-0.25, family="Times", lwd=1, xpd=F)


dr[,plot((lon), (mu.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Longitude), side=1, line=1.25)
mtext(bquote(Mean~Observed~Richness), side=2, line=1.25)
legend("topleft", legend=regKey[usreg], pch=19, col=col.reg[usreg], cex=0.75, ncol=2, bg=NA)

for(i in 1:length(usreg)){
	t.lm <- dr[s.reg==usreg[i], lm(mu.dr~lon)]
	t.x <- dr[s.reg==usreg[i],range(lon, na.rm=TRUE)]
	t.y <- dr[s.reg==usreg[i],range(mu.dr, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lon=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(dr[,mu.dr]~dr[,lon]), lwd=3, lty="dashed")




dr[,plot((lat), (mu.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25)

for(i in 1:length(usreg)){
	t.lm <- dr[s.reg==usreg[i], lm(mu.dr~lat)]
	t.x <- dr[s.reg==usreg[i],range(lat, na.rm=TRUE)]
	t.y <- dr[s.reg==usreg[i],range(mu.dr, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lat=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(dr[,mu.dr]~dr[,lat]), lwd=3, lty="dashed")

dev.off()


# One figure: mean richness vs. lat, with overall trend line
png(width=4, height=4, file="./trawl/Figures/BioClimate/richnessMean_vs_Lat_observed.png", res=200, units="in")
par(mfrow=c(1,1), mar=c(2.5,2.5,0.5,0.5), ps=8, cex=1, mgp=c(3, 0.2, 0), tcl=-0.25, family="Times", lwd=1, xpd=F, cex.axis=1.2)

dr[,plot((lat), (mu.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25, cex=2)
mtext(bquote(Mean~Observed~Richness), side=2, line=1.25, cex=2)

abline(mod<-lm(dr[,mu.dr]~dr[,lat]), lwd=3, lty="dashed")
summary(mod)

dev.off()



# Two figure: mean richness vs. lat by East or West Coast, with overall trend lines
png(width=9, height=4, file="./trawl/Figures/BioClimate/richnessMean_vs_Lat_observed_byCoast.png", res=200, units="in")
# quartz(width=9, height=4)
par(mfrow=c(1,2), mar=c(2.5,2.5,2,0.5), ps=8, cex=1, mgp=c(3, 0.2, 0), tcl=-0.25, family="Times", lwd=1, xpd=F)
wcoast = c('ai', 'ebs', 'goa', 'wc')
ecoast = c('gmex', 'neus', 'newf', 'sa', 'sgulf', 'shelf')

dr[dr$s.reg %in% wcoast,plot((lat), (mu.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25, cex=2)
mtext(bquote(Mean~Observed~Richness), side=2, line=1.25, cex=2)
mtext(bquote(West~Coast), side=3, line=0.5, cex=2)

abline(mod<-lm(dr[dr$s.reg %in% wcoast,mu.dr]~dr[dr$s.reg %in% wcoast,lat]), lwd=2, lty="dashed")
summary(mod)


dr[dr$s.reg %in% ecoast,plot((lat), (mu.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25, cex=2)
mtext(bquote(East~Coast), side=3, line=0.5, cex=2)

abline(mod<-lm(dr[dr$s.reg %in% ecoast,mu.dr]~dr[dr$s.reg %in% ecoast,lat]), lwd=2, lty="dashed")
summary(mod)

dev.off()




# Richness trend vs. lon (left) or lat (right)
png(width=9, height=4, file="./trawl/Figures/BioClimate/richnessTrend_vs_LonLat_observed.png", res=200, units="in")
par(mfrow=c(1,2), mar=c(2.5,2.5,0.5,0.5), ps=8, cex=1, mgp=c(3, 0.2, 0), tcl=-0.25, family="Times", lwd=1, xpd=F)


dr[,plot((lon), (slope.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Longitude), side=1, line=1.25)
mtext(bquote(Observed~Richness~Trend~~(species~~year^-1)), side=2, line=1.25)
legend("bottomleft", legend=regKey[usreg], pch=19, col=col.reg[usreg], cex=0.75, ncol=2, bg=NA)

for(i in 1:length(usreg)){
	t.lm <- dr[s.reg==usreg[i], lm(slope.dr~lon)]
	t.x <- dr[s.reg==usreg[i],range(lon, na.rm=TRUE)]
	t.y <- dr[s.reg==usreg[i],range(slope.dr, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lon=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(dr[,slope.dr]~dr[,lon]), lwd=3, lty="dashed")


dr[,plot((lat), (slope.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Latitude), side=1, line=1.25)
# mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
# legend("topright", legend=regKey[usreg], pch=19, col=col.reg[usreg])

for(i in 1:length(usreg)){
	t.lm <- dr[s.reg==usreg[i], lm(slope.dr~lat)]
	t.x <- dr[s.reg==usreg[i],range(lat, na.rm=TRUE)]
	t.y <- dr[s.reg==usreg[i],range(slope.dr, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(lat=t.x))
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}
abline(lm(dr[,slope.dr]~dr[,lat]), lwd=3, lty="dashed")

dev.off()



# =============================
# = Dummy Rich vs. Temp Trend =
# =============================
# prep data and colors
qSlope <- function(x, y){if(length(x)<2){return(NA_real_)}else{lm(y~x)$coef[2]}}
dummy.rich <- trawl2.veri[,list(dummy.rich=lu(spp)), by=c("s.reg","stratum","year")]
ll <- t(dummy.rich[,(strsplit(stratum, split=" "))])
names(ll) <- NULL
dummy.rich[,c("lon","lat"):=list(as.numeric(ll[,1]), as.numeric(ll[,2]))]
dummy.rich <- dummy.rich[s.reg!="wc" | (s.reg=="wc" & year<=2003)]
dr <- dummy.rich[,list(mu.dr=mean(dummy.rich), slope.dr=qSlope(year, dummy.rich)), by=c("s.reg","stratum","lon","lat")]
setkey(dr, s.reg, lon, lat)
setkey(cT.rcoS.noAnn, s.reg, lon, lat)

drtemp = merge(dr, cT.rcoS.noAnn[,c('lon', 'lat', 's.reg', 'slope.btemp'), with=FALSE])

usreg <- dr[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg


# quartz(width=5.5, height=5.5)
png("./trawl/Figures/BioClimate/richTrend_vs_bottomTrend_observed.png", width=5.5, height=5.5, res=200, units="in")
par(mar=c(2.5,2.75,0.2,1), mgp=c(1,0.25,0), tcl=-0.15, cex=1, ps=10)

drtemp[,plot((slope.btemp), (slope.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
#drtemp[,plot((slope.btemp), (slope.dr/mu.dr), col=col.reg[s.reg], pch=19, xlab="", ylab="")]
mtext(bquote(Bottom~Temperature~Trend~~(phantom()*degree*C~~year^-1)), side=1, line=1.5)
mtext(bquote(Local~Species~Richness~Trend~~(species~~year^-1)), side=2, line=1.5)
legend("bottomleft", legend=regKey[usreg], pch=19, col=col.reg[usreg], bg="transparent")

for(i in 1:length(usreg)){
	t.lm <- drtemp[s.reg==usreg[i], lm(slope.dr~slope.btemp)]
	t.x <- drtemp[s.reg==usreg[i],range(slope.btemp, na.rm=TRUE)]
	t.y <- drtemp[s.reg==usreg[i],range(slope.dr, na.rm=TRUE)]
	t.new <- predict(t.lm, newdata=data.frame(slope.btemp=t.x))
	#segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=4, col="black")
	segments(x0=t.x[1], y0=t.new[1], x1=t.x[2], y1=t.new[2], lwd=2, col=col.reg[usreg[i]])
}

dev.off()


# ===================================
# = Plot Map of Observed Site Richness Trend =
# ===================================
# prep data and colors
qSlope <- function(x, y){if(length(x)<2){return(NA_real_)}else{lm(y~x)$coef[2]}}
dummy.rich <- trawl2.veri[,list(dummy.rich=lu(spp)), by=c("s.reg","stratum","year")]
ll <- t(dummy.rich[,(strsplit(stratum, split=" "))])
names(ll) <- NULL
dummy.rich[,c("lon","lat"):=list(as.numeric(ll[,1]), as.numeric(ll[,2]))]
dummy.rich <- dummy.rich[s.reg!="wc" | (s.reg=="wc" & year<=2003)]
dr <- dummy.rich[,list(mu.dr=mean(dummy.rich), slope.dr=qSlope(year, dummy.rich)), by=c("s.reg","stratum","lon","lat")]
setkey(dr, s.reg, lon, lat)

usreg <- dr[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
dr[,z.col:=heat.cols[cut(slope.dr, 256)]]

# New device
# dev.new(height=4, width=dr[,map.w(lat,lon,4)])
png(height=4, width=cT.rcoS[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/richness_slope_stratum_observed.png", res=200, units="in")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
dr[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")] # set plot region
invisible(dr[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
dr[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points

# Key
dr[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
dr[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
dr[,text(-167, y=seq(30,40, length.out=4), round(seq(min(slope.dr, na.rm=TRUE), max(slope.dr, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
dr[,text(-162.5, 41.5, bquote(Richness~Trend~(spp~~year^-1)))] # add label for key

dev.off()



# ==============
# = Region Map =
# ==============
png(height=4, width=cT.rcoS[,map.w(lat,lon,4)], file="./trawl/Figures/regionMap.png", res=250, units="in")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=10, family="Times", cex=1)

usreg <- cT.rcoS.noAnn[,unique(s.reg)]
col.reg <- rainbow(length(usreg))
names(col.reg) <- usreg

cT.rcoS.noAnn[,plot(lon, lat, bg=col.reg[s.reg], pch=21, cex=1, type="n")] # set plot region
invisible(cT.rcoS.noAnn[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
cT.rcoS.noAnn[,points(lon, lat, bg=col.reg[s.reg], pch=21, cex=1)] # set plot region

legend("bottomleft", legend=regKey[usreg], pch=19, col=col.reg[usreg])

dev.off()