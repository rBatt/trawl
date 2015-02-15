

# =================
# = Load Packages =
# =================
library(data.table)
library(fields)


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
load("./trawl/Results/Richness/ao.rco.RData")


# rco[,spp.depth:=weighted.mean(depth, w=Z, na.rm=TRUE), by=c("s.reg","spp")]
regKey <- c("ai"="Aleutian Islands", "ebs"="Eastern Bering Strait", "gmex"="Gulf of Mexico", "goa"="Gulf of Alaska", "neus"="Northeast US", "newf"="Newfoundland", "sa"="US South Atlantic", "sgulf"="Southern Gulf of St. Lawrence", "shelf"="Scotian Shelf", "wcann"="West Coast (ann)", "wctri"= "West Coast (tri)")



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

MyGray <- rgb(t(col2rgb("black")), alpha=25, maxColorValue=255)

# NOTE:  RUNNING THIS COMMAND FOR MORE THAN 1 REGION PER R SESSION HAS BEEN RESULTING IN AN ERROR FOR ME! SEGFAULT MEMORY ERROR. DO 1 AT A TIME
chooseReg <- "ebs"
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
smplt <- c(0.9,0.92, 0.2,0.9)
bgplt <- c(0.1,0.89,0.125,0.95)
axargs <- list(mgp=c(0.5,0.25,0))

rco[,
	j={
		depthGrad <- seq(min(depth, na.rm=TRUE),max(depth, na.rm=TRUE), length.out=200)
		tempGrad <- seq(min(btemp, na.rm=TRUE),max(btemp, na.rm=TRUE), length.out=200)
		
		auto.dim <- auto.mfrow(lu(year))
		
		# zlim <- range(Nsite, na.rm=TRUE)*c(0.5, 1.15)
		xlim <- range(depthGrad)
		ylim <- range(tempGrad)
		
		z.lim <- .SD[,
			j={
				get1 <- stratum==unique(stratum)[1]
				t.aList <- list(u.a0=u.a0[get1], a1=a1[get1], a2=a2[get1], a3=a3[get1], a4=a4[get1])
				depth.temp.N <- outer(depthGrad, tempGrad, n.from.cov, aList=t.aList)
				list(min(depth.temp.N), max(depth.temp.N))
			},
			by=c("year")
		][,list(V1,V2)]
		# print(z.lim)
		
		zlim <- range(z.lim)
		
		# print(zlim)
		
		# zlim <- range(outer(xlim, ylim, n.from.cov, aList=list(u.a0=range(u.a0), a1=range(a1), a2=range(a2), a3=range(a3), a4=range(a4))))*lu(stratum)
		
		# dev.new(width=(auto.dim[2]/auto.dim[1])*7, height=7)
		png(paste0("./trawl/Figures/Diversity/msomCov/",unique(s.reg),"_depth.temp.N.png"), width=(auto.dim[2]/auto.dim[1])*7, height=7, res=200, units="in")
		par(mfrow=auto.dim, mar=c(2,2,1,1), oma=c(0.75,0.75,1.5,0.5), cex=1, mgp=c(0.5,0.15,0), tcl=-0.1, ps=8, xaxs="i", yaxs="i")
		
		.SD[,
			j={
				get1 <- stratum==unique(stratum)[1]
				t.aList <- list(u.a0=u.a0[get1], a1=a1[get1], a2=a2[get1], a3=a3[get1], a4=a4[get1])
				depth.temp.N <- outer(depthGrad, tempGrad, n.from.cov, aList=t.aList)
				
				# Heat map
				image.plot(x=depthGrad, y=tempGrad, z=depth.temp.N, smallplot=smplt, bigplot=bgplt, axis.args=axargs, xlim=xlim, ylim=ylim, zlim=zlim, ylab="", xlab="")
				box()
				par(cex=1)

				points(depth, btemp, col=MyWhite, pch=20, cex=0.25)
				
				mtext(side=3, line=-0.1, .BY[[1]], font=2, outer=F, adj=0.05, cex=1)
			},
			by=c("year")
		]
		
		mtext(side=1, line=-0.25, "Depth (m)", outer=TRUE)
		mtext(side=2, line=-0.25, "Temperature", outer=TRUE)
		mtext(side=3, line=0.25, regKey[.BY[[1]]], font=2, cex=1.5, outer=TRUE)
		
		dev.off()
		
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
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

i=1
useNum <- gsub("[a-z]+_[0-9]{4}_([0-9]){1}$", "\\1", names(all.out), perl=TRUE)
t.ao <- all.out[[i]] #[[useNum[i]]]
dev.new(height=1.5, width=map.w(t.ao$ID$lat, t.ao$ID$lon, height=1.5))
par(mar=c(1.5,1.5,0.2,1), oma=c(0,0,0,0), cex=1, mgp=c(0.75,0.1,0), tcl=-0.1, ps=8, xaxs="r", yaxs="r")
t.cols <- heat.cols[cut(t.ao$median$Nsite, 256)]
plot(t.ao$ID$lon, t.ao$ID$lat, pch=21, bg=t.cols)
map(add=TRUE,fill=FALSE)


drape.color(matrix(1:10, ncol=3))




# ==============================================
# = Latitudinal Gradient in Richness Over Time =
# ==============================================


