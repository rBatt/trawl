# =================
# = Load Packages =
# =================
library(reshape2)
library(data.table)


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


# =============
# = Load Data =
# =============
load("./trawl/Data/OS_2016/ebs.RData")


# =======================================
# = Determine Source-Colonization pairs =
# =======================================
#' Determine which strata could have been the source for the species that colonized another stratum.
#' If a species is absent in stratum S1 in year=t-1, and is present in S1 in year=t, then that species is said to have colonized S1 in t.
#' To determine where that species could have come from, see which other strata had that species in year=t-1. So if the species was present in S2-5 in t-1, then those strata could all have been potential "sources" for the colonization of S1 by that species in year t.
#' By mapping the relationship between these strata, it might become apparent how species tend to move across this region. I.e., colonization events are being used as biological tracers of community dynamics in space and time.
uy <- ebs[,unique(year)]
us <- ebs[,unique(spp)]
ustrat <- ebs[,unique(stratum)]

# test <- ebs[stratum%in%c("-158.5 57.5","-176.5 61.5") & spp%in%c("Acantholithodes hispidus","Zaprora silenus","Yoldia hyperborea")]
# test <- ebs[spp%in%c("Anarhichas orientalis","Zaprora silenus","Yoldia hyperborea")]
t.dt <- data.table(year=uy, key="year")

ebs.zCol <- ebs[,j={
	t.yz <- .SD[,list(Z,c.now), key="year"]
	t.dt2 <- t.yz[t.dt]
	t.dt2[,Z.last:=t.dt[,c(NA, Z[-length(Z)])]]
	# t.dt2[,spp:=.SD[,unique(spp)]]
},by=c("stratum","spp")]



arr.Z <- acast(ebs.zCol, year~stratum~spp, value.var="Z")
arr.Zlast <- acast(ebs.zCol, year~stratum~spp, value.var="Z.last")
arr.cnow <- acast(ebs.zCol, year~stratum~spp, value.var="c.now")

# construct array to hold colonization sources
# 1st dimension is the year of the local colonization
# 2nd dimension is the location (stratum) that could have supplied the colonizing species
	# i.e., strata where species was present previous year
# 3rd dimension is the species
# 4th dimension is the location (stratum) of the actual colonization
c.source.arr <- array(
	NA, 
	dim=c(dim(arr.Z),length(ustrat)), 
	dimnames=list(year=uy, donor=ustrat, spp=us, colonized=ustrat)
) 
# Check dimnames!!!
# Don't get "donor" strata (d2) mixed up with "colonized" strata (d4)
# same as in transition matrix cell r,c gives mass from location row=r to location col=c)
# names(dimnames(c.source.arr))

for(i in 1:dim(arr.Z)[3]){ # loop through species
	z.last.mat <- arr.Zlast[,,i] # grab last year's locations of species i
	for(j in 1:dim(arr.Z)[2]){ # loop through colonized strata
		t.cnow <- arr.cnow[,j,i] # grab stratum experiencing colonization of species i this year
		t.sum <- (t.cnow + z.last.mat)
		c.source.arr[,,i,j] <- (t.sum == 2 & !is.na(t.sum)) # identify possible sources of colonization
	}
}

donor.source.ts <- apply(c.source.arr, c(2,4,1), sum)

png("./trawl/Figures/Colonization/ebs.colonizationSource.stratumConnectivity.png", width=6, height=7, res=200, units="in")
par(mfrow=c(7,4), mar=c(0.75,0.5,0.25,0.1), ps=8, tcl=-0.15, mgp=c(0.5,0.05,0), oma=c(1.5,1.5,1.5,0.1))
for(i in 5:32){
	image.plot(x=1:86, y=1:86, z=donor.source.ts[,,i], xlab="source", ylab="colonized", axis.args=list(mgp=c(0.5,0.15,0)))
	title("")
	mtext(paste("t =",i), side=3, line=-0.15, font=2, adj=0, cex=1)
}
title("")
mtext("EBS; Panels are years, color is # of spp colonizing X from Y", side=3, line=0.5, outer=T)
mtext("Source Stratum", side=1, line=0.5, outer=T)
mtext("Colonized Stratum", side=2, line=0.5, outer=T)
dev.off()



# trawl[!is.na(lat) & !is.na(lon) & s.reg=="ebs"][!duplicated(stratum), j={plot(lon, lat, type="p", cex=2, pch=20, col=adjustcolor(tim.colors(86),alpha.f=0.25)); for(i in 1:length(lon)){text(lon[i],lat[i],i, cex=0.5)}}]

t1 <- ebs[!is.na(lat) & !is.na(lon)]
t2 <- t1[!duplicated(stratum)]

col <- adjustcolor(tim.colors(86),alpha.f=0.25)
lon <- as.numeric(t2[,lon])
lat <- as.numeric(t2[,lat])

png("./trawl/Figures/Colonization/ebs.colonizationSource.stratumKey.png", width=5, height=5, res=200, units="in")
plot(lon, lat, type="p", cex=2, pch=20, col=col) 
for(s in 1:length(lon)){
	text(lon[s],lat[s],s, cex=0.5)
}
dev.off()


# area represented by our analysis, 
# assuming that samples within a 1º grid cell are representative 
# of an area exactly equal to the area of that 1º cell
# (i.e., if we were to sample exhaustively in that cell, 
# and assume the findings were not applicable to any other area)
# blah <- trawl[is.finite(lon) & is.finite(lat)][!duplicated(stratum), ll2km(lon-0.5, lat-0.5)] - trawl[is.finite(lon) & is.finite(lat)][!duplicated(stratum), ll2km(lon+0.5, lat+0.5)]
# sum(abs(blah[,lat.km*lon.km]))




