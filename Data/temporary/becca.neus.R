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


load("./trawl/Data/MSOM/prepd.msom.cov.RData")




neus <- trawl[s.reg=="neus"]


neus[, lat2:=sapply(strsplit(stratum, " "), function(x){x[2]})]
neus[,lat:=lat2]
neus[,lat:=NULL]

neus[, lon2:=sapply(strsplit(stratum, " "), function(x){x[1]})]
neus[,lon:=lon2]
neus[,lon:=NULL]

setnames(neus, c("lat2","lon2"), c("lat", "lon"))


neus[, btemp:=mean(btemp, na.rm=TRUE), by=c("year", "stratum")]
neus[, stemp:=mean(stemp, na.rm=TRUE), by=c("year", "stratum")]
neus[, depth:=mean(depth, na.rm=TRUE), by=c("year", "stratum")]

neus[,c("correctSpp","taxLvl"):=NULL]



neus[,c("lat","lon"):=list(as.numeric(lat), as.numeric(lon))]


# =====================
# = get the PA object =
# =====================
# from https://gist.github.com/rBatt/b4bfba056e7cf1bec55a
pa <- neus[,list(stratum, lon, lat, pa=1)]
pa <- pa[!duplicated(stratum)]
setkey(pa, lon, lat)
pa0 <- copy(pa)
pa[,stratum:=NULL]


skeleton <- pa[,expand.grid(lon=seq(min(lon),max(lon),by=0.1), lat=seq(min(lat),max(lat),by=0.1))]
skeleton <- as.data.table(skeleton)
setkey(skeleton, lon, lat)
pa <- pa[skeleton]

pa[is.na(pa), pa:=0]


pa[paste(roundGrid(lon), roundGrid(lat))%in%pa0[,stratum], pa:=1]

pa.cast <- reshape2:::acast(pa, lat~lon)

# pa2 <- as.data.frame(pa)
# pa2[is.na(pa2[,"pa"])] <- 0

# ========
# = Save =
# ========
save(neus, file="./trawl/Data/Temporary/neus.RData")
save(pa, file="./trawl/Data/Temporary/pa.RData")
save(pa.cast, file="./trawl/Data/Temporary/pa.cast.RData")