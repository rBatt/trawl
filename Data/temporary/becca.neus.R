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


# ========
# = Save =
# ========
save(neus, file="./trawl/Data/Temporary/neus.RData")