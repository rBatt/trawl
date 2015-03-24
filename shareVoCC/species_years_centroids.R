

library(bit64)
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)
library(reshape2)


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


# ======================
# = Read Centroid Data =
# ======================
# Longitude Data
cent.lat <- fread("./trawl/Science2013/Results/centbiolat_2012-10-16.csv")[,V1:=NULL] # read
cent.lat.melt <- melt(cent.lat, id.vars=c("regspp", "region", "spp"), variable.name="year", value.name="spp.centroid.lat") # format
setkey(cent.lat.melt, regspp, region, spp, year)

# Latitude Data
cent.lon <- fread("./trawl/Science2013/Results/centbiolon_2012-10-16.csv")[,V1:=NULL] # read
cent.lon.melt <- melt(cent.lon, id.vars=c("regspp", "region", "spp"), variable.name="year", value.name="spp.centroid.lon") # format
setkey(cent.lon.melt, regspp, region, spp, year)

# Centroid Depth
cent.depth <- fread("./trawl/Science2013/Results/centbiodepth_2012-11-15.csv")[,V1:=NULL] # read; depths is in meters, I believe
cent.depth.melt <- melt(cent.depth, id.vars=c("regspp", "region", "spp"), variable.name="year", value.name="spp.centroid.depth") # format
setkey(cent.depth.melt, regspp, region, spp, year)


# ===========================
# = Merge Centroid Lon/ Lat =
# ===========================
centroids.ll <- merge(cent.lat.melt, cent.lon.melt)

centroids <- merge(centroids.ll, cent.depth.melt)

# centroids[,plot(lon, lat)]

# ==================
# = Save Centroids =
# ==================
write.csv(centroids, file="./trawl/Science2013/Results/pinsky2013_centroids.csv", row.names=FALSE)





