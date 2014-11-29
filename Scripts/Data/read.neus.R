

# ==================
# = Load Libraries =
# ==================
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rm9s.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/calcarea.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")


# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =============
# = Read Data =
# =============
neus.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NEFSC/2014-03-23/"

neus.strata <- fread(paste(neus.start, "neusStrata.csv", sep=""), select=c('StratumCode', 'Areanmi2')) # Need neusStrata.csv file from Malin (18-Aug-2014)
local({ # create a local environment to read in .RData, to ensure that other objects aren't overwritten

	load(paste(neus.start, "station.RData", sep="")) # station
	load(paste(neus.start, "Survdat.RData", sep="")) # survdat
	load(paste(neus.start, "SVSPP.RData", sep="")) # spp
	
	# assign variables in global environment (if not found in local environment)
	# neus.station <<- station
	# neus.survdat <<- survdat
	# neus.spp <<- spp
	
	# assign variables in global environment
	assign("neus.station", station, envir=.GlobalEnv)
	assign("neus.survdat.raw", survdat, envir=.GlobalEnv)
	assign("neus.spp", spp, envir=.GlobalEnv)
	
}) # end expressions to be carried out in new local environment
rmWhite(neus.station)
rmWhite(neus.survdat.raw)
rmWhite(neus.spp)

# ==================
# = Trim/ fix data =
# ==================
# based largely on Malin's code

# make changes to neus.spp
neus.spp[,c('ITISSPP', 'COMNAME', 'AUTHOR') := NULL] # remove some columns from spp data.table
neus.spp[,SVSPP:=as.character(SVSPP)]

# make changes to neus.strata
setkey(neus.strata, StratumCode)
setnames(neus.strata, "StratumCode", "STRATUM") # updates the key, too; this was done b/c data.table:::merege.data.table does not accept by.x and by.y

# make changes to neus.survdat.raw
setkey(neus.survdat.raw, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)

# begin creating neus from neus.survdat.raw
neus000 <- unique(neus.survdat.raw) # drops length data
neus000[, c('LENGTH', 'NUMLEN') := NULL] # remove length columns
neus000[,c("SEASON", "SVSPP"):=list(as.character(SEASON), as.character(SVSPP))]

setkey(neus000, SEASON)
neus00 <- neus000["SPRING"]

neus0 <- neus00[j=lapply(list(BIOMASS=BIOMASS, ABUNDANCE=ABUNDANCE), FUN=sumna), by=list(YEAR, SEASON, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM, SVSPP)] # sum different sexes of same spp together; also, as a byproduct, this drops the key from neus000
# setnames(neus00, 'V1', 'wtcpue')
setkey(neus0, SEASON, SVSPP, YEAR, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM) # resetting key that was lost during combining sexes


# Merge spp and strata into neus data.table
neus <- merge(neus0, neus.spp, by="SVSPP") # add species names
neus <- merge(neus, neus.strata, by="STRATUM", all.x=TRUE)

# ====================================================
# = Create Haul ID, merge station info (temperature) =
# ====================================================
neus[,haulid:=paste(formatC(CRUISE6, width=6, flag=0), formatC(STATION, width=3, flag=0), formatC(STRATUM, width=4, flag=0), sep='-')]

neus.station[,haulid:=paste(formatC(CRUISE6, width=6, flag=0), formatC(STATION, width=3, flag=0), formatC(STRATUM, width=4, flag=0), sep='-')]
neus.station <- neus.station[,list(haulid, SURFTEMP, BOTTEMP)]

setkey(neus, haulid)

neus <- merge(neus, neus.station, all.x=TRUE)



# =============
# = Set Names =
# =============
setnames(neus, c("YEAR", "SCINAME", "LAT", "LON", "DEPTH", "STRATUM", "SURFTEMP", "BOTTEMP", "BIOMASS", "ABUNDANCE"), c("year", "spp", "lat", "lon", "depth", "stratum", "stemp", "btemp", "wtcpue", "cntcpue"))

# ================================
# = Trim Strata (malin line 163) =
# ================================
# neus <- neus[neus$STRATUM %in% c("1010", "1020", "1030", "1040", "1050", "1060", "1070", "1080", "1090", "1100", "1110", "1130", "1140", "1150", "1160", "1170", "1190", "1200", "1210", "1220", "1230", "1240", "1250", "1260", "1270", "1280", "1290", "1300", "1340", "1360", "1370", "1380", "1400", "1650", "1660", "1670", "1680", "1690", "1700", "1710", "1730", "1740", "1750"), ] # strata to keep (based on Nye et al. MEPS)

# ===============
# = Trim Strata =
# ===============
nyears <- neus[,length(unique(year))]

# neus[,sum(colSums(table(year, stratum)>0)==nyears)] # original strata gives 43 strata seen every year

neus[,strat2:=paste(stratum, ll2strat(lon, lat))]
# neus[,sum(colSums(table(year, strat2)>0)==nyears)] # 1º grid gives you 19 strata seen every year
neus[,sum(colSums(table(year, strat2)>0)>=(nyears-3))]

# neus[,strat2:=paste(stratum, ll2strat(lon, lat, 0.5))]
# neus[,sum(colSums(table(year, strat2)>0)==nyears)] # 0.5º grid gives you 3 strata seen every year
#
# neus[,strat2:=paste(stratum, ll2strat(lon, lat, 0.25))]
# neus[,sum(colSums(table(year, strat2)>0)==nyears)] # 0.25º grid gives you 0 strata seen every year



# nstrata <- c()
# nstrata.orig <- c()
# for(i in 0:(nyears-1)){
# 	nstrata[i+1] <- neus[,sum(colSums(table(year, strat2)>0)>=(nyears-i))]
# 	nstrata.orig[i+1] <- neus[,sum(colSums(table(year, stratum)>0)>=(nyears-i))]
# }
# dev.new(width=4)
# par(mfrow=c(2,1), mar=c(2.5,2,1.5,0.2), cex=1, ps=10, mgp=c(1.25, 0.15, 0), tcl=-0.25)
# plot(0:(nyears-1), nstrata, type="o", xlab="threshold # years missing", ylab="# strata below threshold missingness", main="# strata vs. tolerance of missingness")
# lines(0:(nyears-1), nstrata.orig, type="o", col="red")
# legend("topleft", legend=c("original strata definition", "1 degree grid definition"), lty=1, pch=21, col=c("red","black"))
# image(x=neus[,sort(unique(year))], y=neus[,1:length(unique(strat2))], z=neus[,table(year, strat2)>0], xlab="year", ylab="1 degree stratum ID", main="stratum presence vs. time; red is absent")


thresholdYears <- 3

goodStrat2 <- neus[,names(colSums(table(year, strat2)>0))[colSums(table(year, strat2)>0)>=(nyears-thresholdYears)]]
neus <- goa[strat2%in%goodStrat2]
neus[,stratum:=strat2]
neus[,strat2:=NULL]


# =================================
# = Stratum Area (malin line 181) =
# =================================
neus$stratumarea <- neus$Areanmi2 * 3.429904 # convert square nautical miles to square kilometers
neus[,stratumarea:=Areanmi2*3.429904]


# ==================
# = Remove bad spp =
# ==================
setkey(neus, spp)
neus <- neus[!is.na(spp)]
neus.spp.bad <- c("",'UNIDENTIFIED FISH', 'ILLEX ILLECEBROSUS EGG MOPS', 'LOLIGO PEALEII EGG MOPS')
neus <- neus[!.(neus.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================
i <- sapply(neus, is.factor)
if(any(i)){
	neus[i] <- lapply(neus[i], as.character)
}



# =============
# = Aggregate =
# =============
neus[,datetime:=as.character(year)]
# setkey(neus, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# neus2 <- neus[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(neus)]
# neus2 <- neus[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(neus)] # I think cpue should be avgd

setkey(neus, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
neus2 <- neus[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(neus)]


# ==============
# = Add region =
# ==============
neus2[,region:="NEFSC_NEUSSpring"]
neus2[,s.reg:="neus"]

# ========
# = Save =
# ========
save(neus2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/neus2.RData")


