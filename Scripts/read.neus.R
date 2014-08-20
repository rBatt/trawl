


library(data.table)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")


neus.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NEFSC/2014-03-23/"

# =============
# = Read Data =
# =============
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

neus00 <- neus000[,sum(BIOMASS),by=list(YEAR, SEASON, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM, SVSPP)] # sum different sexes of same spp together; also, as a byproduct, this drops the key from neus000
setnames(neus00, 'V1', 'wtcpue')
setkey(neus00, SEASON, SVSPP, YEAR, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM) # resetting key that was lost during combining sexes
neus0 <- neus00["SPRING"] # trim to spring survey only; note that SEASON has to be the first key in order for this to work


# Merge spp and strata into neus data.table
neus <- merge(neus0, neus.spp, by="SVSPP") # add species names
neus <- merge(neus, neus.strata, by="STRATUM", all.x=TRUE)


