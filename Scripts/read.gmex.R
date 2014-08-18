

library(data.table)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")


gmex.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/SEAMAP-GMex/2014-06-25/"

gmex.bg <- fread(paste(gmex.start, "BGSREC_ryan.csv", sep=""), sep=",", colClasses=c(rep("integer",5), rep("character",3), rep("integer",2), "numeric", "integer64"))

gmex.sta <- fread(paste(gmex.start, "STAREC_ryan.csv", sep=""), select=c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', 'VESSEL_SPD', 'COMSTAT'))

gmex.tow <- fread(paste(gmex.start, "INVREC_ryan.csv", sep=""), colClasses=c(rep("integer",7), "character", "numeric","character", "integer", rep("character",3), rep("numeric", 12)), select=c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP'))

gmex.spp <- fread(paste(gmex.start, "NEWBIOCODESBIG_ryan.csv", sep=""), colClasses=c("integer", "character", "integer64", "integer", "integer", "character", "integer64"))

gmex.cruises <- fread(paste(gmex.start, "CRUISES_ryan.csv", sep=""), select=c("CRUISEID", "VESSEL", "TITLE"))


# gmex.glf <- fread(paste(gmex.start, "GLFREC_ryan.csv", sep="")) # has length data





