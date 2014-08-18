
library(data.table)
library(bit64)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")

wctri.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_WestCoast/2011-12-08/"


wctri.catch.raw <- fread(paste(wctri.start,"CATCHWCTRIALLCOAST.csv", sep=""), drop=c("REGION","SUBSAMPLE_CODE","VOUCHER","AUDITJOIN"))


# wctri.cruise.raw <- fread(paste(wctri.start,"CRUISEWCTRIALLCOAST.csv", sep=""), drop=c("REGION","AGENCY_NAME","AUDITJOIN"))


wctri.haul.raw <- fread(paste(wctri.start,"HAULWCTRIALLCOAST.csv", sep=""), drop=c("GEAR_DEPTH","REGION","NET_MEASURED","NET_HEIGHT","BOTTOM_TYPE","WIRE_LENGTH","GEAR","ACCESSORIES","SUBSAMPLE","AUDITJOIN"))


# wctri.length.raw <- fread(paste(wctri.start,"LENGTHWCTRIALLCOAST.csv", sep=""))


wctri.species.raw <- fread(paste(wctri.start,"RACEBASE_SPECIES.csv", sep=""), select=c("SPECIES_CODE","SPECIES_NAME","COMMON_NAME"))





