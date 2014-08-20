


library(bit64)
library(data.table)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")

# ====================
# = Read in raw data =
# ====================

gmex.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/SEAMAP-GMex/2014-06-25/"

gmex.bio000 <- fread(paste(gmex.start, "BGSREC_ryan.csv", sep=""), sep=",", colClasses=c(rep("integer",7), rep("character",3), rep("integer",2), "numeric", "numeric", "character", rep("character",3)), drop=c("SAMPLE_BGS", "NODC_BGS", "IS_SAMPLE", "TAXONID", "CNT"))

gmex.sta <- fread(paste(gmex.start, "STAREC_ryan.csv", sep=""), select=c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', 'VESSEL_SPD', 'COMSTAT'))

gmex.tow <- fread(paste(gmex.start, "INVREC_ryan.csv", sep=""), colClasses=c(rep("integer",7), "character", "numeric","character", "integer", rep("character",3), rep("numeric", 12)), select=c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP'))

gmex.spp0 <- fread(paste(gmex.start, "NEWBIOCODESBIG_ryan.csv", sep=""), colClasses=c("integer", "character", "character", "integer", "integer", "character", "character"))

gmex.cruises <- fread(paste(gmex.start, "CRUISES_ryan.csv", sep=""), select=c("CRUISEID", "VESSEL", "TITLE"))


# gmex.glf <- fread(paste(gmex.start, "GLFREC_ryan.csv", sep="")) # has length data

# ================================
# = Subset and clean up gmex.bio =
# ================================
# setkey(gmex.bio00, BGSCODE, GENUS_BGS)
gmex.bio00 <- gmex.bio000[BGSCODE!="T" & GENUS_BGS!="UNKNOWN"] # this is actually faster than using a key
# gmex.bio0 <- gmex.bio00[!"T"] # [!.(unique(BGSCODE), "UNKNOWN")] note that I can't find any of the unknown in there
# dim(gmex.bio00[!"T"] [!.(unique(BGSCODE), "UNKNOWN")])

setkey(gmex.bio00, CRUISEID, STATIONID, VESSEL, CRUISE_NO, P_STA_NO, GENUS_BGS, SPEC_BGS, BGSCODE, BIO_BGS, SELECT_BGS)
gmex.bio0 <- unique(gmex.bio00)
gmex.bio0 <- setkey(gmex.bio0, NULL)

# ===================
# = Fix up gmex.spp =
# ===================
bad.gmex.CODE <- names(gmex.spp0[,table(CODE)][gmex.spp0[,table(CODE)] > 1])
good.gmex.CODE <- names(gmex.spp0[,table(CODE)][gmex.spp0[,table(CODE)] <= 1])

setkey(gmex.spp0, CODE)
gmex.spp <- gmex.spp0[good.gmex.CODE]


newspp <- data.table(Key1 = c(503L,5770L), TAXONOMIC = c('ANTHIAS TENUIS AND WOODSI', 'MOLLUSCA AND UNID.OTHER #01'), CODE=bad.gmex.CODE, TAXONSIZECODE=as.integer(NA), isactive=-1L, common_name=c('threadnose and swallowtail bass', 'molluscs or unknown'), tsn=as.character(NA), key="CODE") # redefine the many-species codes

gmex.spp <- rbind(gmex.spp, newspp, use.names=TRUE) # add the redefined codes back to gmex.spp

gmex.spp <- gmex.spp[,list(CODE, TAXONOMIC)]

# ===================
# = Subset gmex.tow =
# ===================
gmex.tow <- gmex.tow[GEAR_TYPE=="ST"]

# =============
# = Form gmex =
# =============
gmex <- merge(gmex.bio0, gmex.tow, by=intersect(names(gmex.bio0), names(gmex.tow)), all.x=TRUE)

gmex <- merge(gmex, gmex.sta, by=intersect(names(gmex.bio0), names(gmex.sta)), all.x=TRUE)

setkey(gmex, BIO_BGS)
setnames(gmex.spp, "CODE", "BIO_BGS")
setkey(gmex.spp, BIO_BGS)
gmex <- merge(gmex, gmex.spp, all.x=TRUE) # SOURCE OF THE PROBLEM, LINE 112 & LINE 117 MALIN'S CODE





