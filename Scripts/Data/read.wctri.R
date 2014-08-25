

library(data.table)
library(bit64)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rm9s.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/calcarea.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")



wctri.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_WestCoast/2011-12-08/"


wctri.catch <- fread(paste(wctri.start,"CATCHWCTRIALLCOAST.csv", sep=""), drop=c("REGION","SUBSAMPLE_CODE","VOUCHER","AUDITJOIN"))
rmWhite(wctri.catch)

# wctri.cruise.raw <- fread(paste(wctri.start,"CRUISEWCTRIALLCOAST.csv", sep=""), drop=c("REGION","AGENCY_NAME","AUDITJOIN"))


wctri.haul <- fread(paste(wctri.start,"HAULWCTRIALLCOAST.csv", sep=""), drop=c("GEAR_DEPTH","REGION","NET_MEASURED","NET_HEIGHT","BOTTOM_TYPE","WIRE_LENGTH","GEAR","ACCESSORIES","SUBSAMPLE","AUDITJOIN"))
rmWhite(wctri.catch)

# wctri.length.raw <- fread(paste(wctri.start,"LENGTHWCTRIALLCOAST.csv", sep=""))


wctri.species <- fread(paste(wctri.start,"RACEBASE_SPECIES.csv", sep=""), select=c("SPECIES_CODE","SPECIES_NAME","COMMON_NAME"))
rmWhite(wctri.species)

# =========================
# = Merge to create wctri =
# =========================

wctri000 <- merge(wctri.catch, wctri.haul, by=intersect(names(wctri.catch), names(wctri.haul)), all.x=TRUE)

wctri00 <- merge(wctri000, wctri.species, by=intersect(names(wctri000), names(wctri.species)), all.x=TRUE)

wctri <- wctri00[HAUL_TYPE==3 & PERFORMANCE==0,]


# ==============
# = Add haulid =
# ==============
wctri[,haulid:=paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE, width=3, flag=0), formatC(HAUL, width=3, flag=0), sep='-')]


# ================
# = Extract Year =
# ================
wctri[,year:=as.numeric(substr(CRUISE, 1, 4))]

# ==============
# = Add strata =
# ==============
wctri[,stratum:=paste(floor(START_LATITUDE)+0.5, floor(BOTTOM_DEPTH/100)*100+50, sep="-")]



# ================================
# = Trim Strata (malin line 165) =
# ================================
wctri <- wctri[wctri$stratum %in% c("36.5-50", "37.5-150", "37.5-50", "38.5-150", "38.5-250", "38.5-350", "38.5-50", "39.5-150", "39.5-50", "40.5-150", "40.5-250", "41.5-150", "41.5-250", "41.5-50", "42.5-150", "42.5-250", "42.5-50", "43.5-150", "43.5-250", "43.5-350", "43.5-50", "44.5-150", "44.5-250", "44.5-350", "44.5-50", "45.5-150", "45.5-350", "45.5-50", "46.5-150", "46.5-250", "46.5-50", "47.5-150", "47.5-50", "48.5-150", "48.5-250", "48.5-50"),]



# ================
# = Stratum area =
# ================
wctri[,stratumarea:=calcarea(cbind(START_LONGITUDE, START_LATITUDE)), by=stratum]


# ================
# = name columns =
# ================
setnames(wctri, c("VESSEL", "START_LATITUDE", "START_LONGITUDE", "BOTTOM_DEPTH", "SPECIES_NAME", "WEIGHT", "SURFACE_TEMPERATURE", "GEAR_TEMPERATURE", "START_TIME", "NUMBER_FISH"), c("svvessel", "lat", "lon", "depth", "spp", "wtcpue", "stemp", "btemp", "datetime", "cntcpue"))

# ============
# = Get cpue =
# ============
wctri[,wtcpue:=wtcpue*1E4/(DISTANCE_FISHED*1E3*NET_WIDTH)]
wctri[,cntcpue:=cntcpue*1E4/(DISTANCE_FISHED*1E3*NET_WIDTH)]


# ==================
# = Remove bad spp =
# ==================
setkey(wctri, spp)
wctri.spp.bad <- c("","Apristurus brunneus egg case", "fish eggs unident.", "Raja binoculata egg case", "Raja sp. egg case", "Rajiformes egg case", "Shark egg case unident.")
wctri <- wctri[!.(wctri.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================

i <- sapply(wctri, is.factor)
if(any(i)){
	wctri[i] <- lapply(wctri[i], as.character)
}



wctri[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(wctri, spp)
wctri[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajwctrinterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(wctri, spp)



# =============
# = Aggregate =
# =============
setkey(wctri, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
wctri2 <- wctri[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(wctri)]



# ==============
# = Add region =
# ==============
wctri2[,region:="AFSC_WTri"]
wctri2[,s.reg:="wctri"]



# ========
# = Save =
# ========
save(wctri2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/wctri2.RData")


