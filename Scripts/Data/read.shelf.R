# =================
# = Load packages =
# =================
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

# ====================
# = Source Functions =
# ====================
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rm9s.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/calcarea.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")


# =====================
# = Set preliminaries =
# =====================
# String to begin directory
shelf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/DFO_ScotianShelf/"

# ======================
# = Read in catch data =
# ======================
shelf.catch0 <- fread(paste(shelf.start, "gscat_adj_pinsky.csv", sep=""), colClasses=c(rep("character",4),rep("numeric",3), rep("character",3)), drop=c("SAMPWGT","MARKET","CALWT", "REMARKS", "SIZE_CLASS"))
shelf.catch <- shelf.catch0
setnames(shelf.catch, c("ADJ_TOTWGT", "ADJ_TOTNO"), c("wtcpue", "cntcpue"))
setkey(shelf.catch, MISSION, SETNO)


# ====================
# = Read in Set Data =
# ====================
shelf.set0 <- fread(paste(shelf.start, "gsinf_pinsky.csv", sep=""), colClasses=c(rep("character",5), rep("numeric",15),rep("character",3),"numeric",rep("character",2), rep("numeric",5)), drop=c("TIME","ELAT","ELONG","AREA","DUR","DIST","HOWS", "AUX", "SPEED","WIND","FORCE","CURNT","GEAR", "ETIME", "REMARKS", "START_DEPTH","END_DEPTH", "BOTTOM_SALINITY"))

# get good depth for shelf, based on what's available (malin line 112)
shelf.set0[is.na(DEPTH), DEPTH:=(DMIN+DMAX)/2]
shelf.set0[is.na(DEPTH)&is.na(DMIN)&!is.na(DMAX), DEPTH:=DMAX]
shelf.set0[is.na(DEPTH)&is.na(DMAX)&!is.na(DMIN), DEPTH:=DMIN]

setnames(shelf.set0, c("STRAT","DEPTH", "SURFACE_TEMPERATURE", "BOTTOM_TEMPERATURE"), c("stratum","depth", "stemp", "btemp"))

shelf.set <- shelf.set0[,list(MISSION, SETNO, SDATE, stratum, SLAT, SLONG, TYPE, depth, stemp, btemp)]


# ==================
# = Read in Strata =
# ==================
shelf.strata0 <- fread(paste(shelf.start, "ScotianShelf_strata.csv", sep=""), drop=c("depthzone_fathoms"))
shelf.strata0[,stratumarea:=stratarea_nmi2*1.852^2]
shelf.strata <- shelf.strata0[,list(stratum, stratumarea)]
setkey(shelf.strata, stratum)


# ===================
# = Read in Species =
# ===================
shelf.spp <- fread(paste(shelf.start, "species list.csv", sep=""))
setnames(shelf.spp, "CODE", "SPEC")
shelf.spp[,SPEC:=as.character(SPEC)]
setkey(shelf.spp, SPEC)



# ============================
# = First Merge: Catch & Set =
# ============================
shelf.raw000 <- merge(shelf.catch, shelf.set, all.x=TRUE, all.y=FALSE)


# ================================
# = Add Dates and Haul to raw000 =
# ================================
# Add Dates to raw000
shelf.raw000[,SDATE:=gsub("-", "/", SDATE)]
shelf.raw000[,datetime:=as.POSIXct(SDATE, format="%y/%m/%d", tz="GMT")]
shelf.raw000[,month:=as.numeric(format.Date(datetime, format="%m"))]
shelf.raw000[,year:=as.numeric(format.Date(datetime, format="%Y"))]

# Add Haul to raw000
shelf.raw000[,haulid:=paste(MISSION, formatC(SETNO, width=3, flag=0))]


# =====================================
# = Trim TYPE and month, create raw00 =
# =====================================
# malin trimmed TYPE on line 40, and trimmed month on line 76
cols2keep <- quote(list(year, month, datetime, haulid, stratum, SLAT, SLONG, depth, SPEC, stemp, btemp, wtcpue, cntcpue))
# shelf.raw00 <- shelf.raw000[TYPE==1 & month>=6 & month<= 8, list(year, month, datetime, haulid, stratum, SLAT, SLONG, depth, SPEC, stemp, btemp, wtcpue, cntcpue)] 
shelf.raw00 <- shelf.raw000[TYPE==1 & month>=6 & month<= 8, eval(cols2keep)] 


# ==================================
# = Subset to Strata sampled often =
# ==================================
# shelf.raw00[,rowSums(table(stratum, year)>=1)] # most strata were sampled every year (42 years is mode and max)
# shelf.raw00[,colSums(table(stratum, year)>=1)] # most years sampled between 48 and 51 strata

shelf.YS <- shelf.raw00[,rowSums(table(stratum, year)>=1)] # the number of years (Y) in each stratum (S)
shelf.YS.pick <- names(shelf.YS)[shelf.YS==max(shelf.YS)] # max() still works on class()=="character"
shelf.raw0 <- shelf.raw00[stratum%in%shelf.YS.pick,] # vector scan instead of binary search, but idc



# ===============
# = Fix lat/lon =
# ===============
# malin line 119 & 120
shelf.raw0[,lat:=as.numeric(substr(SLAT,1,2))+as.numeric(substr(SLAT,3,4))/60]
shelf.raw0[,lon:=as.numeric(substr(SLONG,1,2))-as.numeric(substr(SLONG,3,4))/60]


# =========================
# = Remove 9999 and -9999 =
# =========================
# replaces either of those values with NA; similar to malin line 133
rm9s(shelf.raw0)


# =======================================
# = Add in species, then begin trimming =
# =======================================
shelf.raw0 <- merge(shelf.raw0, shelf.spp, all.x=TRUE, by="SPEC")


# Note that Malin looked for some duplicates (year-lat-lon, same haulid), but I won't do this removal
# which(duplicated(shelf.raw0[,list(year, lat, lon)]) & !duplicated(shelf.raw0$haulid)) # Malin line 176
# Malin line 198 - 218 basically loop through rows with
# duplicated year-lat-lon, but different haulid,
# and pick a haulid to keep based on the number of non-na temp (s and b) readings it has
# I will eventually just sum (for wt/ cnt) and mean (for temps) based on 















