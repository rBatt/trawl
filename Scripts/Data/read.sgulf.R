

# =================
# = Load packages =
# =================
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

# # ====================
# # = Source Functions =
# # ====================
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


# =====================
# = Set preliminaries =
# =====================
# String to begin directory
sgulf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/DFO_SouthernGulf/"

# ======================
# = Read in catch data =
# ======================
sgulf.catch00 <- fread(paste(sgulf.start, "southern Gulf survey data.csv", sep=""), colClasses=c(rep("character",3),"numeric", rep("character",2), rep("numeric",4), "character", rep("numeric",8), rep("character",2), rep("numeric",2)), drop=c("depthst", "depthend", "dtow", "N", "kg", "temperature"))

sgulf.catch0 <- sgulf.catch00[!((is.na(catch)|catch==0) & (is.na(biomass)|biomass==0)) & expt==1,]

sgulf.catch0[,haulid:=paste(vessel, cruise, set, sep="-")]
sgulf.catch0[,datetime:=paste(paste(year, month, day, sep="-"), gsub("(?<=\\d)([\\d]{2})(?=$)", ":\\1", time, perl=TRUE))]
# sgulf.catch0[,datetime:=as.POSIXct(datetime)]

setnames(sgulf.catch0, c("catch", "biomass", "latitude", "longitude", "latin_name", "name", "strat"), c("cntcpue", "wtcpue", "lat", "lon", "spp", "common", "stratum"))

sgulf.catch <- sgulf.catch0[,list(spp, common, year, datetime, haulid, stratum, depth, lat, lon, cntcpue, wtcpue)]
setkey(sgulf.catch, spp, year, datetime, haulid, stratum)

# ====================
# = Read in Set Data =
# ====================
sgulf.set0 <- fread(paste(sgulf.start, "sGSL_RV Survey sets_1971_2009_ryan.csv", sep=""), colClasses=c(rep("character",4), rep("numeric",12)))

sgulf.set0[,haulid:=paste(vessel, cruise, set, sep="-")]
sgulf.set0[,datetime:=paste(paste(year, month, day, sep="-"), gsub("(?<=\\d)([\\d]{2})(?=$)", ":\\1", time, perl=TRUE))]
# sgulf.set0[,datetime:=as.POSIXct(datetime)]

setnames(sgulf.set0, c("t_surface", "t_bottom", "strat"), c("stemp", "btemp", "stratum"))

sgulf.set <- sgulf.set0[expt==1,list(year, datetime, haulid, stratum, stemp, btemp)]
setkey(sgulf.set, year, datetime, haulid, stratum)


# ==================
# = Read in Strata =
# ==================
sgulf.strata <- fread(paste(sgulf.start, "4T_RV_strata.csv", sep=""), drop="trawlableunits")
# sgulf.strata0[,stratumarea:=stratarea_nmi2*1.852^2]
setnames(sgulf.strata, "stratarea", "stratumarea")
sgulf.strata[,stratum:=as.character(stratum)]
setkey(sgulf.strata, stratum)


# ============================
# = First Merge: Catch & Set =
# ============================
sgulf.raw000 <- merge(sgulf.catch, sgulf.set, all.x=TRUE, all.y=FALSE)

# =============================
# = Also merge in stratumarea =
# =============================
sgulf.raw00 <- merge(sgulf.raw000, sgulf.strata, all.x=TRUE, by="stratum")


# ==================================
# = Subset to Strata sampled often =
# ==================================
# sgulf.raw00[,rowSums(table(stratum, year)>=1)] # most strata were sampled every year (42 years is mode and max)
# sgulf.raw00[,colSums(table(stratum, year)>=1)] # most years sampled between 48 and 51 strata

sgulf.YS <- sgulf.raw00[,rowSums(table(stratum, year)>=1)] # the number of years (Y) in each stratum (S)
sgulf.YS.pick <- names(sgulf.YS)[sgulf.YS==max(sgulf.YS)] # max() still works on class()=="character"
sgulf.raw0 <- sgulf.raw00[stratum%in%sgulf.YS.pick,] # vector scan instead of binary search, but idc

# some checks to make sure that being this selective with strata is still preserving spp obs and no biasing
# sgulf.raw0[,length(unique(spp))] # 33 spp if use only the strata sampled most often
# sgulf.raw000[,length(unique(spp))] # 33 spp in original data set, so not losing any species
# mean(sgulf.raw000[,colSums(table(spp, year)>1)]) # ~27 spp observed per year in original data
# mean(sgulf.raw0[,colSums(table(spp, year)>1)]) # ~ 24 spp observed per year after trimming strata
# plot(sgulf.raw000[,colSums(table(spp, year)>1)] - sgulf.raw0[,colSums(table(spp, year)>1)]) # no pattern that would provide evidence of loss of spp/year over time (i.e., trimming strata doesn't make 1971 look more or less spp rich than 2009)

# =========================
# = Remove 9999 and -9999 =
# =========================
# rm9s(sgulf.raw0)
# any(!is.na(sgulf.raw0)&sgulf.raw0>9990&sgulf.raw0<10000) # 9's aren't a problem


# =======================================
# = Add in species, then begin trimming =
# =======================================
# sgulf.raw0[,unique(spp)] # very well behaved species names


# =========================
# = Make final data.table =
# =========================
sgulf <- sgulf.raw0[,list(year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, stemp, btemp, wtcpue, cntcpue)]

setkey(sgulf, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
sgulf2 <- sgulf[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(sgulf)]
# there actually weren't any rows being aggregated, but I'll keep the previous line just as a safety net if more data are added


# ===============
# = Add regions =
# ===============
sgulf2[,region:="DFO_SoGulf"]
sgulf2[,s.reg:="sgulf"]


# ========
# = Save =
# ========
save(sgulf2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/sgulf2.RData")

