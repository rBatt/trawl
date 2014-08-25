library(data.table)
library(bit64)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/calcarea.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/sumna.R")


# ====================
# = Read in Raw Data =
# ====================
# read in main ai data
ai1.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/ai1983_2000.csv")
setnames(ai1.raw, names(ai1.raw), gsub("^\\s* | \\s*$", "", names(ai1.raw)))

ai2.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/ai2002_2012.csv")
setnames(ai2.raw, names(ai2.raw), gsub("^\\s* | \\s*$", "", names(ai2.raw)))

ai.raw <- rbind(ai1.raw, ai2.raw)

setkey(ai.raw, STRATUM)

# read in ai strata
aiStrata <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/aiStrata.csv", select=c("StratumCode", "Areakm2"))
setnames(aiStrata, names(aiStrata), gsub("^\\s* | \\s*$", "", names(aiStrata)))
setnames(aiStrata, "StratumCode", "STRATUM")
setkey(aiStrata, STRATUM)

# merge ai with strata
ai <- merge(ai.raw, aiStrata, all.x=TRUE)

# ====================
# = Clean up ai data =
# ====================
rmWhite(ai) # remove whitespace in the elements of each column
rm9s(ai) # check each column for 9999, and replace with NA


# ==================
# = Add ai haul ID =
# ==================

ai[,haulid:=paste(formatC(VESSEL, width=3, flag=0), formatC(CRUISE, width=3, flag=0), formatC(HAUL, width=3, flag=0), sep='-')]


# ===================================
# = Trim Strata (line 160 of malin) =
# ===================================
ai <- ai[!(ai$STRATUM %in% c(221, 411, 421, 521, 611)),]


# =============
# = Fix names =
# =============
setnames(ai, c("STRATUM", "YEAR", "LATITUDE", "LONGITUDE", "BOT_DEPTH", "SCIENTIFIC", "WTCPUE", "Areakm2", "BOT_TEMP", "SURF_TEMP", "DATETIME"), c("stratum", "year", "lat", "lon", "depth", "spp", "wtcpue", "stratumarea", "btemp", "stemp", "datetime"))

# ==================
# = Remove bad spp =
# ==================

setkey(ai, spp)
ai.spp.bad <- c("","Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")
ai <- ai[!.(ai.spp.bad)]



# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================

i <- sapply(ai, is.factor)
if(any(i)){
	ai[i] <- lapply(ai[i], as.character)
}


ai[.(c('Atheresthesevermanni', 'Atheresthesstomias')), spp:='Atheresthessp.']; setkey(ai, spp)
ai[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(ai, spp)
ai[.(c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')), spp:='Myoxocephalussp.']; setkey(ai, spp)
ai[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajainterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(ai, spp)


# =============
# = Aggregate =
# =============
setkey(ai, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
ai2 <- ai[j=lapply(list(wtcpue=wtcpue, cntcpue=NUMCPUE), FUN=sumna), by=key(ai)]



# Calculate a corrected longitude for Aleutians (all in western hemisphere coordinates)
ai2$lon[ai2$lon>0] = ai2$lon[ai2$lon>0] - 360

# ==============
# = Add region =
# ==============
ai2[,region:="AFSC_Aleutians"]
ai2[,s.reg:="ai"]


# ========
# = Save =
# ========
save(ai2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/ai2.RData")

