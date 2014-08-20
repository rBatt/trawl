library(data.table)
library(bit64)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")


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








