
# ====================
# = Read in Raw Data =
# ====================
ai1.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/ai1983_2000.csv")
setnames(ai1.raw, names(ai1.raw), gsub("^\\s* | \\s*$", "", names(ai1.raw)))

ai2.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/ai2002_2012.csv")
setnames(ai2.raw, names(ai2.raw), gsub("^\\s* | \\s*$", "", names(ai2.raw)))

ai.raw <- rbind(ai1.raw, ai2.raw)
rmWhite(ai.raw) # remove whitespace in the elements of each column
rm9s(ai.raw) # check each column for 9999, and replace with NA

aiStrata.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_Aleutians/2013-10-17/aiStrata.csv")
setnames(aiStrata.raw, names(aiStrata.raw), gsub("^\\s* | \\s*$", "", names(aiStrata.raw)))


