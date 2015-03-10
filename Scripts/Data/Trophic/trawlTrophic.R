


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))



# =================
# = Load Packages =
# =================
library(rfishbase)
library(data.table)


# ======================
# = Load FishBase Data =
# ======================
data(fishbase)


# ============================
# = Load Trawl Taxonomy Data =
# ============================
load("./trawl/Data/Taxonomy/taxInfo.RData")


# =====================
# = Get Trophic Level =
# =====================
TLs <- getTL(spp=taxInfo[,spp])


setkey(taxInfo, spp)
setkey(TLs, spp)


foundTLs <- copy(taxInfo)
foundTLs[,c("trophicLevel","trophicLevel.se"):=TLs[,list(trophicLevel,trophicLevel.se)]]

setcolorder(foundTLs, c("spp", "common", "trophicLevel", "trophicLevel.se", "phylum", "taxLvl", "species", "genus", "family", "order", "class", "superclass", "subphylum", "kingdom", "raw.spp", "isSpecies", "correctSpp"))


save(foundTLs, file="./trawl/Data/Taxonomy/foundTLs.RData")

write.csv(foundTLs, file="~/Desktop/foundTLS.csv", row.names=FALSE)


# ===================================
# = Combine this w/ rachel's manual =
# ===================================
# read rachel's trophic information
racheltax <- read.csv("/Users/Battrd/Downloads/racheltax.csv")
racheltax <- data.table(racheltax, key=c("spp","raw.spp","common"))

# just to make the sorting consistent between rachel and computer
foundTLs <- data.table(foundTLs, key=c("spp","raw.spp","common"))

# check to make sure these data frames match up in rows
stopifnot(nrow(racheltax)==nrow(foundTLs))

# convert rachel's trophic information to numeric
racheltax[,c("trophicLevel","trophicLevel.se"):=list(as.numeric(as.character(trophicLevel)), as.numeric(as.character(trophicLevel.se)))]

# indicies of the automated trohpic information
addTL <- foundTLs[,!is.na(trophicLevel)]
addTL.se <- foundTLs[,!is.na(trophicLevel.se)]

# indices of rachel's trophic information
addTL.r <- racheltax[,!is.na(trophicLevel)]
addTL.se.r <- racheltax[,!is.na(trophicLevel.se)]

# Grab the trophic level information to be added
TL2add <- foundTLs[addTL&!addTL.r, trophicLevel]
TLse2add <- foundTLs[addTL.se&!addTL.se.r, trophicLevel.se]

# Add the automated trohpic levels to rachel's spreadsheet
racheltax[addTL&!addTL.r, trophicLevel:=TL2add]
racheltax[addTL.se&!addTL.se.r, trophicLevel.se:=TLse2add]

# write the result
write.csv(racheltax, file="/Users/Battrd/Desktop/rachtax_plus.csv", row.names=FALSE)



