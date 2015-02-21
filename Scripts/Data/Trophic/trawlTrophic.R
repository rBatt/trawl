


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