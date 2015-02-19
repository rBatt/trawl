


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# =================
# = Load Packages =
# =================
library(devtools)
library(rfishbase)


# ======================
# = Load FishBase Data =
# ======================
data(fishbase)


# ============================
# = Load Trawl Taxonomy Data =
# ============================
load("./trawl/Data/Taxonomy/taxInfo.RData")


# ====================
# = Subset fish.data =
# ====================
fb.names <- fish_names(fish.data)

findable <- fb.names %in% taxInfo[,spp]

fd.sub <- list()
for(i in 1:sum(findable)){fd.sub[[i]] <- fish.data[[i]]}


# ================================
# = Get Trophic Level for Subset =
# ================================
tl.sub <- getTrophicLevel(fd.sub)

