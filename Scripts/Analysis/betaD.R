
library(data.table)
library(vegan)
library(reshape2)


load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")


# ================================
# = Combine 2 West Coast surveys =
# ================================
trawl[s.reg=="wcann",s.reg:="wc"]
trawl[s.reg=="wctri",s.reg:="wc"]

# ==================
# = Begin trimming =
# ==================
# Trim to Genus or Spp
trawl1 <- trawl[taxLvl%in%c("Genus","Species")]

# Drop rows w/ no wtcpue
trawl1 <- trawl1[is.finite(wtcpue)&wtcpue>0,]

# Add # years observed
lu <- function(x) length(unique(x))
trawl1[,n.yrs:=lu(year), by=c("spp","s.reg")]

# Add column for total weight of a species in a stratum in a year (aggregate across multiple hauls)
trawl1[,sumWtStrat:=sum(wtcpue), by=c("spp","year","stratum","s.reg")]



# =====================================
# = Aggregate within stratum-year-spp =
# =====================================
# Avg by wtcpue
wtAvg <- function(x,y){
	# x is something like temperature (the value to be averaged)
	# y is something like wtcpue (the value to be used for weighting)
	totW <- sum(y[is.finite(x)])
	propW <- y/totW
	sumna(x*propW)
}

setkey(trawl1, s.reg, spp, common, year, stratum)
trawl2 <- trawl1[,list(lat=wtAvg(as.numeric(lat), wtcpue), lon=wtAvg(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)]


# =========================================
# = Pad to regular ts for spp in a strata =
# =========================================
# Pad so that all unique spp in a stratum have a row each year for that stratum
# Added rows are 0's for wtcpue (0 wt per effort)
# Does not include adding NA's for missing years (i.e., if no sampling occurred that year)
allSpp <- trawl2[,CJ(spp=unique(spp), year=unique(year)), by=c("s.reg","stratum")]
setkey(allSpp)


# Fill the NA values of a vector with the mean of the non-NA portion
fill.mean <- function(x){
	nai <- is.na(x)
	x[nai] <- meanna(x)
	x
}

trawl3 <- merge(allSpp, trawl2, all=TRUE)
trawl3[is.na(wtcpue), wtcpue:=0]
trawl3[, c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg","stratum")]
trawl3[, c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]


# =======================
# = Free up some memory =
# =======================
rm(list=c("trawl","trawl1"))




# ====================
# = Calculate a Beta =
# ====================
# trawl3[s.reg=="neus", sum(wtcpue>0), by=c("stratum","year")][V1==max(V1), list(stratum, year, V1)]
test <- trawl3[s.reg=="neus"&stratum=="1100"]
test2 <- acast(melt(test, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
test3 <- vegdist(test2, method="jaccard")



castExp <- bquote(acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1])
d.jac <- bquote(vegdist(cmat, method="jaccard"))

trawl3[,,by=c("s.reg","stratum")]






