

# ==================
# = Load libraries =
# ==================
library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)


# =======================
# = Load data functions =
# =======================
# data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
# invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load data functions =
# =======================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load trawl2 Dataset =
# =======================
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl2.RData")


# ============================
# = Add missing combinations =
# ============================
# Make sure all of a region's strata are present in all years
# Make sure all of a region's species are present in each stratum in each year
# If the s.reg-stratum-year was present, but the s.reg-stratum-year-spp was not, fill in w/ 0, NA otherwise
# Aggregate among "K" replicate hauls (substrata)
setkey(trawl2, s.reg, year, stratum, K, spp)
trawl <- expand.data(
	comD = copy(trawl2),
	arr.dim = c("stratum", "year", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	fillID=c("spp"),
	fillValue=c(0), # values to fill with, for a fillID
	Rule=c("value"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
	keyValue="wtcpue", # the column whose values would fill the array
	gScope="s.reg", # global scope
	fScope=list("s.reg"), #
	vScope=list(c("s.reg","stratum","year")),
	redID=list(
		c("spp"),
		c("s.reg","year","stratum","spp")
	), 
	redValue=list(
		c("correctSpp","taxLvl","phylum","common"),
		c("lat", "lon", "depth", "stemp", "btemp")
	),
	arrayOut=FALSE, aggFun=meanna, maxOut=Inf
)


# ========
# = Save =
# ========
setkey(trawl, s.reg, spp, year, stratum)
# save(trawl, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
save(trawl, file="./trawl/Data/trawl.RData")
rm(list="trawl")

	#
	#
	#
	# comD = trawl2
	# arr.dim = c("stratum", "year", "spp") # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	# fillID=c("spp")
	# fillValue=c(0) # values to fill with, for a fillID
	# Rule=c("value") # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	# keyID=c("s.reg", "stratum","year","spp", "K") # column names whose values uniquely identify rows in the input
	# keyValue="wtcpue" # the column whose values would fill the array
	# gScope="s.reg" # global scope
	# fScope=list("s.reg") #
	# vScope=list(c("s.reg","stratum","year"))
	# redID=list(
	# 	c("spp"),
	# 	c("s.reg","year","stratum","spp")
	# )
	# redValue=list(
	# 	c("correctSpp","taxLvl","phylum","common"),
	# 	c("lat", "lon", "depth", "stemp", "btemp")
	# )
	# arrayOut=FALSE
	# aggFun=meanna
	# maxOut=Inf



# ==================================
# = Example of how to prepare MSOM =
# ==================================
setkey(trawl2, s.reg, year, stratum, K, spp)
msom.dat <- expand.data(
	comD = copy(trawl2),
	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	fillID=c("spp","K"),
	fillValue=c(0,NA), # values to fill with, for a fillID
	Rule=c("value","scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
	keyValue="wtcpue", # the column whose values would fill the array
	gScope="s.reg", # global scope
	fScope=list("s.reg", c("s.reg","year")), #
	vScope=list(c("s.reg","stratum","year"), NULL),
	redID=NULL, 
	redValue=NULL,
	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
)

# ========
# = Save =
# ========
# save(msom.dat, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/msom.dat.RData")
save(msom.dat, file="./trawl/Data/msom.dat.RData")

#
# trawl.test <- trawl2[s.reg%in%c("gmex","ai","neus") & year%in%c("1983","1986")]
# comD = copy(trawl.test)
# arr.dim = c("stratum", "K", "spp") # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
# fillID=c("spp","K")
# fillValue=c(0,NA) # values to fill with, for a fillID
# Rule=c("value","scope") # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# keyID=c("s.reg", "stratum","year","spp", "K") # column names whose values uniquely identify rows in the input
# keyValue="wtcpue" # the column whose values would fill the array
# gScope="s.reg" # global scope
# fScope=list("s.reg", c("s.reg","year"))
# vScope=list(c("s.reg","stratum","year"), NULL)
# redID=NULL
# redValue=NULL
# arrayOut=TRUE
# aggFun=meanna
# maxOut=Inf
#