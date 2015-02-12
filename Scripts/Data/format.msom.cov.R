
# ==================
# = Load libraries =
# ==================
library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)


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


# =======================
# = Load trawl2 Dataset =
# =======================
load("./trawl/Data/trawl2.RData")


# ======================
# = Remove unknown spp =
# ======================
trawl2.veri <- trawl2[(correctSpp)&!is.na(common)&is.species(spp),]





# ===========================
# = Test Covariate Building =
# ===========================
# trawl.test <- trawl2.veri[s.reg%in%c("gmex","ai","neus") & year%in%c("1983","1986")]
#
# comD = copy(trawl.test)
# arr.dim = c("stratum", "K", "spp") # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
# fillID=c("spp","K")
# fillValue=c(0,NA) # values to fill with, for a fillID
# Rule=c("value","scope") # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# keyID=c("s.reg", "stratum","year","spp", "K") # column names whose values uniquely identify rows in the input
# keyValue="wtcpue" # the column whose values would fill the array
# gScope="s.reg" # global scope
# fScope=list("s.reg", c("s.reg","year"))
# vScope=list(c("s.reg","stratum", "K", "year"), NULL)
# redID=list(c("s.reg","year","stratum","K","spp"))
# redValue=list(c("btemp","depth"))
# arrayOut=TRUE
# aggFun=meanna
# maxOut=Inf
#
#
# setkey(trawl.test, s.reg, year, stratum, K, spp)
# cov.btemp.dat.test <- expand.data(
# 	comD = copy(trawl.test),
# 	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
# 	fillID=c("spp","K"),
# 	fillValue=c(0,NA), # values to fill with, for a fillID
# 	Rule=c("value","scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
# 	keyValue="wtcpue", #
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg", c("s.reg","year")), #
# 	vScope=list(c("s.reg","stratum","K","year"), NULL),
# 	redID=list(c("s.reg","year","stratum","K","spp")),
# 	redValue=list(c("btemp","depth")),
# 	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
# )
#







# ==========================================
# = Prepare Temperature Covariate for MSOM =
# ==========================================
setkey(trawl2.veri, s.reg, year, stratum, K, spp)
cov.dat0 <- expand.data(
	comD = copy(trawl2.veri),
	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	fillID=c("spp","K"),
	fillValue=c(0,NA), # values to fill with, for a fillID
	Rule=c("value","scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	keyID=c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
	keyValue="wtcpue", #
	gScope="s.reg", # global scope
	fScope=list("s.reg", c("s.reg","year")), #
	vScope=list(c("s.reg","stratum","K","year"), NULL),
	redID=list(c("s.reg","year","stratum","K","spp")), 
	redValue=list(c("btemp","depth")),
	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
)


fill.cov <- function(x, FUN){ # fill covariate means
	# Environmental variables like depth and temperature have species-specific values in the data set
	# E.g., temperature in a region-stratum-substratum-year for a particular species is just the average of temperatures where that species was caught
	# because there are multiple hauls (potentially) per substratum, and not all species are caught, this value can vary
	# however, we can assume that the variability among hauls is small relative to the variability among strata or years
	# If a species isn't caught at in a year/substratum, the temperature associated with that absence is the average of the temperatures recorded for the other species caught in that same time/ place.
	bother <- apply(x, c(1,2), function(x)any(is.na(x))&any(!is.na(x)))
	if(any(bother)){
		bother2 <- which(bother, arr.ind=TRUE)
		fm <- apply(x, c(1,2), FUN, na.rm=TRUE)
				
		for(i in 1:nrow(bother2)){
			t.bother <- bother2[i,]
			x.na <- is.na(x[t.bother[1], t.bother[2], ])
			x[t.bother[1], t.bother[2], x.na] <- fm[t.bother[1],t.bother[2]]
		}
	}
	return(x)
}


prec <- function(x, ...){
	if(sum(!is.na(x))==1){
		return(1/(max(x,...)^2))
	}else{
		return( min(1E4, 1/var(x, ...)) )
	}
}
fill.cov.prec <- function(x){ # fill covariate means
	# Environmental variables like depth and temperature have species-specific values in the data set
	# E.g., temperature in a region-stratum-substratum-year for a particular species is just the average of temperatures where that species was caught
	# because there are multiple hauls (potentially) per substratum, and not all species are caught, this value can vary
	# however, we can assume that the variability among hauls is small relative to the variability among strata or years
	# If a species isn't caught at in a year/substratum, the temperature associated with that absence is the average of the temperatures recorded for the other species caught in that same time/ place.
	bother <- apply(x, c(1,2), function(x)any(is.na(x))&any(!is.na(x)))
	fill.but.no.sd <- apply(x, c(1,2), function(x)any(is.na(x))&(sum(!is.na(x))==1))
	# if(any(fill.but.no.sd)){warning("only 1 non-NA; values would be filled with mean, but not stdev")}
	if(any(bother)){
		bother2 <- which(bother, arr.ind=TRUE)
		fm <- apply(x, c(1,2), prec, na.rm=TRUE)
				
		for(i in 1:nrow(bother2)){
			t.bother <- bother2[i,]
			x.na <- is.na(x[t.bother[1], t.bother[2], ])
			x[t.bother[1], t.bother[2], x.na] <- fm[t.bother[1],t.bother[2]]
			x[t.bother[1], t.bother[2], !x.na] <- 1E4
		}
	}
	return(x)
}



cov.dat <- cov.dat0
for(i in 1:length(cov.dat[[3]])){
	cov.dat[[3]][[i]] <- lapply(cov.dat[[3]][[i]], fill.cov, mean)
}

cov.dat.prec <- cov.dat0[[3]]
for(i in 1:length(cov.dat.prec)){
	cov.dat.prec[[i]] <- lapply(cov.dat.prec[[i]], fill.cov.prec)
}

# DIMS tests (does it make sense)
# min(cov.dat.prec[[2]][[275]], na.rm=TRUE)
# sqrt(1/min(cov.dat.prec[[2]][[275]], na.rm=TRUE))
# max(cov.dat.prec[[2]][[3]], na.rm=TRUE)

# ========
# = Save =
# ========
save(cov.dat, file="./trawl/Data/MSOM/cov.dat.RData", compress="xz")
save(cov.dat.prec, file="./trawl/Data/MSOM/cov.dat.prec.RData", compress="xz")


