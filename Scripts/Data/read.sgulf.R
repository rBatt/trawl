

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


# ===============
# = Trim Strata =
# ===============
# sgulf.raw00 <- sgulf.raw00[!(sgulf.raw00$year %in% c(1971, 1972)),] # doesn't really help much
nyears <- sgulf.raw00[,length(unique(year))]

# sgulf.raw00[,sum(colSums(table(year, stratum)>0)>=(nyears-1))] # original strata gives 12 strata seen every year
# image(x=sgulf.raw00[,sort(unique(year))], y=sgulf.raw00[,1:length(unique(stratum))], z=sgulf.raw00[,table(year, stratum)>0])

sgulf.raw00[,strat2:=paste(stratum, ll2strat(lon, lat))]
# sgulf.raw00[,sum(colSums(table(year, strat2)>0)>=(nyears-1))] # 1º grid gives you 5 strata seen every year
# >=(nyears-1) gives 16 strata on 1º grid
# >=(nyears-2) gives 19 strata on 1º grid
# >=(nyears-3) gives 22 strata on 1º grid
# >=(nyears-4) gives 23 strata on 1º grid

# nstrata <- c()
# nstrata.orig <- c()
# for(i in 0:38){
# 	nstrata[i+1] <- sgulf.raw00[,sum(colSums(table(year, strat2)>0)>=(nyears-i))]
# 	nstrata.orig[i+1] <- sgulf.raw00[,sum(colSums(table(year, stratum)>0)>=(nyears-i))]
# }
# dev.new(width=4)
# par(mfrow=c(2,1), mar=c(2.5,2,1.5,0.2), cex=1, ps=10, mgp=c(1.25, 0.15, 0), tcl=-0.25)
# plot(0:38, nstrata, type="o", xlab="threshold # years missing", ylab="# strata below threshold missingness", main="# strata vs. tolerance of missingness")
# lines(0:38, nstrata.orig, type="o", col="red")
# legend("topleft", legend=c("original strata definition", "1 degree grid definition"), lty=1, pch=21, col=c("red","black"))
# image(x=sgulf.raw00[,sort(unique(year))], y=sgulf.raw00[,1:length(unique(strat2))], z=sgulf.raw00[,table(year, strat2)>0], xlab="year", ylab="1 degree stratum ID", main="stratum presence vs. time; red is present")

toleranceChoice <- 1


goodStrat2 <- sgulf.raw00[,names(colSums(table(year, strat2)>0))[colSums(table(year, strat2)>0)>=(nyears-toleranceChoice)]]
sgulf.raw0 <- sgulf.raw00[strat2%in%goodStrat2]
sgulf.raw0[,stratum:=strat2]
sgulf.raw0[,strat2:=NULL]


# ==================================
# = Subset to Strata sampled often =
# ==================================
# sgulf.raw00[,rowSums(table(stratum, year)>=1)] # most strata were sampled every year (42 years is mode and max)
# sgulf.raw00[,colSums(table(stratum, year)>=1)] # most years sampled between 48 and 51 strata

# sgulf.YS <- sgulf.raw00[,rowSums(table(stratum, year)>=1)] # the number of years (Y) in each stratum (S)
# sgulf.YS.pick <- names(sgulf.YS)[sgulf.YS==max(sgulf.YS)] # max() still works on class()=="character"
# sgulf.raw0 <- sgulf.raw00[stratum%in%sgulf.YS.pick,] # vector scan instead of binary search, but idc

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

