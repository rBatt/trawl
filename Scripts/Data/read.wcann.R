

library(data.table)
library(bit64)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

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


wcann.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/NWFSC/2014-02-11/"

wcann.fish <- fread(paste(wcann.start,"wcann2003_2012fish.csv", sep=""))
wcann.haul <- fread(paste(wcann.start,"wcann2003_2012haul.csv", sep=""))
wcann.invert <- fread(paste(wcann.start,"wcann2003_2012invert.csv", sep=""))

# Change names of the data.tables to make.names()
setnames(wcann.fish, names(wcann.fish), make.names(names(wcann.fish)))
setnames(wcann.haul, names(wcann.haul), make.names(names(wcann.haul)))
setnames(wcann.invert, names(wcann.invert), make.names(names(wcann.invert)))

# Remove leading and trailing whitespace
rmWhite(wcann.fish)
rmWhite(wcann.haul)
rmWhite(wcann.invert)

# merge fish and inverts
wcann.invert[,Individual.Average.Weight..kg.:=as.numeric(NA)]
wcann.catch <- rbind(wcann.fish[, names(wcann.invert), with=FALSE], wcann.invert)

# make Trawl.Id a character for catch and haul
wcann.catch[,Trawl.Id:=as.character(Trawl.Id)]
wcann.haul[,Trawl.Id:=as.character(Trawl.Id)]

# merge catch and haul by Trawl.Id
wcann <- merge(wcann.catch, wcann.haul, by=intersect(names(wcann.catch), names(wcann.haul)), all.x=TRUE)

# ===============
# = Add haul id =
# ===============
wcann[,haulid:=Trawl.Id]


# ================
# = Extract year =
# ================
wcann[,year:=as.numeric(gsub('Cycle ', '', Survey.Cycle))]


# ================
# = Name columns =
# ================
setnames(wcann, c("Best.Latitude..dd.", "Best.Longitude..dd.", "Best.Depth..m.", "Species", "Temperature.At.the.Gear..degs.C.", "Trawl.Start.Time"), c("lat", "lon", "depth", "spp", "btemp", "datetime"))


# =====================
# = Add & Trim Strata =
# =====================
nyears <- wcann[,length(unique(year))]


wcann[,strat2:=ll2strat(lon, lat)]
# wcann[,sum(colSums(table(year, strat2)>0)==nyears)] # 1º grid gives you 31 strata seen every year

# wcann[,strat2:=ll2strat(lon, lat, 0.5)]
# wcann[,sum(colSums(table(year, strat2)>0)==nyears)] # 0.5º grid gives you 67 strata seen every year
#
# wcann[,strat2:=ll2strat(lon, lat, 0.25)]
# wcann[,sum(colSums(table(year, strat2)>0)==nyears)] # 0.25º grid gives you 96 strata seen every year

goodStrat2 <- wcann[,names(colSums(table(year, strat2)>0))[colSums(table(year, strat2)>0)==nyears]]
wcann <- wcann[strat2%in%goodStrat2]
wcann[,stratum:=strat2]
wcann[,strat2:=NULL]




# ==============
# = Add strata =
# ==============
# wcann[,stratum:=paste(floor(Best.Latitude..dd.)+0.5, floor(Best.Depth..m./100)*100+50, sep="-")]


# ================================
# = Trim Strata (malin line 167) =
# ================================
# wcann <- wcann[wcann$stratum %in% c("36.5-50", "37.5-150", "37.5-50", "38.5-150", "38.5-250", "38.5-350", "38.5-50", "39.5-150", "39.5-50", "40.5-150", "40.5-250", "41.5-150", "41.5-250", "41.5-50", "42.5-150", "42.5-250", "42.5-50", "43.5-150", "43.5-250", "43.5-350", "43.5-50", "44.5-150", "44.5-250", "44.5-350", "44.5-50", "45.5-150", "45.5-350", "45.5-50", "46.5-150", "46.5-250", "46.5-50", "47.5-150", "47.5-50", "48.5-150", "48.5-250", "48.5-50"),] # trim wcann to same footprint as wctri



# ================
# = Stratum area =
# ================
wcann[,stratumarea:=calcarea(cbind(lon, lat)), by=stratum]




# ============
# = Add cpue =
# ============
wcann[,wtcpue:=Haul.Weight..kg./Area.Swept.by.the.Net..hectares.]
wcann[,cntcpue:=(Haul.Weight..kg./Individual.Average.Weight..kg.)/Area.Swept.by.the.Net..hectares.]



# ==================
# = Remove bad spp =
# ==================
setkey(wcann, spp)
wcann.spp.bad <- c("","Apristurus brunneus egg case", "gastropod eggs", "Selachimorpha egg case")
wcann <- wcann[!.(wcann.spp.bad)]


# ===========================================================
# = Adj spp names when theyve changed or if matching failed =
# ===========================================================
i <- sapply(wcann, is.factor)
if(any(i)){
	wcann[i] <- lapply(wcann[i], as.character)
}


wcann[.(c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')), spp:='Lepidopsettasp.']; setkey(wcann, spp)
wcann[.(c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajwcannnterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')), spp:='Bathyrajasp.']; setkey(wcann, spp)



# =============
# = Aggregate =
# =============
wcann[,stemp:=as.numeric(NA)]
# setkey(wcann, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth, btemp, stemp)
# wcann2 <- wcann[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=sumna), by=key(wcann)]
# wcann2 <- wcann[j=lapply(list(wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(wcann)] # I think cpue should be avgd

setkey(wcann, year, datetime, spp, haulid, stratum, stratumarea, lat, lon, depth)
wcann2 <- wcann[j=lapply(list(stemp=stemp, btemp=btemp, wtcpue=wtcpue, cntcpue=cntcpue), FUN=meanna), by=key(wcann)]


# ==============
# = Add region =
# ==============
wcann2[,region:="NWFSC_WCAnn"]
wcann2[,s.reg:="wcann"]


# ========
# = Save =
# ========
save(wcann2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/wcann2.RData")

