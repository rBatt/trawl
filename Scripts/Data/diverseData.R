

# library(maps)
library(data.table)
library(vegan)
library(reshape2)


load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")


# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))



# ================================
# = Combine 2 West Coast surveys =
# ================================
trawl[s.reg=="wcann",s.reg:="wc"]
trawl[s.reg=="wctri",s.reg:="wc"]

# ==================
# = Begin trimming =
# ==================
# Trim to taxa identified to species level
trawl1 <- trawl[taxLvl%in%c("Species")]

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

setkey(trawl1, s.reg, spp, common, year, stratum)
trawl2 <- trawl1[,list(lat=wtAvg(as.numeric(lat), wtcpue), lon=wtAvg(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)] # this is important to remember: the lat and lon temperature and depth are all wtcpue-weighted averages.

good.strat.id <- c()
n.year.strat <- trawl2[, # subsetting to strata that are observed in max(x) years, where x is the number of years each strata was observed. So if some strata were observed for a minimum of 12 years, and for a maximum of 30 years, only use strata observed for 30 years (even if only 1 was observed in 30 years, but 20 others were observed in 29 years, e.g.). So this is a pretty blunt and harsh tool.
	{
	nys <- rowSums(table(stratum, year)>1)
	nys.strat <- names(nys)
	nys.n <- as.numeric(nys)
	good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n==max(nys.n)]))
	},
	by=c("s.reg")
]
trawl2.1 <- trawl2[paste(s.reg,stratum)%in%good.strat.id,] # there were 35,259 strata that weren't there every year




# =========================================
# = Pad to regular ts for spp in a strata =
# =========================================
# Pad so that all unique spp in a stratum have a row each year for that stratum
# Added rows are 0's for wtcpue (0 wt per effort)
# Does not include adding NA's for missing years (i.e., if no sampling occurred that year)
allSpp <- trawl2.1[,CJ(spp=unique(spp), year=unique(year)), by=c("s.reg","stratum")]
setkey(allSpp)




trawl3 <- merge(allSpp, trawl2.1, all=TRUE)
trawl3[is.na(wtcpue), wtcpue:=0]
trawl3[, c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg","stratum")]
trawl3[, c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]


divData <- trawl3


save(divData, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/divData.RData")