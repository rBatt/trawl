

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
# TODO don't need to use the "Obsd" column anymore; I have already distinguished between NA's and 0's.
trawl.new <- trawl[(s.reg!="wctri" | (s.reg=="wctri"&year<2003)) & taxLvl%in%c("Species") & Obsd==TRUE,] # need to prevent overlap of the 2 WC
spp.both.wc <- trawl.new[s.reg%in%c("wctri","wcann"),
	j={
		ann.spp <- .SD[s.reg=="wcann", unique(spp)]
		tri.spp <- .SD[s.reg=="wctri", unique(spp)]
		# wc.spp.both <- intersect(ann.spp, tri.spp)
		wc.spp.both <- intersect(ann.spp, tri.spp)
	}	
] # only use species present in both of the WC
trawl.new[s.reg=="wcann",s.reg:="wc"]
trawl.new[s.reg=="wctri",s.reg:="wc"]
trawl.new <- trawl.new[s.reg!="wc"|(s.reg=="wc" & spp%in%spp.both.wc)]

setkey(trawl.new, s.reg, spp, year, stratum)


# ==================
# = Begin trimming =
# ==================
# Trim to taxa identified to species level
# trawl1d <- trawl.new[taxLvl%in%c("Species")] # the "d" just stands for the name of the diverseData script; needed to distinguish when testing so as to not overwrite w/ combine.trawl
setkey(trawl.new, s.reg, spp, year, stratum) # this line is producing duplicatesin trawl1d

# Drop rows w/ no wtcpue
# trawl1 <- trawl1[is.finite(wtcpue)&wtcpue>0,]

# Add # years observed
lu <- function(x) length(unique(x))
trawl.new[,n.yrs:=lu(year), by=c("spp","s.reg")]

# Add column for total weight of a species in a stratum in a year (aggregate across multiple hauls)
# trawl.new[,sumWtStrat:=sum(wtcpue), by=c("spp","year","stratum","s.reg")]



# =====================================
# = Aggregate within stratum-year-spp =
# =====================================

setkey(trawl.new, s.reg, spp, year, stratum)
# trawl2 <- trawl1[,list(lat=wtAvg(as.numeric(lat), wtcpue), lon=wtAvg(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)] # this is important to remember: the lat and lon temperature and depth are all wtcpue-weighted averages.
# trawl2 <- trawl1[,list(lat=roundGrid(as.numeric(lat), wtcpue), lon=roundGrid(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)] # this is important to remember: the lat and lon temperature and depth are all wtcpue-weighted averages.


# TODO this whole next part can probably be deleted now. I think I've standardized the NA's and 0's and tolerance for missing strata pretty thoroughly in the read.xx and combine.trawl files. These changes came during the keepHaulsSep branch.
good.strat.id <- c()
n.year.strat <- trawl.new[, # subsetting to strata that are observed in max(x) years, where x is the number of years each strata was observed. So if some strata were observed for a minimum of 12 years, and for a maximum of 30 years, only use strata observed for 30 years (even if only 1 was observed in 30 years, but 20 others were observed in 29 years, e.g.). So this is a pretty blunt and harsh tool.
# TODO should read in the tolerance values instead of these manual implementations
	{
	nys <- rowSums(table(stratum[Obsd], year[Obsd])>1)
	nys.strat <- names(nys)
	nys.n <- as.numeric(nys)
	if(s.reg=="sgulf"){
		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-1)]))
	}else if(s.reg=="shelf"){
		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-6)]))
	}else if(s.reg=="neus"){
		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-3)]))
	}else if(s.reg=="newf"){
		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-1)]))
	}else if(s.reg=="goa"){
		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-2)]))
	}else{
		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n==max(nys.n)]))
	}
	
	},
	by=c("s.reg")
]
trawl2 <- trawl.new[paste(s.reg,stratum)%in%good.strat.id,]


# trawl2 <- trawl1


divData <- trawl2
divData[,c("lon","lat"):=list(roundGrid(lon),roundGrid(lat))]


save(divData, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/divData.RData")
