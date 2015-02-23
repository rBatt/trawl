

# library(maps)
library(data.table)
library(vegan)
library(reshape2)



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
divData <- trawl2[(correctSpp)&!is.na(common)&is.species(spp),]
divData <- divData[s.reg!="wcann" | (s.reg=="wcann" & year > 2003)]
divData[s.reg=="wcann" | s.reg=="wctri", s.reg:="wc"]


setkey(divData, s.reg, spp, year, stratum)


# Add # years observed
# trawl.new[,n.yrs:=lu(year), by=c("spp","s.reg")]

# Add column for total weight of a species in a stratum in a year (aggregate across multiple hauls)
# trawl.new[,sumWtStrat:=sum(wtcpue), by=c("spp","year","stratum","s.reg")]



# =====================================
# = Aggregate within stratum-year-spp =
# =====================================

# setkey(trawl.new, s.reg, spp, year, stratum)
# trawl2 <- trawl1[,list(lat=wtAvg(as.numeric(lat), wtcpue), lon=wtAvg(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)] # this is important to remember: the lat and lon temperature and depth are all wtcpue-weighted averages.
# trawl2 <- trawl1[,list(lat=roundGrid(as.numeric(lat), wtcpue), lon=roundGrid(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)] # this is important to remember: the lat and lon temperature and depth are all wtcpue-weighted averages.


# TODO this whole next part can probably be deleted now. I think I've standardized the NA's and 0's and tolerance for missing strata pretty thoroughly in the read.xx and combine.trawl files. These changes came during the keepHaulsSep branch.
# good.strat.id <- c()
# n.year.strat <- trawl.new[, # subsetting to strata that are observed in max(x) years, where x is the number of years each strata was observed. So if some strata were observed for a minimum of 12 years, and for a maximum of 30 years, only use strata observed for 30 years (even if only 1 was observed in 30 years, but 20 others were observed in 29 years, e.g.). So this is a pretty blunt and harsh tool.
# # TODO should read in the tolerance values instead of these manual implementations
# 	{
# 	nys <- rowSums(table(stratum[Obsd], year[Obsd])>1)
# 	nys.strat <- names(nys)
# 	nys.n <- as.numeric(nys)
# 	if(s.reg=="sgulf"){
# 		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-1)]))
# 	}else if(s.reg=="shelf"){
# 		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-6)]))
# 	}else if(s.reg=="neus"){
# 		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-3)]))
# 	}else if(s.reg=="newf"){
# 		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-1)]))
# 	}else if(s.reg=="goa"){
# 		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n>=(max(nys.n)-2)]))
# 	}else{
# 		good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n==max(nys.n)]))
# 	}
#
# 	},
# 	by=c("s.reg")
# ]
# trawl2 <- trawl.new[paste(s.reg,stratum)%in%good.strat.id,]


# trawl2 <- trawl1


# divData <- trawl2
divData[,c("lon","lat"):=list(roundGrid(lon),roundGrid(lat))]

divData <- divData[,list(lon=meanna(lon),lat=meanna(lat),depth=meanna(depth), stemp=meanna(stemp), btemp=meanna(btemp), wtcpue=meanna(wtcpue)), by=c("region","s.reg","year","stratum","spp","common","taxLvl","phylum")]

setkey(divData, s.reg, spp, year, stratum)
save(divData, file="./trawl/Data/divData.RData")
