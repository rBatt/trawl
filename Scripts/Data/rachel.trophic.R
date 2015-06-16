


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


# ====================
# = Load Data trawl2 =
# ====================
load("./trawl/Data/trawl2.RData")


# ============================
# = Load Trophic Information =
# ============================
# load("./trawl/Data/Taxonomy/taxInfo.RData") # taxInfo3


# ============================================
# = Aggregate As Much As Possible for Rachel =
# ============================================

# Drop unconfirmed species ID's
tt.onlySpp <- trawl2[(correctSpp)&is.species(spp)]

# Aggregate over substrata
tt.noK <- tt.onlySpp[,j={
	list(
		lat=mean(lat, na.rm=TRUE),
		lon=mean(lon, na.rm=TRUE), 
		depth=meanna(depth),
		stemp=meanna(stemp),
		btemp=meanna(btemp),
		wtcpue=meanna(wtcpue),
		trophicLevel=mean(trophicLevel, na.rm=TRUE),
		trophicLevel.se=mean(trophicLevel.se, na.rm=TRUE)
	)
	
	
	},
	by=c("region", "s.reg", "year", "stratum", "spp", "common")
]


# Create Groups/ Bins of Trophic Levels
tt.noK[,unique(trophicLevel)]
tt.tl.breaks <- seq(2.0, 5.0, by=0.25) #tt.noK[,hist(unique(trophicLevel))$breaks]
tt.noK[,trophicGroup:=cut(trophicLevel, breaks=tt.tl.breaks)]
tt.noK[is.na(trophicLevel),trophicGroup:="unknown"]



# Aggregate over Species, but keep strata (local)
tt.local <- tt.noK[,j={
	list(
		lat=mean(lat, na.rm=TRUE),
		lon=mean(lon, na.rm=TRUE), 
		depth=meanna(depth),
		stemp=meanna(stemp),
		btemp=meanna(btemp),
		wtcpue.sum=sumna(wtcpue),
		wtcpue.mean=meanna(wtcpue),
		# trophicLevel=mean(trophicLevel, na.rm=TRUE), # TODO  this needs to be a weighted mean?! This is where I left off. Can probably just drop the wtcpue at this point and only provided the weighted mean of this value. Then for the spatially collapsed data set, I'd want to first aggregate over space, using the mean of wtcpue to aggregate, then I would want to do the weighted mean from there. So don't create the spatially aggregated data set from this data set!
		trophicLevel.se=mean(trophicLevel.se, na.rm=TRUE)
	)
	
	
	},
	by=c("region", "s.reg", "year", "stratum", "trophicGroup")
]

# Aggregate over strata using average, but keep species
tt.noK[,nStrat:=lu(stratum), by=c("s.reg","year")]

# For each species I took the sum of biomass caught for that species in that year, and divided it by the number of strata sampled in that region in that year, giving the "wtcpue" value as a mean.
tt.region.spp <- tt.noK[,j={
	list(
		lat=mean(lat, na.rm=TRUE),
		lon=mean(lon, na.rm=TRUE), 
		depth=meanna(depth),
		stemp=meanna(stemp),
		btemp=meanna(btemp),
		wtcpue=sumna(wtcpue)/as.numeric(unique(nStrat)),
		trophicLevel=mean(trophicLevel, na.rm=TRUE),
		trophicLevel.se=mean(trophicLevel.se, na.rm=TRUE)
	)
	
	
	},
	by=c("region", "s.reg", "year", "trophicGroup", "spp")
]

tt.region.spp[,nSpp.perGroup.perYear:=lu(spp), by=c("s.reg","year","trophicGroup")]
tt.region.spp[,nSpp.perGroup:=lu(spp), by=c("s.reg","trophicGroup")]
tt.region.spp[,trophicLevel.region.wmean:=weighted.mean(trophicLevel, w=wtcpue, na.rm=TRUE), by=c("s.reg","year")]


# Final aggregation: for each trophic group provide the total wtcpue (sum across species in that trophic group), the sum of the trophic level, and the biomass-weighted trophic level.
tt.region <- tt.region.spp[,j={
	list(
		stemp=meanna(stemp),
		btemp=meanna(btemp),
		wtcpue.sum=sumna(wtcpue),
		trophicLevel.region.wmean=unique(trophicLevel.region.wmean),
		trophicLevel.sum=sum(trophicLevel, na.rm=TRUE),
		trophicLevel.wmean=weighted.mean(trophicLevel, w=wtcpue, na.rm=TRUE),
		trophicLevel.se.squareSum=sum(trophicLevel.se^2, na.rm=TRUE),
		nSpp.perGroup.perYear=unique(nSpp.perGroup.perYear),
		nSpp.perGroup=unique(nSpp.perGroup)
	)
	
	
	},
	by=c("region", "s.reg", "year", "trophicGroup")
]


setkey(tt.region, s.reg, year, trophicGroup)


# ================================================================
# = Make each trophicGroup a regular time series w/in the region =
# ================================================================
tt.region2 <- tt.region[,j={
		regts <- expand.grid(year=.SD[,unique(year)], trophicGroup=.SD[,unique(trophicGroup)])
		# head(regts)
		out <- merge(regts, .SD, all=TRUE, by=c("year","trophicGroup"))
	},
	by=c("s.reg")


]

fill0s <- c("wtcpue.sum", "trophicLevel.region.wmean", "trophicLevel.sum", "trophicLevel.wmean", "trophicLevel.se.squareSum", "nSpp.perGroup.perYear", "nSpp.perGroup")

tt.region2[tt.region2[,is.na(region)], (fill0s):=0]

write.csv(tt.region2, "./trawl/Data/trophicTrawl/trophicTrawl.csv", row.names=FALSE)

# tt.region[,plot((trophicLevel.sum/nSpp.perGroup.perYear~trophicLevel.wmean))]
# abline(a=0, b=1)
# basically this shows me that I could have just taken a normal mean and not worried about the weights. The intervals for the trophic groups are small enough (1/4 a trophic level) that the wiggle room within that 0.25 interval looks pretty tiny overall. But if you want to dig into the finer details, this also shows the intuitive relationship between trophicLevel.sum, nSpp.perGroup.perYear, and trophicLevel.wmean.