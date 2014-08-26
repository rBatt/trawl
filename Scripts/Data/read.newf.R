
# =================
# = Load packages =
# =================
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

# ====================
# = Source Functions =
# ====================
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rm9s.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/calcarea.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")


# =====================
# = Set preliminaries =
# =====================
# String to begin directory
newf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/DFO_Newfoundland/"


# =====================================
# = Read in Tables (spp, strata, etc) =
# =====================================
# Read in Species
newf.spp <- fread(paste(newf.start, "Tables/", "GFSPCY.CODE_2012-07-05.csv", sep=""), drop="V1")


# =======================
# = Read in Survey Info =
# =======================
newf.surv1 <- fread(paste(newf.start, "Tables/", "surveys_table.csv", sep=""), colClasses=rep("character", 16), drop=c("Comment"))
newf.surv2 <- fread(paste(newf.start, "Tables/", "surveys_table2009-2011.csv", sep=""), colClasses=rep("character", 7))



# ======================
# = Read in Data Files =
# ======================
newf.files <- list.files(path=paste(newf.start, "Data/", sep=""), pattern = "199[23456789]|200[0123456789]|201[012]")

n <- numeric(0)
ch <- numeric(0)
newf.raw00 <- data.frame(recordtype = n, vessel = n, trip = n, set = n, yearl=n, monthl = n, dayl = n, settype = n, stratum = n, nafo = ch, unitarea = ch, light = n, winddir = n, windforce = n, sea = n, bottom = n, timel = n, duration = n, distance = n, operation = n, depth = n, depthmin = n, depthmax = n, depthbottom = n, surftemp = n, bottemp = n, latstart = n, lonstart = n, posmethod = n, gear = n, sppcode = n, num = n, wgt = n, latend = n, lonend = n, bottempmeth = n, geardevice = n)


for(i in 1:length(newf.files)){ # for each file
	if(i == 1) print(length(newf.files)) # 45 files
	print(i)
	indata = read.fwf(file=paste(newf.start, "Data/", newf.files, sep=''), widths=c(
	1, # record type
	2, # vessel
	3, # trip
	3, # set
	2, # year
	2, # mo
	2, # day
	2, # set type
	3, # stratum
	2, # nafo
	3, # unit
	3, # light
	1, # winddir
	1, # wind force
	1, # sea
	1, # bottom type
	4, # time
	3, # duration
	3, # distance 
	1, # operation
	4, # depth mean
	4, # depth min
	4, # depth max
	4, # depth bottom
	3, # temp surf
	3, # temp bot
	5, # lat start
	5, # lon start
	1, # pos meth
	4, # gear
	4, # sppcode
	6, # number
	7, # wgt 
	5, # lat end
	5, # lon end
	2, # bot temp device
	2), # gear mon device
	header= FALSE, stringsAsFactors = FALSE)

	names(indata) <- c('recordtype', 'vessel', 'trip', 'set', 'yearl', 'monthl', 'dayl', 'settype', 'stratum', 'nafo', 'unitarea', 'light', 'winddir', 'windforce', 'sea', 'bottom', 'timel', 'duration', 'distance', 'operation', 'depth', 'depthmin', 'depthmax', 'depthbottom', 'surftemp', 'bottemp', 'latstart', 'lonstart', 'posmethod', 'gear', 'sppcode', 'num', 'wgt', 'latend', 'lonend', 'bottempmeth', 'geardevice')

	newf.raw00 <- rbind(newf.raw00, indata)
	print(dim(newf.raw00))
}
newf.raw0 <- newf.raw00
# ========================
# = Subset data based on =
# ========================
ss1 <- newf.raw0$operation %in% c(1,2) & newf.raw0$recordtype == 6 # 6 is biological data, 5 is set information
newf.raw0 <- newf.raw0[ss1,]

ss2 <- newf.raw0$gear == 61 & !is.na(newf.raw0$gear) # CAMPELEN 1800 SHRIMP TRAWL--LINED
newf.raw0 <- newf.raw0[ss2,]

ss3 <- newf.raw0$settype == 1
newf.raw0 <- newf.raw0[ss3,]

# =========================
# = Convert to data.table =
# =========================
newf.raw <- data.table(newf.raw0)

# ========================
# = Create unique haulid =
# ========================
newf.raw[,haulid:=paste(formatC(vessel, width=2, flag=0), formatC(trip, width=3, flag=0), formatC(set, width=3, flag=0, format='d'), sep='-')]

# =============
# = Fix years =
# =============
newf.raw[,yearl:=yearl+1900]
newf.raw[newf.raw$yearl<1950, yearl:=yearl+100]
# setkey(newf.raw, yearl)

# ============
# = Add date =
# ============
newf.raw[,datetime:=as.POSIXct(paste(yearl, monthl, dayl, sep="-"), tz="Canada/Newfoundland")]
# newf.raw[,datetime:=format.Date(datetime, tz="GMT")]
newf.raw[,julian:=as.integer(format.Date(datetime, format="%j"))]


# ==================
# = Format lat/lon =
# ==================
# first, fix latitude
lat.1 <- newf.raw[,(latstart>0&latend>0)]
lat.2 <- newf.raw[,(latstart>0&latend==0)]
# lat.3 <- newf.raw[,(latstart==0&latend>0)] # no instances of this case

newf.raw[lat.1,lat:=(as.numeric(substr(latstart, 1, 2)) + as.numeric(substr(latstart, 3, 5))/600 + as.numeric(substr(latend, 1, 2)) + as.numeric(substr(latend, 3, 5))/600)/2]

newf.raw[lat.2, lat:=as.numeric(substr(latstart, 1, 2)) + as.numeric(substr(latstart, 3, 5))/600]

# fix longitude
lon.1 <- newf.raw[,(lonstart>0&lonend>0)]
lon.2 <- newf.raw[,(lonstart>0&lonend==0)]

newf.raw[lon.1, lon:=-(as.numeric(substr(lonstart, 1, 2)) + as.numeric(substr(lonstart, 3, 5))/600 + as.numeric(substr(lonend, 1, 2)) + as.numeric(substr(lonend, 3, 5))/600)/2]

newf.raw[lon2, lon:=-(as.numeric(substr(lonstart, 1, 2)) + as.numeric(substr(lonstart, 3, 5))/600)]

# ================================
# = Standardize count and weight =
# ================================
newf.raw[,area:=distance/10 * 1852 * 55.25 * 0.3048]

area.out <- newf.raw[,area]

bad15 <- data$duration == 15 & (area.out==0 | is.na(area.out))
area.out[bad15] = mean(area.out[newf.raw$duration==15 & area.out > 0 & !is.na(area.out) & newf.raw$yearl %in% data$year[bad15]])

bad25 <- data$duration == 25 & (area.out==0 | is.na(area.out))
area.out[bad25] = mean(area.out[newf.raw$duration==25 & area.out > 0 & !is.na(area.out) & newf.raw$yearl %in% data$year[bad25]])

bad30 <- data$duration == 30 & (area.out==0 | is.na(area.out))
area.out[bad30] = mean(area.out[newf.raw$duration==30 & area.out > 0 & !is.na(area.out) & newf.raw$yearl %in% data$year[bad30]])

meanarea <- mean(area.out, na.rm=TRUE)

newf.raw[,area:=area.out]
newf.raw[,wtcpue:=(wgt/100)*(meanarea/area)]
newf.raw[,numcpue:=num*(meanarea/area)]

# newf.raw[,list(sum(is.na(wtcpue)), sum(is.na(numcpue)))] # malin got 27, 448, I get 27, 451.


# ====================
# = Fix temperatures =
# ====================
# Change column class to numeric
newf.raw[,surftemp:=as.numeric(surftemp)]
newf.raw[,bottemp:=as.numeric(bottemp)]

# Fix the surface temp
fixT.surf <- newf.raw[,surftemp >= 900 & !is.na(surftemp)]
newf.raw[fixT.surf, surftemp:= -(surftemp - 900)/10]

fixT.surf2 <- newf.raw[,surftemp < 900 & surftemp > 0 & !is.na(surftemp)]
newf.raw[fixT.surf2, surftemp:=surftemp/10]
summary(newf.raw$surftemp) # 379,007 NAs (of 383,710 rows): nearly all missing # Ryan gets 379,007 NA's too

# Fix the bottom temp
fixT.bot <- newf.raw[,bottemp >= 900 & !is.na(bottemp)]
newf.raw[fixT.bot, bottemp:= -(bottemp - 900)/10]

fixT.bot2 <- newf.raw[,bottemp < 900 & bottemp > 0 & !is.na(bottemp)]
newf.raw[fixT.bot2, bottemp:=bottemp/10]
summary(newf.raw$bottemp) # only 6459 NAs


# =====================
# = Add Species Names =
# =====================
setkey(newf.raw, sppcode)
setkey(newf.spp, sppcode)
newf.raw <- merge(newf.raw, newf.spp, all.x=TRUE)


# =======================
# = Rename some columns =
# =======================
setnames(newf.raw, c("vessel", "trip", "set"), c("svvessel", "cruise", "tow"))

# =====================
# = Add empty columns =
# =====================
# newf.raw[,station:=as.character(NA)]
# newf.raw[,surfsal:=as.numeric(NA)]
# newf.raw[,botsal:=as.numeric(NA)]


# ====================================
# = Identifying Spring/ Fall Surveys =
# ====================================
fallseries <- c(as.character(newf.surv1$CRUISE[newf.surv1$Series %in% c('2GH - Stratified Random Bottom Trawl Survey - Campelen 1800', 'Fall - Stratified Random Bottom Trawl Survey - Campelen 1800')]), as.character(newf.surv2$cruise[newf.surv2$season=='fall']))

springseries <- c(as.character(newf.surv1$CRUISE[newf.surv1$Series %in% c('Annual 3P - Stratified Random Bottom Trawl Survey - Campelen 1800', 'Spring 3LNO - Stratified Random Bottom Trawl Survey - Campelen 1800')]), as.character(newf.surv2$cruise[newf.surv2$season == 'spring']))

cruiseid <- paste(newf.raw$svvessel, formatC(newf.raw$cruise, width=3, flag=0), sep='')

is.fall <- cruiseid %in% fallseries
is.spring <- cruiseid %in% springseries
newf.season <- rep(NA, nrow(newf.raw))
newf.season[is.fall] <- "fall"
newf.season[is.spring] <- "spring"

bad.series <- is.na(newf.season)

newf.raw[, season:=newf.season]


# Fix years for spring and fall (fall might cover 2 calendar years per 1 survey)
# TODO 
datafal$yearsurv = datafal$year
datafal$yearsurv[datafal$month<4] =	datafal$yearsurv[datafal$month<4] - 1

# ===============
# = Trim Strata =
# ===============
newf00 <- newf.raw
setnames(newf00, "yearl", "year")
setkey(newf00, season)


# Trim to high quality strata (sampled every year)
	# for Spring
	strat.year.spr <- newf00["spring"][,table(stratum, year)]
	sys.spring.rsum <- rowSums(strat.year.spr>0)
	sys.spring.csum <- colSums(strat.year.spr>0)
		# as.data.frame(sys.spring.rsum) # how many years per stratum?
	# 		hist(sys.spring.rsum, breaks=60, col='grey') # most strata cover 1 years, next most cover 15 #????
	# 		sum(sys.spring.rsum==14) # 34 stratum
	# 		sum(sys.spring.rsum==15) # 50 strata
	# 	as.data.frame(sys.spring.csum) # how many strata per year?
	# 	hist(sys.spring.csum, col='grey', breaks=60) # most years have 82 strata
	strat.year.spr2 <- strat.year.spr[sys.spring.rsum==15,]#;  strat.year.spr2
		# colSums(strat.year.spr2>0) # all years have 50 strata
	# 	rowSums(strat.year.spr2>0) # all strata have 15 years
	strats.spr <- rownames(strat.year.spr2)[rowSums(strat.year.spr2>0)==15]
	# 	length(strats.spr) # 50 strata
	# 	 	i = !duplicated(newf00["spring"]$haulid) & newf00["spring"]$stratum %in% strats.spr
	# 	 	dev.new();plot(newf00["spring"]$lon[i], newf00["spring"]$lat[i])
	# 		plot(newf00["spring"]$lon, newf00["spring"]$lat)
	#
	# dataspr = newf00["spring"][newf00["spring"]$stratum %in% strats.spr,]
	# 	dim(dataspr) # 62,249

	# Fall
	strat.year.fall <- newf00["fall"][,table(stratum, year)]
	sys.fall.rsum <- rowSums(strat.year.fall>0)
	sys.fall.csum <- colSums(strat.year.fall>0)
		# as.data.frame(sys.fall.rsum) # how many years per stratum?
	# 		hist(sys.fall.rsum, breaks=60, col='grey') # most strata cover 1 years, next most cover 15 #????
			sum(sys.fall.rsum==15) # 31 stratum #?? I get 
			sum(sys.fall.rsum==16) # 130 strata
	# 	as.data.frame(sys.fall.csum) # how many strata per year?
		hist(sys.fall.csum, col='grey', breaks=60) # most years have ~230 strata
	strat.year.fall2 <- strat.year.fall[sys.fall.rsum==16,]#;  strat.year.fall2
		colSums(strat.year.fall2>0) # all years have 130 strata # ?????
		rowSums(strat.year.fall2>0) # all strata have 16 years
	strats.fall <- rownames(strat.year.fall2)[rowSums(strat.year.fall2>0)==16]
		length(strats.fall) # 50 strata
		 	i = !duplicated(newf00["fall"]$haulid) & newf00["fall"]$stratum %in% strats.fall
		 	dev.new();plot(newf00["fall"]$lon[i], newf00["fall"]$lat[i])
	# 		plot(newf00["fall"]$lon, newf00["fall"]$lat)
	#
	# datafall = newf00["fall"][newf00["fall"]$stratum %in% strats.fall,]
	# 	dim(datafall) # 62,249
	
	
	
	i = table(datafal$stratum, datafal$yearsurv); i
	sum = rowSums(i>0)
	sumc = colSums(i>0)
		as.data.frame(sum) # how many years per stratum?
			hist(sum, breaks=60, col='grey') # most strata cover 1 years, next most cover 16
			sum(sum==15) # 31 strata
			sum(sum==16) # 130 strata
		as.data.frame(sumc) # how many strata per year?
		hist(sumc, col='grey', breaks=60) # most years have ~230 strata
	i2 = i[sum==16,];  i2
		colSums(i2>0) # all years have 130 strata
		rowSums(i2>0) # all strata have 16 years
			all(rowSums(i2>0)==16)
	strats = rownames(i2)[rowSums(i2>0)==16]
		length(strats) # 130 strata
		 	i = !duplicated(datafal$haulid) & datafal$stratum %in% strats
		 	plot(datafal$lon[i], datafal$lat[i])
		
	datafal = datafal[datafal$stratum %in% strats,]
		dim(datafal) # 166,034



















names_chars <- c('recordtype', 'vessel', 'trip', 'set', 'yearl', 'monthl', 'dayl', 'settype', 'stratum', 'nafo', 'unitarea', 'light', 'winddir', 'windforce', 'sea', 'bottom', 'timel', 'duration', 'distance', 'operation', 'depth', 'depthmin', 'depthmax', 'depthbottom', 'surftemp', 'bottemp', 'latstart', 'lonstart', 'posmethod', 'gear', 'sppcode', 'num', 'wgt', 'latend', 'lonend', 'bottempmeth', 'geardevice')
for (col in names_chars) {
	set(newf.raw0)
}
