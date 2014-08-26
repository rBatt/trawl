
library(data.table)
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rm9s.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/calcarea.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")




newf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/DFO_Newfoundland/"


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



names_chars <- c('recordtype', 'vessel', 'trip', 'set', 'yearl', 'monthl', 'dayl', 'settype', 'stratum', 'nafo', 'unitarea', 'light', 'winddir', 'windforce', 'sea', 'bottom', 'timel', 'duration', 'distance', 'operation', 'depth', 'depthmin', 'depthmax', 'depthbottom', 'surftemp', 'bottemp', 'latstart', 'lonstart', 'posmethod', 'gear', 'sppcode', 'num', 'wgt', 'latend', 'lonend', 'bottempmeth', 'geardevice')
for (col in names_chars) {
	set(newf.raw0)
}
