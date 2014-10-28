## Prep datafiles for the DFO Newfoundland spring and fall trawl survey

##################
## Read in data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Newfoundland')
#strata = read.csv('.csv')
files = list.files(path='Spring and Fall', pattern = "199[23456789]|200[0123456789]|201[012]") # only 1992 and later, since data format changes (more columns added)

n = numeric(0); ch = numeric(0)
data = data.frame(recordtype = n, vessel = n, trip = n, set = n, yearl=n, monthl = n, dayl = n, settype = n, stratum = n, nafo = ch, unitarea = ch, light = n, winddir = n, windforce = n, sea = n, bottom = n, timel = n, duration = n, distance = n, operation = n, depth = n, depthmin = n, depthmax = n, depthbottom = n, surftemp = n, bottemp = n, latstart = n, lonstart = n, posmethod = n, gear = n, sppcode = n, num = n, wgt = n, latend = n, lonend = n, bottempmeth = n, geardevice = n)

options(warn = 1)
for(i in 1:length(files)){ # for each file
	if(i == 1) print(length(files)) # 45 files
	print(i)
	indata = read.fwf(file=paste('Spring and Fall/', files[i], sep=''), widths=c(
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

	names(indata) = c('recordtype', 'vessel', 'trip', 'set', 'yearl', 'monthl', 'dayl', 'settype', 'stratum', 'nafo', 'unitarea', 'light', 'winddir', 'windforce', 'sea', 'bottom', 'timel', 'duration', 'distance', 'operation', 'depth', 'depthmin', 'depthmax', 'depthbottom', 'surftemp', 'bottemp', 'latstart', 'lonstart', 'posmethod', 'gear', 'sppcode', 'num', 'wgt', 'latend', 'lonend', 'bottempmeth', 'geardevice')

	data = rbind(data, indata)
	print(dim(data))
}

# write out file, since previous step takes so long
	write.csv(data, paste('Output/dataorig_', Sys.Date(), '.csv', sep=''))

# read in file
	data = read.csv('Output/dataorig_2012-07-07.csv', row.names=1, stringsAsFactors=FALSE)

# examine
	sort(unique(data$vessel))
	length(unique(data$trip)) # 497
	length(unique(data$set)) # 842

# Trim to high-quality tows
		# sort(unique(data$operation))
		# table(data$operation)
	i = data$operation %in% c(1,2) & data$recordtype == 6 # 6 is biological data, 5 is set information
		sum(i) # 505,255
	data = data[i,]
		dim(data)

# Trim to Campelen 1800 lined shrimp trawl gear
	# sort(unique(data$gear))
	# table(data$yearl, data$gear)
	
	i = data$gear == 61 & !is.na(data$gear) # CAMPELEN 1800 SHRIMP TRAWL--LINED
		sum(i) # 395,392
	data = data[i,]
		dim(data)

# Trim to surveys
	# sort(unique(data$settype))
		# table(data$settype, data$yearl)
	i = data$settype == 1 # set-type == survey
		sum(i) # 383,710
	data = data[i,]
		dim(data)
	
# Create a unique haulid
	data$haulid = paste(formatC(data$vessel, width=2, flag=0), formatC(data$trip, width=3, flag=0), formatC(data$set, width=3, flag=0, format='d'), sep='-')
	length(unique(data$haulid)) # 17,220

# What years, months,days,times? (local time)
	data$yearl = data$yearl + 1900 # l stands for local date/time
	data$yearl[data$yearl<1950] = data$yearl[data$yearl<1950] + 100 # correct for 2000s
		# table(data$yearl, data$monthl)
		# sum(is.na(data$yearl))
		# sum(is.na(data$monthl))
		# sum(is.na(data$dayl))
		# sum(is.na(data$timel))

	# Convert from local time to UTC
		k = strptime(paste(data$yearl, data$monthl, data$dayl, formatC(as.numeric(as.character(data$timel)), width=4, flag=0)), tz='Canada/Newfoundland', format="%Y %m %d %H%M") # first convert to POSIXlt object
		length(k) # 383,710
		sum(is.na(k)) # 0
	dt.pos = as.POSIXct(k) # convert to POSIXct object.
#	 		head(dt.pos)
	dtu = format(dt.pos, tz='GMT', usetz=TRUE) # convert to UTC in text
#		head(dtu)
	dtl = as.POSIXlt(dtu, tz='GMT') # convert back to POSIXlt so I can extract year/month/day/time
#		head(dtl)
#		summary(dtl$mon)
	data$year = 1900 + dtl$year
	data$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
	data$day = dtl$mday # day of the month
	data$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')
		# check my conversions
#		data[!duplicated(data[,c('year', 'month', 'day')]),c('yearl', 'monthl', 'dayl', 'timel', 'year', 'month', 'day', 'time')][1:50,]

#	table(data$month, data$year)
		# spring and fall somewhat overlap

	require(date)
	data$julian = as.numeric(as.date(paste(data$month, data$day, data$year, sep='/')))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1
#		data[!duplicated(data[,c('year', 'month', 'day')]),c('month', 'day', 'year', 'time', 'julian')][1:50,]

## 	write.csv(data, paste('Output/datainterim_', Sys.Date(), '.csv', sep=''))
##  data = read.csv('Output/datainterim_2012-07-16.csv', row.names=1)

# Format lat/lon
	data$lat = NA
	i = data$latstart>0 & data$latend > 0
	data$lat[i] = (as.numeric(substr(data$latstart[i], 1, 2)) + as.numeric(substr(data$latstart[i], 3, 5))/600 + as.numeric(substr(data$latend[i], 1, 2)) + as.numeric(substr(data$latend[i], 3, 5))/600)/2 # lat as mean of start and end lat. format of latstart and latend is DDMM.M
	i = data$latstart>0 & data$latend == 0
		sum(i) # 22
	data$lat[i] = as.numeric(substr(data$latstart[i], 1, 2)) + as.numeric(substr(data$latstart[i], 3, 5))/600 # only use start if end is not available
	i = data$latstart==0 & data$latend > 0
		sum(i) # 0 
	
	data$lon = NA
	i = data$lonstart>0 & data$lonend > 0
	data$lon[i] = -(as.numeric(substr(data$lonstart[i], 1, 2)) + as.numeric(substr(data$lonstart[i], 3, 5))/600 + as.numeric(substr(data$lonend[i], 1, 2)) + as.numeric(substr(data$lonend[i], 3, 5))/600)/2 # lon as mean of start and end lon. format of lonstart and lonend is DDMM.M
	i = data$lonstart>0 & data$lonend == 0
		sum(i) # 22
	data$lon[i] = -(as.numeric(substr(data$lonstart[i], 1, 2)) + as.numeric(substr(data$lonstart[i], 3, 5))/600) # only use start if end is not available
	i = data$lonstart==0 & data$lonend > 0
		sum(i) # 0 
	#	data[1:20,c('latstart', 'latend', 'lat', 'lonstart', 'lonend', 'lon')]
	
	i =	is.na(data$lat) | is.na(data$lon)
		sum(i) # 0

# Standardize count and weight
	area = data$distance/10 * 1852 * 55.25 * 0.3048 # area trawled: distance in nm with one decimal * 1852 m/nm * 55.25 ft wide * 0.3048 m/ft
		round(range(area, na.rm=T)) # 0 to 118,515 m2 per tow
		# hist(area)
		sort(unique(data$duration[area == 0 | is.na(area)])) # 15, 25, or 30
		i = data$duration == 15 & (area==0 | is.na(area)) # missing area from a 15 min tow
			sort(unique(data$year[i])) # 1998 only
		area[i] = mean(area[data$duration==15 & area > 0 & !is.na(area) & data$year %in% data$year[i]]) # fill with mean area for a 15 min tow in the appropriate years
		
		i = data$duration == 25 & (area==0 | is.na(area)) # missing area from a 15 min tow
			sort(unique(data$year[i])) # 2007 only
		area[i] = mean(area[data$duration==25 & area > 0 & !is.na(area) & data$year %in% data$year[i]]) # NaN, unfortunately

		i = data$duration == 30 & (area==0 | is.na(area)) # missing area from a 15 min tow
			sort(unique(data$year[i])) # 1997 only
		area[i] = mean(area[data$duration==30 & area > 0 & !is.na(area) & data$year %in% data$year[i]]) # mean area for a 30 min tow in 1997

	round(range(area, na.rm=T)) # 9,356 to 118,515 m2 per tow
	meanarea = mean(area, na.rm=T)
		meanarea # 25,458 m2

	data$wtcpue = data$wgt/100 * meanarea/area # in kg/tow
	data$numcpue = data$num * meanarea/area # in num/tow
		sum(is.na(data$wtcpue))	# 27
		sum(is.na(data$numcpue)) # 448

# Fix temperature fields: one decimal place. 900 means negative
	i = data$surftemp >= 900 & !is.na(data$surftemp)
	data$surftemp[i] = -(data$surftemp[i] - 900)/10
	i = data$surftemp < 900 & data$surftemp > 0 & !is.na(data$surftemp)
	data$surftemp[i] = data$surftemp[i]/10
		summary(data$surftemp) # 379,007 NAs (of 383,710 rows): nearly all missing

	i = data$bottemp >= 900 & !is.na(data$bottemp)
	data$bottemp[i] = -(data$bottemp[i] - 900)/10
	i = data$bottemp < 900 & data$bottemp > 0 & !is.na(data$bottemp)
	data$bottemp[i] = data$bottemp[i]/10
		summary(data$bottemp)
	
# Add spp names
	spp = read.csv('Tables/GFSPCY.CODE_2012-07-05.csv', row.names=1)
		dim(data)
	data = merge(data, spp, all.x=TRUE)
		dim(data)

# Fix column names
	names(data)[names(data)=='vessel'] = 'svvessel'
	names(data)[names(data)=='trip'] = 'cruise'
	names(data)[names(data)=='set'] = 'tow'
	names(data)

# Add blank columns that are missing in this region
	data$station = NA
	data$surfsal = NA
	data$botsal = NA

# Check for non-standard NA codes and fix where needed
	summary(data$numcpue)
	summary(data$wtcpue)
	summary(data$surftemp)
		# hist(data$surftemp)
	summary(data$bottemp)
		# hist(data$bottemp)


# Separate spring and fall (should have done this earlier, but it's ok)
	table(data$month, data$year) # some years are difficult to separate spring and fall
	surveys = read.csv('Tables/surveys_table.csv')
		sort(unique(surveys$Series))
	surveys2 = read.csv('Tables/surveys_table2009-2011.csv')
	fallseries = c(as.character(surveys$CRUISE[surveys$Series %in% c('2GH - Stratified Random Bottom Trawl Survey - Campelen 1800', 'Fall - Stratified Random Bottom Trawl Survey - Campelen 1800')]), as.character(surveys2$cruise[surveys2$season=='fall']))
	springseries = c(as.character(surveys$CRUISE[surveys$Series %in% c('Annual 3P - Stratified Random Bottom Trawl Survey - Campelen 1800', 'Spring 3LNO - Stratified Random Bottom Trawl Survey - Campelen 1800')]), as.character(surveys2$cruise[surveys2$season == 'spring']))
	
	cruiseid = paste(data$svvessel, formatC(data$cruise, width=3, flag=0), sep='') # CRUISE is concatenated vessel and trip
	datafal = data[cruiseid %in% fallseries,] # also take any fall tows in later years: need to ask Bill Brodie which cruiseids are actually correct!
	dataspr = data[cruiseid %in% springseries,] # also take any spring tows in later years: need to ask Bill Brodie which cruiseids are actually correct!
		dim(datafal) # 261,130
		dim(dataspr) # 86,519
		
		table(dataspr$month, dataspr$year) # April to July (mostly May/June)
		table(datafal$month, datafal$year) # September to March, mostly Oct-Dec

# Add a survey year and a julian day that turns over July 1 (mostly for fall surveys)
	require(date)
	
	dataspr$yearsurv = dataspr$year
	dataspr$juliansurv = dataspr$julian
	
	datafal$yearsurv = datafal$year
	datafal$yearsurv[datafal$month<4] =	datafal$yearsurv[datafal$month<4] - 1
	datafal$juliansurv = as.numeric(as.date(paste(datafal$month, datafal$day, datafal$year, sep='/')))-as.numeric(as.date(paste('07/01/', datafal$yearsurv, sep='')))
	

# Trim to high-quality years (sample all strata) (choose this after examining strata definitions below)
	table(dataspr$stratum, dataspr$year) # 1996 to 2011, no 2008 (15 years)
	table(datafal$stratum, datafal$yearsurv) # 1995 to 2011, no 2008 (16 years)
				
# Trim to high quality strata (sampled every year)
	# for Spring
	i = table(dataspr$stratum, dataspr$year); # i
	sum = rowSums(i>0)
	sumc = colSums(i>0)
		as.data.frame(sum) # how many years per stratum?
			hist(sum, breaks=60, col='grey') # most strata cover 1 years, next most cover 15
			sum(sum==14) # 34 stratum
			sum(sum==15) # 50 strata
		as.data.frame(sumc) # how many strata per year?
		hist(sumc, col='grey', breaks=60) # most years have 82 strata
	i2 = i[sum==15,];  i2
		colSums(i2>0) # all years have 50 strata
		rowSums(i2>0) # all strata have 15 years
	strats = rownames(i2)[rowSums(i2>0)==15]
		length(strats) # 50 strata
		 	i = !duplicated(dataspr$haulid) & dataspr$stratum %in% strats
		 	plot(dataspr$lon[i], dataspr$lat[i])
		
	dataspr = dataspr[dataspr$stratum %in% strats,]
		dim(dataspr) # 62,249

	# Fall
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
	
	# Plot a map colored by stratum
	quartz(width=10, height=5); par(mfrow=c(1,2))
		i = !duplicated(dataspr$haulid)
		cols = rainbow(length(unique(dataspr$stratum[i])))
		d = rank(dataspr$stratum[i], na.last='keep', ties.method='min')
		du = sort(unique(d))
		for(j in 1:length(du)) d[d==du[j]] = j
		cols = sample(rainbow(length(du)), size=length(du), replace=FALSE)
	plot(dataspr$lon[i], dataspr$lat[i], col=cols[d], pch=16, cex=0.5, main='Spring')
		i = !duplicated(datafal$haulid)
		cols = rainbow(length(unique(datafal$stratum[i])))
		d = rank(datafal$stratum[i], na.last='keep', ties.method='min')
		du = sort(unique(d))
		for(j in 1:length(du)) d[d==du[j]] = j
		cols = sample(rainbow(length(du)), size=length(du), replace=FALSE)
	plot(datafal$lon[i], datafal$lat[i], col=cols[d], pch=16, cex=0.5, main='Fall')

# Reasonable depths? 
	# Fix to numeric if not
	if(class(dataspr$depth)=='character') dataspr$depth = as.numeric(dataspr$depth)
	if(class(datafal$depth)=='character') datafal$depth = as.numeric(datafal$depth)

	hist(dataspr$depth, breaks=seq(0,1000,by=25), col='grey') # most are 50 to 350
	range(dataspr$depth, na.rm=T) # 35-723

	hist(datafal$depth, breaks=seq(0,1000,by=25), col='grey') # most are 50 to 400
	range(datafal$depth, na.rm=T) # 37-1479

# Trim out or fix speed and duration records
		hist(dataspr$duration, breaks=100) # nearly all are 15 or 16 min
		hist(datafal$duration, breaks=100) # nearly all are 15 or 16 min, but one is 68

		range(dataspr$duration) # 10 to 22
		range(datafal$duration) # 8 to 68

		hist(dataspr$distance, breaks=100) # mostly 0.8 nm
		hist(datafal$distance, breaks=100) # mostly 0.8 nm

		range(dataspr$distance) # .5 to 1.1
		range(datafal$distance) # .4 to 3.4

	# Trim out one outlier from the Fall
	i = datafal$duration<=60
		sum(is.na(i))
		sum(i) # 102,820
		sum(!i) # 26 to be trimmed
		datafal[which(!i)[1], c('year', 'svvessel', 'cruise', 'tow')]
	datafal = datafal[i,]
		dim(datafal) # 166,008
		
# Add strata areas
	strat1 = read.fwf('Tables/stratum_areas.ver1.fwf', widths=c(
		3, #stratum
		4, # area in square nautical miles
		4, # max depth
		3), # NAFO division
		col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)
	strat2 = read.fwf('Tables/stratum_areas.ver2.fwf', widths=c(3, 4, 4, 3), col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)
	strat3 = read.fwf('Tables/stratum_areas.ver3.fwf', widths=c(3, 4, 4, 3), col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)
	strat4 = read.fwf('Tables/stratum_areas.ver4.fwf', widths=c(3, 4, 4, 3), col.names = c('stratum', 'area', 'maxdepth', 'nafo'), stringsAsFactors=FALSE)
	
	# Convert square nautical miles to square meters
	strat1$aream2 = strat1$area*3429904
	strat2$aream2 = strat2$area*3429904
	strat3$aream2 = strat3$area*3429904
	strat4$aream2 = strat4$area*3429904
	
	# Trim out spaces in NAFO division names
	strat1$nafo = gsub(" ", "", strat1$nafo)
	strat2$nafo = gsub(" ", "", strat2$nafo)
	strat3$nafo = gsub(" ", "", strat3$nafo)
	strat4$nafo = gsub(" ", "", strat4$nafo)

	# Spring
	sort(unique(dataspr$nafo))
	dataspr$nafo = as.character(dataspr$nafo) # to allow comparison with strat files
	dataspr$stratarea = NA
	stratyr = paste(dataspr$yearsurv, dataspr$stratum, dataspr$nafo)
	stratyrs = sort(unique(stratyr))
	length(stratyrs) # 750
	for(i in 1:length(stratyrs)){
		print(i)
		j = which(stratyr == stratyrs[i]); length(j)
		if(all(dataspr$yearsurv[j] == dataspr$yearsurv[j][1]) & all(dataspr$stratum[j] == dataspr$stratum[j][1]) & all(dataspr$nafo[j] == dataspr$nafo[j][1])){ # some error checking
			if(dataspr$nafo[j][1] == '3L'){ # stratification changed in 3L in 1997 and 1998 (spring)
				if(dataspr$yearsurv[j][1] <= 1996){
					k = which(strat2$nafo == dataspr$nafo[j][1] & strat2$stratum == dataspr$stratum[j][1])
					if(length(k)==1) dataspr$stratarea[j] = strat2$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}
				if(dataspr$yearsurv[j][1] == 1997){
					k = which(strat3$nafo == dataspr$nafo[j][1] & strat3$stratum == dataspr$stratum[j][1])
					if(length(k)==1) dataspr$stratarea[j] = strat3$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
				if(dataspr$yearsurv[j][1] >= 1998){
					k = which(strat4$nafo == dataspr$nafo[j][1] & strat4$stratum == dataspr$stratum[j][1])
					if(length(k)==1) dataspr$stratarea[j] = strat4$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
			}
			if(dataspr$nafo[j][1] %in% c('3N', '3O')){ # stratification changed in 3NO in 1994
				if(dataspr$yearsurv[j][1] <= 1993){
					stop(paste("Didn't deal with these years for", i))				
				}
				if(dataspr$yearsurv[j][1] >= 1994){
					k = which(strat3$nafo == dataspr$nafo[j][1] & strat3$stratum == dataspr$stratum[j][1])
					if(length(k)==1) dataspr$stratarea[j] = strat3$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
			}
			if(!(dataspr$nafo[j][1] %in% c('3L', '3N', '3O'))) stop(paste("Didn't deal with this stratum yet", i))
		} else { stop(paste('Not all year/strat/nafo match for i', i)) }
	}

	range(dataspr$stratarea)

	# Fall
	datafal$nafo = as.character(datafal$nafo) # to allow comparison with strat files
	datafal$stratarea = NA
	stratyr = paste(datafal$yearsurv, datafal$stratum, datafal$nafo)
	stratyrs = sort(unique(stratyr))
	length(stratyrs) # 2080
	for(i in 1:length(stratyrs)){
		print(i)
		j = which(stratyr == stratyrs[i]); length(j)
		if(all(datafal$yearsurv[j] == datafal$yearsurv[j][1]) & all(datafal$stratum[j] == datafal$stratum[j][1]) & all(datafal$nafo[j] == datafal$nafo[j][1])){ # some error checking
			if(datafal$nafo[j][1] == '3L'){ # stratification changed in 3L in 1996 and 1997 (fall)
				if(datafal$yearsurv[j][1] <= 1995){
					k = which(strat2$nafo == datafal$nafo[j][1] & strat2$stratum == datafal$stratum[j][1])
					if(length(k)==1) datafal$stratarea[j] = strat2$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}
				if(datafal$yearsurv[j][1] == 1996){
					k = which(strat3$nafo == datafal$nafo[j][1] & strat3$stratum == datafal$stratum[j][1])
					if(length(k)==1) datafal$stratarea[j] = strat3$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
				if(datafal$yearsurv[j][1] >= 1997){
					k = which(strat4$nafo == datafal$nafo[j][1] & strat4$stratum == datafal$stratum[j][1])
					if(length(k)==1) datafal$stratarea[j] = strat4$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
			}
			if(datafal$nafo[j][1] %in% c('2J', '3K')){ # stratification changed in 2J3K in 1993, 1996 and 1997
				if(datafal$yearsurv[j][1] <= 1992) stop(paste("Didn't deal with these years for", i))
				if(datafal$yearsurv[j][1] <= 1995 & datafal$yearsurv[j][1] >= 1993){
					k = which(strat2$nafo == datafal$nafo[j][1] & strat2$stratum == datafal$stratum[j][1]); k; strat2[k,]
					if(length(k)==1) datafal$stratarea[j] = strat2$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}
				if(datafal$yearsurv[j][1] == 1996){
					k = which(strat3$nafo == datafal$nafo[j][1] & strat3$stratum == datafal$stratum[j][1])
					if(length(k)==1) datafal$stratarea[j] = strat3$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
				if(datafal$yearsurv[j][1] >= 1997){
					k = which(strat4$nafo == datafal$nafo[j][1] & strat4$stratum == datafal$stratum[j][1])
					if(length(k)==1) datafal$stratarea[j] = strat4$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
			}
			if(datafal$nafo[j][1] %in% c('3N', '3O')){ # stratification changed in 3NO in 1994
				if(datafal$yearsurv[j][1] <= 1993){
					stop(paste("Didn't deal with these years for", i))				
				}
				if(datafal$yearsurv[j][1] >= 1994){
					k = which(strat3$nafo == datafal$nafo[j][1] & strat3$stratum == datafal$stratum[j][1])
					if(length(k)==1) datafal$stratarea[j] = strat3$aream2[k]
					else stop(paste('<> 1 strat index for', i))
				}				
			}
			if(!(datafal$nafo[j][1] %in% c('3L', '2J', '3K', '3N', '3O'))) stop(paste("Didn't deal with this stratum yet", i))
		} else { stop(paste('Not all year/strat/nafo match for i', i)) }
	}

	range(datafal$stratarea)

# Find duplicate tows in same location
	# SPRING
		# first sort by time so first index is always the earliest
		dataspr = dataspr[order(dataspr$yearsurv, dataspr$month, dataspr$day, dataspr$time, dataspr$spp),]
			
		# Any completely duplicated rows?
		dups = which(duplicated(dataspr))
			length(dups) # 0: NO
			dim(dataspr) # 62,249
	
		# find tows that have same lat/lon but different haulid
		dups = which(duplicated(dataspr[,c('year', 'lat', 'lon')]) & !duplicated(dataspr$haulid))
			length(dups) # 0: NO

	# FALL
		# first sort by time so first index is always the earliest
		datafal = datafal[order(datafal$yearsurv, datafal$month, datafal$day, datafal$time, datafal$spp),]
			
		# Any completely duplicated rows?
		dups = which(duplicated(datafal))
			length(dups) # 0: NO
			dim(datafal) # 166,008
	
		# find tows that have same lat/lon but different haulid
		dups = which(duplicated(datafal[,c('year', 'lat', 'lon')]) & !duplicated(datafal$haulid))
			length(dups) # 0: NO

					
# Create list of all hauls
	goodhaulspr = dataspr[!duplicated(dataspr$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhaulspr) # 3342
	head(goodhaulspr)

	goodhaulfal = datafal[!duplicated(datafal$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhaulfal) # 7415
	head(goodhaulfal)

# Adjust spp names for those cases where they've changed
	# SPRING
		dataspr$spp = as.character(dataspr$spp)
		tab = table(dataspr$spp, dataspr$year); write.csv(tab, 'Output/sppbyyearspr.csv') # for checking by eye
	
		i = dataspr$spp %in% c('ARTEDIELLUS ATLANTICUS', 'ARTEDIELLUS UNCINATUS')
		dataspr$spp[i] = 'ARTEDIELLUS  SP.'

		i = dataspr$spp %in% c('BUCCINUM  SP.', 'BUCCINUM TOTTENI', 'BUCCINUM UNDATUM')
		dataspr$spp[i] = 'BUCCINIDAE' # didn't search for all included genera (many!)

		i = dataspr$spp %in% c('CHIONOECETES OPILIO FEMALE', 'CHIONOECETES OPILIO MALE')
		dataspr$spp[i] = 'CHIONOECETES OPILIO'

		i = dataspr$spp %in% c('EUALUS GAIMARDII BELCHERI', 'EUALUS GAIMARDII GAIMARDII')
		dataspr$spp[i] = 'EUALUS GAIMARDII'

		i = dataspr$spp %in% c('EUMICROTREMUS SPINOSUS VARIABILIS')
		dataspr$spp[i] = 'EUMICROTREMUS SPINOSUS'

		i = dataspr$spp %in% c('GAIDROPSARUS ARGENTATUS', 'GAIDROPSARUS ENSIS')
		dataspr$spp[i] = 'GAIDROPSARUS  SP.'

		i = dataspr$spp %in% c('GONATUS FABRICII')
		dataspr$spp[i] = 'GONATUS  SP.'

		i = dataspr$spp %in% c('GORGONOCEPHALUS ARCTICUS')
		dataspr$spp[i] = 'GORGONOCEPHALIDAE'

		i = dataspr$spp %in% c('HYAS ARANEUS', 'HYAS COARCTATUS')
		dataspr$spp[i] = 'HYAS  SP.'

		i = dataspr$spp %in% c('LIPARIS FABRICII', 'LIPARIS LIPARIS', 'LIPARIS TUNICATUS')
		dataspr$spp[i] = 'LIPARIDAE'

		i = dataspr$spp %in% c('LYCENCHELYS PAXILLUS')
		dataspr$spp[i] = 'LYCENCHELYS  SP.'

		i = dataspr$spp %in% c('NOTACANTHUS NASUS')
		dataspr$spp[i] = 'NOTACANTHIDAE'

		i = dataspr$spp %in% c('PANDALUS BOREALIS(FEE 1ST W/HR)', 'PANDALUS BOREALIS(FEE 1ST W/O HR)', 'PANDALUS BOREALIS(FEE OVIG.)', 'PANDALUS BOREALIS(FEE(1+) W/HR)', 'PANDALUS BOREALIS(FEE(1+) W/O HR)', 'PANDALUS BOREALIS(MALE)', 'PANDALUS BOREALIS(TRANS. W/HR)', 'PANDALUS BOREALIS(TRANS. W/O HR)')
		dataspr$spp[i] = 'PANDALUS BOREALIS'

		i = dataspr$spp %in% c('PANDALUS MONTAGUI(FEE 1ST W/HR)', 'PANDALUS MONTAGUI(FEE 1ST W/O HR)', 'PANDALUS MONTAGUI(FEE OVIG.)', 'PANDALUS MONTAGUI(FEE(1+) W/HR)', 'PANDALUS MONTAGUI(FEE(1+) W/O HR)', 'PANDALUS MONTAGUI(MALE)', 'PANDALUS MONTAGUI(TRANS. W/HR)', 'PANDALUS MONTAGUI(TRANS.W/O HR)')
		dataspr$spp[i] = 'PANDALUS MONTAGUI'

		i = dataspr$spp %in% c('PARALEPIS  SP.', 'PARALEPIS BREVIS (ATLANTICA)', 'PARALEPIS COREGONOIDES BOREALIS', 'ANOTOPTERIDAE', 'ANOTOPTERUS PHARAO', 'NOTOLEPIS RISSOI KROYERI') # see http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=162532: Anotopteridae should be Paralepididae
		dataspr$spp[i] = 'PARALEPIDIDAE' # most observations seem to be at family level

		i = dataspr$spp %in% c('PASIPHAEA MULTIDENTATA', 'PASIPHAEA TARDA')
		dataspr$spp[i] = 'PASIPHAEA  SP.'

		i = dataspr$spp %in% c('SIMENCHELYS PARASITICUS')
		dataspr$spp[i] = 'SIMENCHELYIDAE'

		i = dataspr$spp %in% c('TRIGLOPS MURRAYI', 'TRIGLOPS NYBELINI', 'TRIGLOPS PINGELI')
		dataspr$spp[i] = 'TRIGLOPS  SP.'
	
		# remove unidentified spp
		i = !(dataspr$spp %in% c('EGGS, FISH(SPAWN)', 'EGGS, INVERTEBRATE', 'EGGS, SKATE CASES', 'EGGS, UNIDENTIFIED', 'OFFAL, OTHER', 'PLANT MATERIAL', 'SHELLS', 'STONE', 'UNIDENTIFIED FISH'))
			sum(i) # 61,512
		dataspr = dataspr[i,]
			dim(dataspr) # 61,512

		# Remove spp without a scientific name
		i = dataspr$spp == '' | is.na(dataspr$spp)
			sum(i) # 107
			sort(unique(dataspr$sppcode[i]))
			dataspr[i, c('sppcode', 'spp', 'year', 'wtcpue')] # includes 'SEA POTATO' with no scientific name, same for 'SEA ASTERIAN RUBANS', 'SEA PORANIA', 'MUD'
		dataspr = dataspr[!i,]
			dim(dataspr) # 61,405

		# Double check names
		sort(unique(dataspr$spp))

	# FALL
		datafal$spp = as.character(datafal$spp)
		tab = table(datafal$spp, datafal$year); write.csv(tab, 'Output/sppbyyearfal.csv') # for checking by eye
	
		i = datafal$spp %in% c('ARTEDIELLUS ATLANTICUS', 'ARTEDIELLUS UNCINATUS')
		datafal$spp[i] = 'ARTEDIELLUS  SP.'

		i = datafal$spp %in% c('BUCCINUM  SP.', 'BUCCINUM TOTTENI', 'BUCCINUM UNDATUM')
		datafal$spp[i] = 'BUCCINIDAE' # didn't search for all included genera (many!)

		i = datafal$spp %in% c('CHIONOECETES OPILIO FEMALE', 'CHIONOECETES OPILIO MALE')
		datafal$spp[i] = 'CHIONOECETES OPILIO'

		i = datafal$spp %in% c('EUALUS GAIMARDII BELCHERI', 'EUALUS GAIMARDII GAIMARDII')
		datafal$spp[i] = 'EUALUS GAIMARDII'

		i = datafal$spp %in% c('EUMICROTREMUS SPINOSUS VARIABILIS')
		datafal$spp[i] = 'EUMICROTREMUS SPINOSUS'

		i = datafal$spp %in% c('GAIDROPSARUS ARGENTATUS', 'GAIDROPSARUS ENSIS')
		datafal$spp[i] = 'GAIDROPSARUS  SP.'

		i = datafal$spp %in% c('GONATUS FABRICII')
		datafal$spp[i] = 'GONATUS  SP.'

		i = datafal$spp %in% c('GORGONOCEPHALUS ARCTICUS', 'GORGONOCEPHALUS SP.')
		datafal$spp[i] = 'GORGONOCEPHALIDAE'

		i = datafal$spp %in% c('HYAS ARANEUS', 'HYAS COARCTATUS')
		datafal$spp[i] = 'HYAS  SP.'

		i = datafal$spp %in% c('LIPARIS ATLANTICUS', 'LIPARIS FABRICII', 'LIPARIS GIBBUS', 'LIPARIS LIPARIS', 'LIPARIS TUNICATUS')
		datafal$spp[i] = 'LIPARIDAE'
		
		i = datafal$spp %in% c('LITHODES  SP.', 'LITHODES  SP.', 'NEOLITHODES  SP.', 'NEOLITHODES GRIMALDII')
		datafal$spp[i] = 'LITHODIDAE'

		i = datafal$spp %in% c('LYCENCHELYS PAXILLUS', 'LYCENCHELYS SARSI', 'LYCENCHELYS VERRILLI')
		datafal$spp[i] = 'LYCENCHELYS  SP.'

		i = datafal$spp %in% c('NOTACANTHUS NASUS')
		datafal$spp[i] = 'NOTACANTHIDAE'

		i = datafal$spp %in% c('PANDALUS BOREALIS(FEE 1ST W/HR)', 'PANDALUS BOREALIS(FEE 1ST W/O HR)', 'PANDALUS BOREALIS(FEE 1ST)', 'PANDALUS BOREALIS(FEE OVIG.)', 'PANDALUS BOREALIS(FEE(1+) W/HR)', 'PANDALUS BOREALIS(FEE(1+) W/O HR)', 'PANDALUS BOREALIS(MALE)', 'PANDALUS BOREALIS(TRANS. W/HR)', 'PANDALUS BOREALIS(TRANS. W/O HR)')
		datafal$spp[i] = 'PANDALUS BOREALIS'

		i = datafal$spp %in% c('PANDALUS MONTAGUI(FEE 1ST W/HR)', 'PANDALUS MONTAGUI(FEE 1ST W/O HR)', 'PANDALUS MONTAGUI(FEE OVIG.)', 'PANDALUS MONTAGUI(FEE(1+) W/HR)', 'PANDALUS MONTAGUI(FEE(1+) W/O HR)', 'PANDALUS MONTAGUI(MALE)', 'PANDALUS MONTAGUI(TRANS. W/HR)', 'PANDALUS MONTAGUI(TRANS.W/O HR))')
		datafal$spp[i] = 'PANDALUS MONTAGUI'

		i = datafal$spp %in% c('PARALEPIS  SP.', 'PARALEPIS BREVIS (ATLANTICA)', 'PARALEPIS COREGONOIDES BOREALIS', 'ANOTOPTERIDAE', 'ANOTOPTERUS PHARAO', 'NOTOLEPIS RISSOI KROYERI') # see http://www.itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=162532: Anotopteridae should be Paralepididae
		datafal$spp[i] = 'PARALEPIDIDAE' # most observations seem to be at family level

		i = datafal$spp %in% c('PASIPHAEA MULTIDENTATA', 'PASIPHAEA TARDA')
		datafal$spp[i] = 'PASIPHAEA  SP.'

		i = datafal$spp %in% c('SCOPELOSAURUS  SP.')
		datafal$spp[i] = 'SCOPELOSAURIDAE' # according to ITIS, this should be 	Scopelosauridae

		i = datafal$spp %in% c('SIMENCHELYS PARASITICUS')
		datafal$spp[i] = 'SIMENCHELYIDAE'

		i = datafal$spp %in% c('TRIGLOPS MURRAYI', 'TRIGLOPS NYBELINI', 'TRIGLOPS PINGELI')
		datafal$spp[i] = 'TRIGLOPS  SP.'
	
		# remove unidentified spp
		i = !(datafal$spp %in% c('EGGS, FISH(SPAWN)', 'EGGS, INVERTEBRATE', 'EGGS, SKATE CASES', 'EGGS, UNIDENTIFIED', 'OFFAL, OTHER', 'PLANT MATERIAL', 'SHELLS', 'STONE', 'UNIDENTIFIED FISH', 'UNIDENTIFIED MATERIAL'))
			sum(i) # 163,606
		datafal = datafal[i,]
			dim(datafal) # 163,606

		# Remove spp without a scientific name
		i = datafal$spp == '' | is.na(datafal$spp)
			sum(i) # 171
			sort(unique(datafal$sppcode[i]))
			sort(unique(datafal$common[i]))			
			datafal[i, c('sppcode', 'spp', 'common', 'year', 'wtcpue')] # includes 'SEA POTATO' with no scientific name, same for 'SEA ASTERIAN RUBANS', 'SEA PORANIA', 'MUD'
		datafal = datafal[!i,]
			dim(datafal) # 163,378

		# Double check names
		sort(unique(dataspr$spp))

# Find duplicate tows (repeat rows for the same tow), and combine entries from spp that have now been consolidated
	# SPRING
		# first sort by time so first index is always the earliest
		dataspr = dataspr[order(dataspr$year, dataspr$month, dataspr$day, dataspr$time, dataspr$spp, dataspr$haulid),]
	
		inds = which(duplicated(dataspr[, c('spp', 'year', 'haulid')]))
			length(inds) # 7116: mostly likely caused by combining spp names (see common names): these should be summed
			i = 4:10
			dataspr[dataspr$spp %in% dataspr$spp[inds[i]] & dataspr$year %in% dataspr$year[inds[i]] & dataspr$haulid %in% dataspr$haulid[inds[i]],]
			
		# write interim dataset
		write.csv(dataspr, paste('Output/datasprinterim_', Sys.Date(), '.csv', sep=''))	
				
		# useful function: acts like sum(na.rm=T) but returns NA if all are NA
		sumna = function(x){
			if(!all(is.na(x))) return(sum(x, na.rm=T))
			if(all(is.na(x))) return(NA)
		}
	
		# Sum the entries: looks like the right thing based on visual inspection of the data (same haulids)
		dataspr$sppyrhaulid = paste(dataspr$spp, dataspr$year, dataspr$haulid, sep='-')
		nm = c("svvessel", "cruise", "tow", "stratum", "nafo", "depth", "surftemp", "bottemp", "haulid", "year", "month", "day", "time", "julian", "lat", "lon", "common", "spp", "station", "surfsal", "botsal", "stratarea", "yearsurv", "juliansurv", "sppyrhaulid")

		newdataspr = aggregate(list(numcpue = dataspr$numcpue, wtcpue = dataspr$wtcpue), by=list(sppyrhaulid = dataspr$sppyrhaulid), FUN=sumna)		
			dim(newdataspr) # 54,289
		newdataspr = merge(newdataspr, dataspr[!duplicated(dataspr[, c('spp', 'year', 'haulid')]),nm])
			dim(newdataspr) # 54,289
			
		# Check the work
		nrow(dataspr) == nrow(newdataspr) + length(inds) # should be TRUE
	
		# Sort
		newdataspr = newdataspr[order(newdataspr$year, newdataspr$month, newdataspr$day, newdataspr$time, newdataspr$spp, newdataspr$haulid),]	


	# FALL
		# first sort by time so first index is always the earliest
		datafal = datafal[order(datafal$year, datafal$month, datafal$day, datafal$time, datafal$spp, datafal$haulid),]
	
		inds = which(duplicated(datafal[, c('spp', 'year', 'haulid')]))
			length(inds) # 18,351
			i = 1
			datafal[datafal$spp %in% datafal$spp[inds[i]] & datafal$year %in% datafal$year[inds[i]] & datafal$haulid %in% datafal$haulid[inds[i]],]
			
		# write interim dataset
		write.csv(datafal, paste('Output/datafalinterim_', Sys.Date(), '.csv', sep=''))	
	
		# useful function: acts like sum(na.rm=T) but returns NA if all are NA
		sumna = function(x){
			if(!all(is.na(x))) return(sum(x, na.rm=T))
			if(all(is.na(x))) return(NA)
		}
	
		# Sum the entries: looks like the right thing based on visual inspection of the data (same haulids)
		datafal$sppyrhaulid = paste(datafal$spp, datafal$year, datafal$haulid, sep='-')
		nm = c("svvessel", "cruise", "tow", "stratum", "nafo", "depth", "surftemp", "bottemp", "haulid", "year", "month", "day", "time", "julian", "lat", "lon", "common", "spp", "station", "surfsal", "botsal", "stratarea", "yearsurv", "juliansurv", "sppyrhaulid")

		newdatafal = aggregate(list(numcpue = datafal$numcpue, wtcpue = datafal$wtcpue), by=list(sppyrhaulid = datafal$sppyrhaulid), FUN=sumna)		
			dim(newdatafal) # 145,027
		newdatafal = merge(newdatafal, datafal[!duplicated(datafal[, c('spp', 'year', 'haulid')]),nm])
			dim(newdatafal) # 145,027

		# Check the work
		nrow(datafal) == nrow(newdatafal) + length(inds) # should be TRUE

		# Sort
		newdatafal = newdatafal[order(newdatafal$year, newdatafal$month, newdatafal$day, newdatafal$time, newdatafal$spp, newdatafal$haulid),]	

# How many tows?
	length(unique(paste(newdataspr$year, newdataspr$lat, newdataspr$lon))) # 3342 unique locations
	length(unique(newdataspr$haulid)) # 3342: good this matches # locations	

	length(unique(paste(newdatafal$year, newdatafal$lat, newdatafal$lon))) # 7415 unique locations
	length(unique(newdatafal$haulid)) # 4876: good this matches # locations	

# How many spp?
	length(unique(newdataspr$spp)) # 299
	length(unique(newdatafal$spp)) # 506

# Add a region column
newdataspr$region = "DFO_Newfoundland_Spring"
newdatafal$region = "DFO_Newfoundland_Fall"

# How many hauls missing from data?
	setdiff(goodhaulspr$haulid, unique(newdataspr$haulid)) # lose no hauls
	setdiff(goodhaulfal$haulid, unique(newdatafal$haulid)) # lose no hauls

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	ncol(newdataspr)
	length(nm)
	setdiff(nm, names(newdataspr)) # missing from new data: 0 = good
	setdiff(names(newdataspr), nm) # to be removed from newdata

	setdiff(nm, names(newdatafal)) # missing from new data: 0 = good
	setdiff(names(newdatafal), nm) # to be removed from newdata

newdatasprout = newdataspr[,nm]
	dim(newdatasprout) # 54,289 x 26

newdatafalout = newdatafal[,nm]
	dim(newdatafalout) # 145,027 x 26

# Write out
	write.csv(newdatasprout, paste('Output/dataspr_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhaulspr, paste('Output/goodhaulspr_', Sys.Date(), '.csv', sep=''))

	write.csv(newdatafalout, paste('Output/datafal_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhaulfal, paste('Output/goodhaulfal_', Sys.Date(), '.csv', sep=''))

#########################################	
## Trim to spp with data and add zeros ##
#########################################	
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Newfoundland')

dataspr = read.csv('Output/dataspr_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
goodhaulspr = read.csv('Output/goodhaulspr_2012-08-04.csv', row.names=1)
datafal = read.csv('Output/datafal_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
goodhaulfal = read.csv('Output/goodhaulfal_2012-08-04.csv', row.names=1)

# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# SPRING
	spplistspr = aggregate(list(count=dataspr$numcpue, weight = dataspr$wtcpue, pres = dataspr$wtcpue>0), by=list(spp=dataspr$spp, year=dataspr$year), FUN=sumna)
		spplistspr = aggregate(list(count=spplistspr$count, weight=spplistspr$weight, pres = spplistspr$pres, presyr = spplistspr$pres>0), by=list(spp=spplistspr$spp), FUN=sum) # presyr col holds # years in which spp was present
		rownames(spplistspr) = 1:nrow(spplistspr)
		nrow(spplistspr) # 299 spp
	
	sum(spplistspr$pres>0) # 299
	sum(spplistspr$pres>10) # 148
	max(spplistspr$presyr) # 15 yrs of data
	sum(spplistspr$presyr == max(spplistspr$presyr)) # 53 spp present every year

	spplistspr = spplistspr[spplistspr$presyr == max(spplistspr$presyr),] # take all spp present >= 1x per yr
		nrow(spplistspr) # 53 spp	

	spplistspr = merge(spplistspr, dataspr[!duplicated(dataspr$spp), c('common', 'spp')])
		dim(spplistspr)
		head(spplistspr)

	# FALL
	spplistfal = aggregate(list(count=datafal$numcpue, weight = datafal$wtcpue, pres = datafal$wtcpue>0), by=list(spp=datafal$spp, year=datafal$year), FUN=sumna)
		spplistfal = aggregate(list(count=spplistfal$count, weight=spplistfal$weight, pres = spplistfal$pres, presyr = spplistfal$pres>0), by=list(spp=spplistfal$spp), FUN=sum) # presyr col holds # years in which spp was present
		rownames(spplistfal) = 1:nrow(spplistfal)
		nrow(spplistfal) # 506 spp
	
	sum(spplistfal$pres>0) # 506 spp present at least once
	sum(spplistfal$pres>10) # 257
	max(spplistfal$presyr) # 16 yrs of data
	sum(spplistfal$presyr == max(spplistfal$presyr)) # 98 spp present every year

	spplistfal = spplistfal[spplistfal$presyr == max(spplistfal$presyr),] # take all spp present >= 1x per yr
		nrow(spplistfal) # 98 spp	

	spplistfal = merge(spplistfal, datafal[!duplicated(datafal$spp), c('common', 'spp')])
		dim(spplistfal)
		head(spplistfal)

# Remove spp not in spplist
	dim(dataspr)
	dataspr = dataspr[dataspr$spp %in% spplistspr$spp,]
	dim(dataspr) # 37,428

	dim(datafal)
	datafal = datafal[datafal$spp %in% spplistfal$spp,]
	dim(datafal) # 124,741

# Any spp duplicated in the same haul?
	i = duplicated(paste(dataspr$haulid, dataspr$spp))
	sum(i) # 0 good

	j = dataspr$haulid == dataspr$haulid[i] & dataspr$spp == dataspr$spp[i]
	sum(j) # 0 good
	
	k = duplicated(dataspr)
	sum(k) # 0 good


	i = duplicated(paste(datafal$haulid, datafal$spp))
	sum(i) # 0 good

	j = datafal$haulid == datafal$haulid[i] & datafal$spp == datafal$spp[i]
	sum(j) # 0 good
	
	k = duplicated(datafal)
	sum(k) # 0 good

# Add any missing hauls
	inds = which(!(goodhaulspr$haulid %in% dataspr$haulid))
	length(inds) # 0 missing

	inds = which(!(goodhaulfal$haulid %in% datafal$haulid))
	length(inds) # 0 missing

# Fill in zeros
	# SPRING
	fin = length(unique(dataspr$haulid))*length(unique(dataspr$spp)) # expected final size
		fin # 177,126
	hauls = unique(dataspr$haulid)
		length(hauls)
		nrow(goodhaulspr) # same: 3342
		
	# set up sets of unique hauls and spp
	newdataspr = dataspr[!duplicated(dataspr$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'yearsurv', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'juliansurv', 'day')]
		dim(newdataspr) # 3342
		newdataspr = newdataspr[order(newdataspr$haulid),] # sort by haulid
		rownames(newdataspr) = 1:nrow(newdataspr)
	spps = dataspr[!duplicated(dataspr$spp), c('spp', 'common')]
		dim(spps) # 53
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdataspr = newdataspr[rep(rownames(newdataspr), rep(nrow(spps), nrow(newdataspr))),]
		dim(newdataspr) # 177,126
			
		# add spp info, replicated so that each haul gets a species
	newdataspr = cbind(newdataspr, spps[rep(rownames(spps), length(unique(newdataspr$haulid))),])
		dim(newdataspr) # 177,126
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datasprna = dataspr[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datasprna$numcpue[is.na(dataspr$numcpue)] = -9999
		datasprna$wtcpue[is.na(dataspr$wtcpue)] = -9999
	newdataspr = merge(newdataspr, datasprna[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdataspr) # 177,126
		summary(newdataspr$numcpue) # some -9999: some missing
		summary(newdataspr$wtcpue) # none missing

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdataspr$numcpue[is.na(newdataspr$numcpue)] = 0
	newdataspr$wtcpue[is.na(newdataspr$wtcpue)] = 0
	newdataspr$numcpue[newdataspr$numcpue == -9999] = NA
	newdataspr$wtcpue[newdataspr$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdataspr[,c('haulid', 'spp')]))
	length(inds) # NO, if this is == 0

		#data[inds,]
	
		# Check: how many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdataspr$haulid), by=list(spp=newdataspr$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdataspr$spp))
		length(inds) # should be 0
		#newdataspr[inds,]
		
		#newdataspr = newdataspr[-inds,]
		#dim(newdataspr)

	# sort by time so first index is always the earliest
	newdataspr = newdataspr[order(newdataspr$year, newdataspr$month, newdataspr$day, newdataspr$time, newdataspr$spp),]

	# FALL
	fin = length(unique(datafal$haulid))*length(unique(datafal$spp)) # expected final size
		fin # 726,670
	hauls = unique(datafal$haulid)
		length(hauls)
		nrow(goodhaulfal) # same: 7415
		
	# set up sets of unique hauls and spp
	newdatafal = datafal[!duplicated(datafal$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'yearsurv', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'juliansurv', 'day')]
		dim(newdatafal) # 7415
		newdatafal = newdatafal[order(newdatafal$haulid),] # sort by haulid
		rownames(newdatafal) = 1:nrow(newdatafal)
	spps = datafal[!duplicated(datafal$spp), c('spp', 'common')]
		dim(spps) # 98
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdatafal = newdatafal[rep(rownames(newdatafal), rep(nrow(spps), nrow(newdatafal))),]
		dim(newdatafal) # 726,670
			
		# add spp info, replicated so that each haul gets a species
	newdatafal = cbind(newdatafal, spps[rep(rownames(spps), length(unique(newdatafal$haulid))),])
		dim(newdatafal) # 726,670
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datafalna = datafal[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datafalna$numcpue[is.na(datafal$numcpue)] = -9999
		datafalna$wtcpue[is.na(datafal$wtcpue)] = -9999
	newdatafal = merge(newdatafal, datafalna[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdatafal) # 726,670
		summary(newdatafal$numcpue) # no -9999: none missing
		summary(newdatafal$wtcpue) # none missing

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdatafal$numcpue[is.na(newdatafal$numcpue)] = 0
	newdatafal$wtcpue[is.na(newdatafal$wtcpue)] = 0
	newdatafal$numcpue[newdatafal$numcpue == -9999] = NA
	newdatafal$wtcpue[newdatafal$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdatafal[,c('haulid', 'spp')]))
	length(inds) # NO, if this is == 0

		#data[inds,]
	
		# Check: how many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdatafal$haulid), by=list(spp=newdatafal$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdatafal$spp))
		length(inds) # should be 0
		#newdatafal[inds,]
		
		#newdatafal = newdatafal[-inds,]
		#dim(newdatafal)

	# sort by time so first index is always the earliest
	newdatafal = newdatafal[order(newdatafal$year, newdatafal$month, newdatafal$day, newdatafal$time, newdatafal$spp),]


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	dim(newdataspr)
	length(nm)
	setdiff(nm, names(newdataspr)) # should be 0
	setdiff(names(newdataspr), nm)

	newdataspr = newdataspr[,nm]

	dim(newdatafal)
	length(nm)
	setdiff(nm, names(newdatafal)) # should be 0
	setdiff(names(newdatafal), nm)

	newdatafal = newdatafal[,nm]

# Fix row names
	row.names(newdataspr) = 1:nrow(newdataspr)
	row.names(newdatafal) = 1:nrow(newdatafal)

# Write out
	write.csv(newdataspr, paste('Output/datasprtrimwzeros_', Sys.Date(), '.csv', sep=''))
	write.csv(newdatafal, paste('Output/datafaltrimwzeros_', Sys.Date(), '.csv', sep=''))


###########################################
## Data Prep for Climate-envelope models ##
###########################################

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Newfoundland')

## Read in data
load('../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

dataspr = read.csv('Output/datasprtrimwzeros_2012-08-04.csv', row.names=1)
	dim(dataspr) # 177,126
datafal = read.csv('Output/datafaltrimwzeros_2012-08-04.csv', row.names=1)
	dim(datafal) # 726,670
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	# SPRING
		tab = aggregate(dataspr$wtcpue>0, by=list(spp=dataspr$spp, year = dataspr$year), FUN=sum, na.rm=T)
		tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
		tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
		sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
		sppnames # These are spp to consider using: 30
		
		# Trim data to right spp
		datasprtrim = dataspr[dataspr$spp %in% sppnames,]
		dim(datasprtrim) # 100,260

	# FALL
		tab = aggregate(datafal$wtcpue>0, by=list(spp=datafal$spp, year = datafal$year), FUN=sum, na.rm=T)
		tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
		tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
		sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
		sppnames # These are spp to consider using: 60
		
		sppnames = setdiff(sppnames, c('SCYPHOZOA'))
		length(sppnames) # 59
		
		# Trim data to right spp
		datafaltrim = datafal[datafal$spp %in% sppnames,]
		dim(datafaltrim) # 437,485
			
## Add HadISST data (min and max temp)
	# SPRING
		datasprtrim$mintemp = NA
		datasprtrim$mintempmnth = NA
		datasprtrim$maxtemp = NA
		datasprtrim$maxtempmnth = NA
	
		# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
		datasprtrim$latgrid = floor(datasprtrim$lat)+0.5
		datasprtrim$longgrid = floor(datasprtrim$lon)+0.5
		inds = which(!duplicated(datasprtrim[,c('latgrid', 'longgrid', 'year', 'month')]))
			length(inds) # 590 combinations to process
		for(i in 1:length(inds)){
			if(i %% 50 == 0) print(i)
			lat = as.character(datasprtrim$latgrid[inds[i]]) # to match hadisst grid
			long = as.character(datasprtrim$longgrid[inds[i]]) # to match hadisst grid
			yr = as.character(datasprtrim$year[inds[i]])
			lastyr = as.character(as.numeric(yr)-1)
			thesemons = as.character(1:datasprtrim$month[inds[i]]) # months in this year, converted to char
			lastmons = as.character((datasprtrim$month[inds[i]]+1):12) # months we want from last year
	
			j = datasprtrim$latgrid == datasprtrim$latgrid[inds[i]] & datasprtrim$longgrid == datasprtrim$longgrid[inds[i]] & datasprtrim$year == datasprtrim$year[inds[i]] & datasprtrim$month == datasprtrim$month[inds[i]]
	
			# since above are char, we can use them as indices into hadisst
			temps = c(hadisst[lat,long,lastmons,lastyr], hadisst[lat,long,thesemons,yr]) # use the previous 12 months, including months in previous year
			if(all(is.na(temps[c('7', '8', '9')]))==TRUE){
				warning(paste('WARNING: No summer temps for i=', i))
			} else {
				datasprtrim$maxtemp[j] = max(temps, na.rm=T)
				datasprtrim$maxtempmnth[j] = as.numeric(names(temps)[which.max(temps)])
			}
			if(all(is.na(temps[c('1', '2', '3', '4')]))==TRUE){
				warning(paste('WARNING: No winter temps for i=', i))
			} else {
				datasprtrim$mintemp[j] = min(temps, na.rm=T)
				datasprtrim$mintempmnth[j] = as.numeric(names(temps)[which.min(temps)])
			}
		}

	# FALL
		datafaltrim$mintemp = NA
		datafaltrim$mintempmnth = NA
		datafaltrim$maxtemp = NA
		datafaltrim$maxtempmnth = NA
	
		# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
		datafaltrim$latgrid = floor(datafaltrim$lat)+0.5
		datafaltrim$longgrid = floor(datafaltrim$lon)+0.5
		inds = which(!duplicated(datafaltrim[,c('latgrid', 'longgrid', 'year', 'month')]))
			length(inds) # 1399 combinations to process
		for(i in 1:length(inds)){
			if(i %% 50 == 0) print(i)
			lat = as.character(datafaltrim$latgrid[inds[i]]) # to match hadisst grid
			long = as.character(datafaltrim$longgrid[inds[i]]) # to match hadisst grid
			yr = as.character(datafaltrim$year[inds[i]])
			lastyr = as.character(as.numeric(yr)-1)
			thesemons = as.character(1:datafaltrim$month[inds[i]]) # months in this year, converted to char
			if(max(thesemons)<12){
				lastmons = as.character((datafaltrim$month[inds[i]]+1):12) # months we want from last year
			} else {
				lastmons = character(0)
			}


			j = datafaltrim$latgrid == datafaltrim$latgrid[inds[i]] & datafaltrim$longgrid == datafaltrim$longgrid[inds[i]] & datafaltrim$year == datafaltrim$year[inds[i]] & datafaltrim$month == datafaltrim$month[inds[i]]
	
			# since above are char, we can use them as indices into hadisst
			temps = c(hadisst[lat,long,lastmons,lastyr], hadisst[lat,long,thesemons,yr]) # use the previous 12 months, including months in previous year
			if(all(is.na(temps[c('7', '8', '9')]))==TRUE){
				warning(paste('WARNING: No summer temps for i=', i))
			} else {
				datafaltrim$maxtemp[j] = max(temps, na.rm=T)
				datafaltrim$maxtempmnth[j] = as.numeric(names(temps)[which.max(temps)])
			}
			if(all(is.na(temps[c('1', '2', '3', '4')]))==TRUE){
				warning(paste('WARNING: No winter temps for i=', i))
			} else {
				datafaltrim$mintemp[j] = min(temps, na.rm=T)
				datafaltrim$mintempmnth[j] = as.numeric(names(temps)[which.min(temps)])
			}
		}


# Add a date and a time column (rather than DATETIME)
datasprtrim$date = paste(formatC(datasprtrim$month, width=2, flag=0), '/', formatC(datasprtrim$day, width=2, flag=0), '/', datasprtrim$year, sep='')

datafaltrim$date = paste(formatC(datafaltrim$month, width=2, flag=0), '/', formatC(datafaltrim$day, width=2, flag=0), '/', datafaltrim$year, sep='')

# Transform data
	datasprtrim$lsurftemp = log(datasprtrim$surftemp+1)
	datasprtrim$lbottemp = log(datasprtrim$bottemp+1)
	datasprtrim$lmintemp = log(datasprtrim$mintemp+1)	

	datafaltrim$lsurftemp = log(datafaltrim$surftemp+1)
	datafaltrim$lbottemp = log(datafaltrim$bottemp+1)
	datafaltrim$lmintemp = log(datafaltrim$mintemp+1)	
	
# Add pres/abs
	datasprtrim$pres = datasprtrim$numcpue>0 | (is.na(datasprtrim$numcpue) & !is.na(datasprtrim$wtcpue) & datasprtrim$wtcpue>0)

	datafaltrim$pres = datafaltrim$numcpue>0 | (is.na(datafaltrim$numcpue) & !is.na(datafaltrim$wtcpue) & datafaltrim$wtcpue>0)

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=datasprtrim$wtcpue), by=list(yearsurv=datasprtrim$yearsurv, spp=datasprtrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datasprtrim = merge(datasprtrim, bm, all.x=T)
	dim(datasprtrim) # 100,260

bm = aggregate(list(nummean=datasprtrim$numcpue), by=list(yearsurv=datasprtrim$yearsurv, spp=datasprtrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datasprtrim = merge(datasprtrim, bm, all.x=T)
	dim(datasprtrim) # 100,260

bm = aggregate(list(biomassmean=datafaltrim$wtcpue), by=list(yearsurv=datafaltrim$yearsurv, spp=datafaltrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datafaltrim = merge(datafaltrim, bm, all.x=T)
	dim(datafaltrim) # 437,485

bm = aggregate(list(nummean=datafaltrim$numcpue), by=list(yearsurv=datafaltrim$yearsurv, spp=datafaltrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datafaltrim = merge(datafaltrim, bm, all.x=T)
	dim(datafaltrim) # 437,485

# Have a cpue that never goes to zero (useful for fitting log-links)
datasprtrim$wtcpuena = datasprtrim$wtcpue
datasprtrim$wtcpuena[datasprtrim$wtcpuena == 0] = 1e-4
datasprtrim$wtcpuenal = log(datasprtrim$wtcpuena)

datasprtrim$numcpuena = datasprtrim$numcpue
datasprtrim$numcpuena[datasprtrim$numcpuena == 0] = 1e-4
datasprtrim$numcpuenal = log(datasprtrim$numcpuena)

datafaltrim$wtcpuena = datafaltrim$wtcpue
datafaltrim$wtcpuena[datafaltrim$wtcpuena == 0] = 1e-4
datafaltrim$wtcpuenal = log(datafaltrim$wtcpuena)

datafaltrim$numcpuena = datafaltrim$numcpue
datafaltrim$numcpuena[datafaltrim$numcpuena == 0] = 1e-4
datafaltrim$numcpuenal = log(datafaltrim$numcpuena)

# Rename columns
names(datasprtrim)[names(datasprtrim)=='longgrid'] = 'longrid' # use the adjusted longitude
names(datafaltrim)[names(datafaltrim)=='longgrid'] = 'longrid' # use the adjusted longitude


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datasprtrim)
	length(nm)
	
	setdiff(nm, names(datasprtrim))
	setdiff(names(datasprtrim), nm) # will drop common and stratarea
	 	 
	datasprtrim = datasprtrim[,nm]
		dim(datasprtrim) # 100,260 x 41

	setdiff(nm, names(datafaltrim))
	setdiff(names(datafaltrim), nm) # will drop common and stratarea
	 	 
	datafaltrim = datafaltrim[,nm]
		dim(datafaltrim) # 437,485 x 41

## Write out
	write.csv(datasprtrim, file=paste('Output/datasprCEM_', Sys.Date(), '.csv', sep=''))
	write.csv(datafaltrim, file=paste('Output/datafalCEM_', Sys.Date(), '.csv', sep=''))





##################
###   PLOTS    ###
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Newfoundland')

dataspr = read.csv('Output/datasprtrimwzeros_2012-07-19.csv', row.names=1)
	dim(dataspr) # 177,126
datafal = read.csv('Output/datafaltrimwzeros_2012-07-19.csv', row.names=1)
	dim(datafal) # 429,088

	sort(unique(datafal$spp))
	sort(unique(datafal$yearsurv))

# Plot maps of a species in a set of years (FALL)
	spp = 'NEMICHTHYS SCOLOPACEUS'
	spp = 'RAJA (BATHYRAJA) SPINICAUDA'
	yrs = sort(unique(datafal$yearsurv))
	scale = 5/max(datafal$wtcpue[datafal$spp==spp])
	xlim = range(datafal$lon[datafal$spp==spp])
	ylim = range(datafal$lat[datafal$spp==spp])
	
	quartz(height=8, width=8)
	# pdf(height=8, width=8, file=paste('Figures/sppmap_', spp, '_', Sys.Date(), '.pdf', sep=''))
	a = ceiling(sqrt(length(yrs)))
	par(mfrow=c(a,a), mai=c(0.5, 0.5, 0.3, 0.1), mgp=c(2,0.7,0))
	for(i in 1:length(yrs)){
		j = datafal$spp == spp & datafal$yearsurv == yrs[i] & datafal$wtcpue==0
		plot(datafal$lon[j], datafal$lat[j], cex=1, col='grey', pch='+', main=yrs[i], xlab='Longitude', ylab='Latitude', xlim=xlim, ylim=ylim)
		j = datafal$spp == spp & datafal$yearsurv == yrs[i] & datafal$wtcpue>0
		cexs = pmax(0.5, datafal$wtcpue[j]*scale) # make sure all presences are visible
		points(datafal$lon[j], datafal$lat[j], cex=cexs, col='red', pch=16)
	}
	
	dev.off()
	
# Plot depth maps against lat of a species in a set of years (FALL)
	spp = 'NEMICHTHYS SCOLOPACEUS'
	spp = 'RAJA (BATHYRAJA) SPINICAUDA'
	yrs = sort(unique(datafal$yearsurv))
	scale = 5/max(datafal$wtcpue[datafal$spp==spp])
	xlim = range(datafal$lat[datafal$spp==spp])
	ylim = rev(range(datafal$depth[datafal$spp==spp]))
	
	quartz(height=8, width=8)
	# pdf(height=8, width=8, file=paste('Figures/sppdepthmap_', spp, '_', Sys.Date(), '.pdf', sep=''))
	a = ceiling(sqrt(length(yrs)))
	par(mfrow=c(a,a), mai=c(0.5, 0.5, 0.3, 0.1), mgp=c(2,0.7,0))
	for(i in 1:length(yrs)){
		j = datafal$spp == spp & datafal$yearsurv == yrs[i] & datafal$wtcpue==0
		plot(datafal$lat[j], datafal$depth[j], cex=1, col='grey', pch='+', main=yrs[i], xlab='Latitude', ylab='Depth', xlim=xlim, ylim=ylim)
		j = datafal$spp == spp & datafal$yearsurv == yrs[i] & datafal$wtcpue>0
		cexs = pmax(0.5, datafal$wtcpue[j]*scale) # make sure all presences are visible
		points(datafal$lat[j], datafal$depth[j], cex=cexs, col='red', pch=16)
	}
	
	dev.off()