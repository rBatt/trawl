## How fast are species moving in AFSC Eastern Bering Shelf data?


#########################
## Prep data           ##
#########################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Southern Gulf')
strata = read.csv('4T_RV_strata.csv')
catchdata = read.csv('southern Gulf survey data.csv')
	names(catchdata)
	dim(catchdata)
setdata = read.csv('sGSL_RV Survey sets_1971_2009.csv', na.strings='.')
	names(setdata)
	dim(setdata)
	intersect(names(data), names(setdata))

# Add temperature and salinity data to catch data
	names(catchdata)[names(catchdata) == 'expt'] = 'exptCatch' # so they don't conflict with names in setdata
	names(catchdata)[names(catchdata) == 'time'] = 'timeCatch'
	names(catchdata)[names(catchdata) == 'depth'] = 'depthCatch'
	intersect(names(catchdata), names(setdata)) # names that catchdata and setdata could match on
data = merge(catchdata, setdata[,c('vessel', 'cruise', 'year', 'set', 'expt', 'strat', 'month', 'day', 'time', 'depth', 't_surface', 't_bottom', 'salin_bottom')], all.x=T, all.y=T)
	dim(data) # 180652 (69 more rows than with all.y=F)
	
		# Fill in new tows that were just added
		i = which(is.na(data$latin_name) & is.na(data$biomass))
			length(i) # 69: good, matches above
			data$latin_name[i] = data$latin_name[1]
			data$name[i] = data$name[1]
			data$biomass[i] = 0
			data$catch[i] = 0
			data[i,]
			sort(unique(data$strat[i]))
			
# Create a unique haulid
	data$haulid = paste(as.character(data$vessel), formatC(data$cruise, width=3, flag=0), formatC(data$set, width=3, flag=0), sep='-')
	length(unique(data$haulid)) # 5578

# Trim to high quality tows
	data = data[data$expt %in% c(1,5),] # surveys and comparative tows

# Trim to high quality strata
	i=table(data$strat, data$year);i
		rs = rowSums(i>0); as.numeric(rs)
		keep = names(rs)[rs == max(rs)]; keep # 18
		drop = setdiff(names(rs), keep); drop # 12
	# data = data[!(data$strat %in% c(401:403, 421, 440:442)),] # for all expts
	data = data[!(data$strat %in% c(401:403, 421, 427, 440:442)),] # for only using expt ==1
	dim(data) # 154983

	table(data$strat, data$year)

# Trim to high quality years	
	# data = data[!(data$year %in% c(1978, 2003, 2005)),] # if only using expt==1
	data = data[!(data$year %in% c(1978, 2003)),] # if including expt 5
	dim(data) # 150745

	i=table(data$strat, data$year);i
		rs = rowSums(i>0); as.numeric(rs)
		keep = names(rs)[rs == max(rs)]; keep # 22
		drop = setdiff(names(rs), keep); drop # 0
	
# Reasonable depths? 
	#hist(data$depth) # 
	range(data$depth, na.rm=T) # 15-386

# Fix column names
	names(data)[names(data)=='vessel'] = 'svvessel'
	names(data)[names(data)=='strat'] = 'stratum'
	names(data)[names(data)=='set'] = 'tow'
	names(data)[names(data)=='latitude'] = 'lat'
	names(data)[names(data)=='longitude'] = 'lon' # use the adjusted longitude
	names(data)[names(data)=='t_surface'] = 'surftemp'
	names(data)[names(data)=='t_bottom'] = 'bottemp'
	names(data)[names(data)=='latin_name'] = 'spp'
	names(data)[names(data)=='biomass'] = 'wtcpue'
	names(data)[names(data)=='catch'] = 'numcpue'
	names(data)[names(data)=='salin_bottom'] = 'botsal'
	names(data)[names(data)=='name'] = 'common'

# Turn -9999 to NA
	range(data$numcpue)
		range(data$numcpue, na.rm=T)
	range(data$wtcpue)
		range(data$wtcpue, na.rm=T)
	range(data$surftemp)
		range(data$surftemp, na.rm=T)
	range(data$bottemp)
		range(data$bottemp, na.rm=T)
	range(data$botsal)
		range(data$botsal, na.rm=T)

# Add blank columns that are missing in this region
data$surfsal = NA
data$station = NA

# What months, days, time, in UTC?
	names(data)[names(data)=='month'] = 'monthLocal'
	names(data)[names(data)=='day'] = 'dayLocal'
	names(data)[names(data)=='time'] = 'timeLocal'
	
	# Convert from local time to UTC
	dt = strptime(paste(data$year, formatC(data$monthLocal, width=2, flag=0), data$dayLocal, formatC(data$timeLocal, width=4, flag=0), sep='-'), tz="America/Halifax", format="%Y-%m-%d-%H%M") # first convert to POSIXlt object
		head(dt)
	dt.pos = as.POSIXct(dt, tz='America/Halifax') # convert to POSIXct object
		head(dt.pos)
	dtu = format(dt.pos, tz='GMT', usetz=TRUE) # convert to UTC in text
		head(dtu)
	dtl = as.POSIXlt(dtu, tz='GMT') # convert back to POSIXlt so I can extract year/month/day/time
		head(dtl)
	data$month = dtl$mon+1 # month, add 1 since POSIXlt starts at 0	
	data$day = dtl$mday # day of the month
	data$time = paste(formatC(dtl$hour, width=2, flag=0), formatC(dtl$min, width=2, flag=0), sep=':')
		# check my conversions
		data[!duplicated(data[,c('year', 'month', 'day')]),c('year', 'monthLocal', 'dayLocal', 'timeLocal', 'month', 'day', 'time')][1:10,]

	table(data$year, data$month)
		# nearly always September. strays slightly earlier or later in a few years

	# Add julian day (in local time)
	require(date)
	data$julian = as.numeric(as.date(paste(data$monthLocal, '/', data$dayLocal, '/', data$year, sep='')))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1

# Add strata areas
dim(data)
data = merge(data, strata[,c('stratum', 'stratarea')])
dim(data) # 150745

# Find duplicate tows (repeat sets)
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]
	
	# turn factors to chars so we can modify them
	data$svvessel = as.character(data$svvessel)
	data$cruise = as.character(data$cruise)
	data$tow = as.character(data$tow)
	data$stratum = as.character(data$stratum)
	data$spp = as.character(data$spp)
	data$common = as.character(data$common)

	# find dups
	inds = which(duplicated(data[, c('spp', 'year', 'lat', 'lon')]))
	length(inds) # 1417
	
	# list of rows to drop (those that are duplicated)
	droprows = numeric(0)
	idsdrops = numeric(0) # haulids to drop
	
	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdata = data

	# Trim some unneeded columns from newdata (t_bottom has bottom temps, depth has mean of depthst and depthend)
	nm = names(newdata)[!(names(newdata) %in% c('species', 'temperature', 'expt', 'depthst', 'depthend', 'dtow', 'N', 'kg'))]
	newdata = newdata[,nm]
		dim(newdata)

	# Find the best entry (least missing data or earliest) and delete the dups
	for(i in 1:length(inds)){
		if(i %% 100 == 0) print(i)

		inds2 = which(data$spp == data$spp[inds[i]] & data$year == data$year[inds[i]] & data$lat == data$lat[inds[i]] & data$lon == data$lon[inds[i]])
		ids = sort(unique(data$haulid[inds2])) # find the haulids 
		data[inds2,]

		if(all(data$haulid[inds2] == data$haulid[inds2][1])) stop('add these? all from the same tow.')
		
		j0 = aggregate(list(surftemp = !is.na(data$surftemp[inds2]), bottemp = !is.na(data$bottemp[inds2])), by=list(haulid = data$haulid[inds2]), FUN=sum) # how many complete entries per haulid?
		j0$tot = (j0$surftemp>0) + (j0$bottemp>0) # how many complete variables per haulid?
		j0 = merge(j0, data[!duplicated(data$haulid) & data$haulid %in% ids, c('haulid', 'month', 'day', 'time')])
		j0 = j0[order(j0$month, j0$day, j0$time),] # order by time
		idskeep = j0$haulid[which(j0$tot == max(j0$tot))[1]] # pick the first haulid with the most complete entries
		idsdrop = ids[!(ids %in% idskeep)] # haulids to drop
		idsdrops = c(idsdrops, idsdrop)

		if(idskeep %in% idsdrops) stop('really, you want to keep this haulid? you wanted to drop it before') # make sure the haulid we like wasn't previously marked for removal
			
			# mark all rows in newdata with the other haulid for removal
		droprows = c(droprows, which(data$haulid %in% idsdrop))		
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values
	
	# drop the duplicated rows
	newdata = newdata[-droprows,]

	dim(data)
	dim(newdata)
	length(droprows) + nrow(newdata) # should match nrow(data)

# Create list of all tows (as opposed to other regions, do this AFTER eliminating duplicates, since in certain other regions I add duplicates togther. here I cut all but one)
	goodhauls = newdata[!duplicated(newdata$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhauls) # 4556
	head(goodhauls)

# Remove spp without a scientific name
	i = newdata$spp == ''
	sum(i) # 0 

# Find duplicate rows (repeat rows for the same tow), and combine entries from spp that have now been consolidated. Already checked for duplicate tows in same location (see above)
	# first sort by time so first index is always the earliest
	newdata = newdata[order(newdata$year, newdata$month, newdata$day, newdata$time, newdata$spp),]

	# Any completely duplicated rows?
	dups = which(duplicated(newdata))
	length(dups) # 0

	# Duplicated rows for the same tow
	inds = which(duplicated(newdata[, c('spp', 'year', 'haulid')]))
	length(inds) # 0

# How many tows?
	length(unique(paste(newdata$svvessel, newdata$cruise, newdata$tow))) # 4556
	length(unique(paste(newdata$year, newdata$lat, newdata$lon))) # 4556: since this matches, there is exactly one set/location/spp/year
	
#	i = which(duplicated(newdata[,c('year', 'latitude', 'longitude')]) & !duplicated(newdata[,c('year', 'latitude', 'longitude', 'set')])) # find all rows where lat/long/year matches, but set doesn't
#	i2 = numeric(0)
#	for(j in 1:length(i)){ # get all the other rows that match the first set
#		new = which(newdata$year == newdata$year[i[j]] & newdata$latitude == newdata$latitude[i[j]] & newdata$longitude == newdata$longitude[i[j]] & newdata$set != newdata$set[i[j]])
#		i2 = c(i2, new)
#	}
#	length(i); length(i2)
#	i3 = which(!duplicated(newdata[i2, c('year', 'latitude', 'longitude', 'set')])) # remove the duplicates from the second set
#	length(i3)
#	i4 = rbind(i, i2[i3])[1:(length(i)+length(i3))] # put the first and second set together so I can see why they don't match
#	length(i4)	
#	newdata[i4,]

# How many spp?
	sort(unique(newdata$spp))
	length(unique(newdata$spp)) #33
		
# Add a region column
newdata$region = "DFO_SoGulf"

# How many hauls missing from data?
	setdiff(goodhauls$haulid, unique(newdata$haulid)) # lose no hauls

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata$month, newdata$year) # surveys don't cross Jan 1, so yearsurv == year
	newdata$yearsurv = newdata$year
	newdata$juliansurv = newdata$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	dim(newdata)
	length(nm)
	setdiff(nm, names(newdata))
	setdiff(names(newdata), nm)

newdataout = newdata[,nm]
	dim(newdataout)

# Write out
	write.csv(newdataout, paste('Output/data_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhauls, paste('Output/goodhauls_', Sys.Date(), '.csv', sep=''))

#########################################	
## Trim to spp with data and add zeros ##
#########################################	
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Southern Gulf')

data = read.csv('Output/data_2012-04-02.csv', row.names=1, stringsAsFactors=FALSE)
	names(data)
goodhauls = read.csv('Output/goodhauls_2012-04-02.csv', row.names=1)	

# Identify the spp to analyze (all of them in this case)
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	spplist = aggregate(list(count=data$numcpue, weight = data$wtcpue, pres = data$wtcpue>0), by=list(spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(count=spplist$count, weight=spplist$weight, pres = spplist$pres, presyr = spplist$pres>0), by=list(spp=spplist$spp), FUN=sum) # presyr col holds # years in which spp was present
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 33 spp
	
	sum(spplist$pres>1) # 33
	sum(spplist$pres>10) # 33
	max(spplist$presyr) # 37 yrs of data
	sum(spplist$presyr == max(spplist$presyr)) # 14 spp present every year

	spplist = spplist[spplist$presyr == max(spplist$presyr),] # take all spp present >= 1x per yr
		nrow(spplist) # 14 spp	

	spplist = merge(spplist, data[!duplicated(data$spp), c('common', 'spp')])
		dim(spplist)
		head(spplist)

# Remove spp not in spplist
	dim(data)
	data = data[data$spp %in% spplist$spp,]
	dim(data) # 63784

# Any spp duplicated in the same haul?
	i = duplicated(paste(data$haulid, data$spp))
	sum(i)

# Add any missing hauls
	inds = which(!(goodhauls$haulid %in% data$haulid))
	length(inds) # 0 missing

# Fill in NAs for those species not included (snow crab and lobster)
# Only works for this dataset because all true zeros were already included by Hugues, while these two species weren't counted in all years
	fin = length(unique(data$haulid))*length(unique(data$spp)) # expected final size
		fin # 63,784
	hauls = unique(data$haulid)
		
	# set up sets of unique hauls and spp
	newdata = data[!duplicated(data$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'day')]
		dim(newdata)
		newdata = newdata[order(newdata$haulid),] # sort by haulid
		rownames(newdata) = 1:nrow(newdata)
	spps = data[!duplicated(data$spp), c('spp', 'common')]
		dim(spps)
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdata = newdata[rep(rownames(newdata), rep(nrow(spps), nrow(newdata))),]
		dim(newdata)
			
		# add spp info, replicated so that each haul gets a species
	newdata = cbind(newdata, spps[rep(rownames(spps), length(unique(newdata$haulid))),])
		dim(newdata)
		names(newdata)
		
		# add catch info. set NAs to -9999 so they're not confused with new records added later
		datana = data[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(data$numcpue)] = -9999
		datana$wtcpue[is.na(data$wtcpue)] = -9999
	newdata = merge(newdata, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdata) # 63784
		summary(newdata$numcpue) # catch has some -9999, but no NAs
		summary(newdata$wtcpue) # again, no NAs
	
		# set catch NAs to NA (new records: true missing since this is SGSL and Hugues already added all true zeros) and -9999 to NA (true missing data)
	newdata$numcpue[is.na(newdata$numcpue)] = NA # not needed
	newdata$wtcpue[is.na(newdata$wtcpue)] = NA # not needed
	newdata$numcpue[newdata$numcpue == -9999] = NA
	newdata$wtcpue[newdata$wtcpue == -9999] = NA

	dim(newdata)

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(data[,c('haulid', 'spp')]))
	length(inds) # GOOD if this == 0

		# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdata$haulid), by=list(spp=newdata$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdata$spp))
		length(inds) # should be 0

	# sort by time so first index is always the earliest
	newdata = newdata[order(newdata$year, newdata$month, newdata$day, newdata$time, newdata$spp),]

# Fix row names
	row.names(data) = 1:nrow(data)

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata$month, newdata$year) # surveys don't cross Jan 1, so yearsurv == year
	newdata$yearsurv = newdata$year
	newdata$juliansurv = newdata$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	dim(newdata)
	length(nm)
	setdiff(nm, names(newdata))
	setdiff(names(newdata), nm)

newdata = newdata[,nm]
	dim(newdata)


# Write out
	write.csv(newdata, paste('Output/datatrimwzeros_', Sys.Date(), '.csv', sep=''))

#######################
### Climate Envelope ##
#######################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Southern Gulf')

## Read in data
load('../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

data = read.csv('Output/datatrimwzeros_2012-07-23.csv', row.names=1)
	dim(data) # 63,784 x 26
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	tab = aggregate(data$wtcpue>0, by=list(spp=data$spp, year = data$year), FUN=sum, na.rm=T)
	tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
	tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
	sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
	sppnames # These are spp to consider using: 6
	
	# Trim data to right spp
	datatrim = data[data$spp %in% sppnames,]
	dim(datatrim) # 27,336 x 26
			
## Add HadISST data (min and max temp)
	datatrim$mintemp = NA
	datatrim$mintempmnth = NA
	datatrim$maxtemp = NA
	datatrim$maxtempmnth = NA

	# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
	datatrim$latgrid = floor(datatrim$lat)+0.5
	datatrim$longrid = floor(datatrim$lon)+0.5
	inds = which(!duplicated(datatrim[,c('latgrid', 'longrid', 'year', 'month')]))
		length(inds) # 1485 to fit
	for(i in 1:length(inds)){
		if(i %% 50 == 0) print(i)
		lat = as.character(datatrim$latgrid[inds[i]]) # to match hadisst grid
		long = as.character(datatrim$longrid[inds[i]]) # to match hadisst grid
			if(as.numeric(long) < -180) long = as.character(as.numeric(long)+360) # fix if long is west of date line
		yr = as.character(datatrim$year[inds[i]])
		lastyr = as.character(as.numeric(yr)-1)
		thesemons = as.character(1:datatrim$month[inds[i]]) # months in this year, converted to char
		lastmons = as.character((datatrim$month[inds[i]]+1):12) # months we want from last year

		j = datatrim$latgrid == datatrim$latgrid[inds[i]] & datatrim$longrid == datatrim$longrid[inds[i]] & datatrim$year == datatrim$year[inds[i]] & datatrim$month == datatrim$month[inds[i]]

		# since above are char, we can use them as indices into hadisst
		temps = c(hadisst[lat,long,lastmons,lastyr], hadisst[lat,long,thesemons,yr]) # use the previous 12 months, including months in previous year
		if(all(is.na(temps[c('7', '8', '9')]))==TRUE){
			warning(paste('WARNING: No summer temps for i=', i))
		} else {
			datatrim$maxtemp[j] = max(temps, na.rm=T)
			datatrim$maxtempmnth[j] = as.numeric(names(temps)[which.max(temps)])
		}
		if(all(is.na(temps[c('1', '2', '3', '4')]))==TRUE){
			warning(paste('WARNING: No winter temps for i=', i))
		} else {
			datatrim$mintemp[j] = min(temps, na.rm=T)
			datatrim$mintempmnth[j] = as.numeric(names(temps)[which.min(temps)])
		}
	}

# Add a date and a time column (rather than DATETIME)
datatrim$date = paste(formatC(datatrim$month, width=2, flag=0), '/', formatC(datatrim$day, width=2, flag=0), '/', datatrim$year, sep='')

# Transform data
	datatrim$lsurftemp = log(datatrim$surftemp+1)
	datatrim$lbottemp = log(datatrim$bottemp+1)
	datatrim$lmintemp = log(datatrim$mintemp+1)	
	
# Add pres/abs
	datatrim$pres = datatrim$numcpue>0 | (is.na(datatrim$numcpue) & !is.na(datatrim$wtcpue) & datatrim$wtcpue>0)

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=datatrim$wtcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm)
	dim(datatrim) # 27,336 x 38

bm = aggregate(list(nummean=datatrim$numcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm, all.x=T)
	dim(datatrim) # 27,336 x 39

# Have a cpue that never goes to zero (useful for fitting log-links)
datatrim$wtcpuena = datatrim$wtcpue
datatrim$wtcpuena[datatrim$wtcpuena == 0] = 1e-4
datatrim$wtcpuenal = log(datatrim$wtcpuena)

datatrim$numcpuena = datatrim$numcpue
datatrim$numcpuena[datatrim$numcpuena == 0] = 1e-4
datatrim$numcpuenal = log(datatrim$numcpuena)

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datatrim)
	length(nm)
	
	setdiff(nm, names(datatrim))
	setdiff(names(datatrim), nm) # remove stratarea and common
	 	 
datatrim = datatrim[,nm]
	dim(datatrim) # 27,336 x 41

## Write out for this species
	write.csv(datatrim, file=paste('Output/dataCEM_', Sys.Date(), '.csv', sep=''))


##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Southern Gulf')

datatrim = read.csv('Output/dataCEM_2012-02-17.csv', row.names=1)
