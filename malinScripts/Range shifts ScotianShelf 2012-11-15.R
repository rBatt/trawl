## How fast are species moving in AFSC Eastern Bering Shelf data?


#########################
## Prep data           ##
#########################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Scotian Shelf')
strata = read.csv('ScotianShelf_strata.csv')
data = read.csv('gscat_adj_pinsky.csv', header=TRUE)
	names(data)
	dim(data)
	summary(data)
setdata = read.csv('gsinf_pinsky.csv')
	names(setdata)
	names(setdata)[names(setdata)=='REMARKS'] = 'REMARKS.SET'
	dim(setdata)
	summary(setdata)
species = read.csv('species list.csv')
	head(species)

# Add set data to catch data and make sure all sets are included
data = merge(data, setdata, all.x=T, all.y=T, by=c('MISSION', 'SETNO')) # make sure all sets get added
	dim(data) # 185,324
	
	# Turn NAs to 0 where sets not in catch data were added
	i = which(is.na(data$SPEC) & is.na(data$ADJ_TOTWGT))
	data$SPEC[i] = sort(unique(data$SPEC))[1] # add a place-holder spp
	data$ADJ_TOTWGT[i] = 0
	data$ADJ_TOTNO[i] = 0

# Add species names
	data = merge(data, species, by.x='SPEC', by.y='CODE', all.x=TRUE)
		dim(data) # 185324
	
# Create a unique haulid
	data$haulid = paste(as.character(data$MISSION), formatC(data$SETNO, width=3, flag=0), sep='-')
	length(unique(data$haulid)) # 15706

# Trim to normal tows
	data = data[data$TYPE==1,]
	dim(data) # 170920
	length(unique(data$haulid)) # 13882

# Set up month, day, time, correct to UTC time
	data$year = as.numeric(substr(as.character(data$MISSION), 4,7))
	data$month = as.numeric(unlist(strsplit(as.character(data$SDATE), split='[-]|[/]'))[seq(2,3*nrow(data), by=3)])
	data$day = as.numeric(unlist(strsplit(as.character(data$SDATE), split='[-]|[/]'))[seq(3,3*nrow(data), by=3)])

	time = formatC(data$TIME, width=4, flag=0)
	hr = substr(time, 1,2)
	hr = as.numeric(hr) + 3 # convert to UTC
	i = which(hr > 23) # pushed into a new day, so decrement by 24 and add 1 to day
	data$day[i] = data$day[i] + 1
	hr[i] = hr[i] - 24
	data$time = paste(hr, substr(time, 3,4), sep=':') # make a time column with a colon
	
	# make sure day didn't push us into a new month (for 31-day months)
	i = which(data$day > 31)
		length(i)
		data[i,c('year', 'SDATE', 'TIME', 'month', 'day', 'time')]
		data$day[i] = data$day[i] - 31
		data$month[i] = data$month[i] + 1 # don't need to adjust year since only using June-August

	i = which(data$day>30 & data$month %in% c(9,4,6,11)) # for 30-day months
		length(i)
		data[i,c('year', 'SDATE', 'TIME', 'month', 'day', 'time')]
		data$day[i] = data$day[i] - 30
		data$month[i] = data$month[i] + 1 # don't need to adjust year since only using June-August

	# February (irrelevant, I'll eliminate it later)

	table(data$year, data$month)
		# July is consistent, but spread is February to December
		
	# Trim to July(ish)
	data = data[data$month >=6 & data$month <= 8,]
		dim(data) # 96179

	# Add julian day
	require(date)
	data$julian = as.numeric(as.date(paste(data$month, '/', data$day, '/', data$year, sep='')))-as.numeric(as.date(paste('01/01/', data$year, sep=''))) # julian day since Jan 1

# Trim to high quality strata
	ts = table(data$STRAT, data$year); ts
		tsr = rowSums(ts>0); as.numeric(tsr)
		tsc = colSums(ts>0); as.numeric(tsc)
		sskeep = rownames(ts[tsr==max(tsr),]); sort(sskeep)  # strata to keep
		length(sskeep) # 47 strata

	data = data[as.character(data$STRAT) %in% sskeep, ]
		data = droplevels(data)
		dim(data) # 91523
		setdiff(sskeep, as.character(data$STRAT)) # none, good

# Trim to high quality years	
	ts = table(data$STRAT, data$year); ts # they look good
		tsr = rowSums(ts>0); as.numeric(tsr)
		all(tsr==42)
		
# Reasonable depths? 
	# which field is depth?
	nnisna = function(x) sum(!is.na(x))
	aggregate(list(depth=data$DEPTH), by = list(year = data$year), FUN=nnisna) # missing after 1991
	aggregate(list(depth=data$START_DEPTH), by = list(year = data$year), FUN=nnisna) # missing all but a few in 2001
	aggregate(list(depth=data$END_DEPTH), by = list(year = data$year), FUN=nnisna)  # missing all but a few in 2001
	aggregate(list(depth=data$DMIN), by = list(year = data$year), FUN=nnisna) # all years
	aggregate(list(depth=data$DMAX), by = list(year = data$year), FUN=nnisna) # all years
	
	plot(data$DMIN, data$DEPTH)
	plot(data$DMAX, data$DEPTH)
	plot(rowMeans(cbind(data$DMIN, data$DMAX)), data$DEPTH) # this looks about the same
	data$DEPTH = rowMeans(cbind(data$DMIN, data$DMAX))
	aggregate(list(depth=data$DEPTH), by = list(year = data$year), FUN=nnisna) # now all years

	range(data$DEPTH, na.rm=T) # 12-272


# Fix lat/lon coding
	data$lat = as.numeric(substr(data$SLAT,1,2)) + as.numeric(substr(data$SLAT,3,4))/60
	data$lon = -as.numeric(substr(data$SLON,1,2)) - as.numeric(substr(data$SLON,3,4))/60

# Fix column names
	names(data)[names(data)=='MISSION'] = 'cruise'
	names(data)[names(data)=='STRAT'] = 'stratum'
	names(data)[names(data)=='SETNO'] = 'tow'
	names(data)[names(data)=='SURFACE_TEMPERATURE'] = 'surftemp'
	names(data)[names(data)=='BOTTOM_TEMPERATURE'] = 'bottemp'
	names(data)[names(data)=='BOTTOM_SALINITY'] = 'botsal'
	names(data)[names(data)=='DEPTH'] = 'depth'
	names(data)[names(data)=='ADJ_TOTWGT'] = 'wtcpue'
	names(data)[names(data)=='ADJ_TOTNO'] = 'numcpue'

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

# Add vessel
	data$svvessel = substr(data$cruise, 1,3)

# Add strata areas
	strata$stratarea = strata$stratarea_nmi2 * (1.852^2)

	dim(data)
	data = merge(data, strata[,c('stratum', 'stratarea')])
	dim(data)

# Add blank columns that are missing in this region
data$surfsal = NA
data$station = NA

# Find duplicate tows in same location
	# first sort by time so first index is always the earliest
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]
	
	# turn factors to chars so we can modify them
	data$svvessel = as.character(data$svvessel)
	data$cruise = as.character(data$cruise)
	data$tow = as.character(data$tow)
	data$stratum = as.character(data$stratum)
	data$spp = as.character(data$spp)
	data$common = as.character(data$common)

	# Any completely duplicated rows?
	dups = which(duplicated(data))
		sum(dups) # 0

	# find tows that have same lat/lon but different haulid
	inds = which(duplicated(data[,c('year', 'lat', 'lon')]) & !duplicated(data$haulid))
		length(inds) # 1

	# list of rows to drop (those that are duplicated)
	droprows = numeric(0)
	idsdrops = numeric(0) # haulids to drop

	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdata = data

	# Trim some unneeded columns from newdata (t_bottom has same bottom temps, depth has mean of depthst and depthend)
	nm = c('svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	newdata = newdata[,nm]
		dim(newdata)

	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# Find the best entry (least missing data or earliest) and delete the duplicate tows
	for(i in 1:length(inds)){
		if(i %% 10 == 0) print(i)
	
		inds2 = which(data$year == data$year[inds[i]] & data$lat == data$lat[inds[i]] & data$lon == data$lon[inds[i]])
		ids = sort(unique(data$haulid[inds2])) # find the haulids 
		data[inds2,]
			
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

	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdata$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows, and the tows I'm eliminating
	newdata = newdata[-droprows,]

	dim(data)
	dim(newdata)
	length(droprows) + nrow(newdata) # should match nrow(data)

	# Step 2: find duplicate entries for the same species in the same tow
	inds = which(duplicated(newdata[, c('spp', 'year', 'lat', 'lon')]))
	# inds = which(duplicated(newdata[, c('spp', 'year', 'lat', 'lon', 'cruise', 'tow')])) # same as the previous row
	length(inds) # 66
	
	# list of rows to drop (those that are duplicated)
	droprows = numeric(0)
	
	# Find the best entry (least missing data or earliest) and delete the dups
	for(i in 1:length(inds)){
		if(i %% 10 == 0) print(i)
	
		inds2 = which(newdata$spp == newdata$spp[inds[i]] & newdata$year == newdata$year[inds[i]] & newdata$lat == newdata$lat[inds[i]] & newdata$lon == newdata$lon[inds[i]])
		newdata[inds2,]

		# put sums in the first row if they're from the same tow
		if(all(newdata$haulid[inds2] == newdata$haulid[inds2][1])){
			temp = data.frame(
			numcpue = sumna(newdata$numcpue[inds2]), 
			wtcpue = sumna(newdata$wtcpue[inds2]))	

			newdata$numcpue[inds2[1]] = temp$numcpue
			newdata$wtcpue[inds2[1]] = temp$wtcpue
	
			# mark the following row(s) for removal
			droprows = c(droprows, inds2[2:length(inds2)])
		} else {		
			stop('Any non-identical tows in the same location should have already been deleted')
		}
		
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values

	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdata$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows, and the tows I'm eliminating
	newdata = newdata[-droprows,]

	dim(newdata)
	length(droprows) + nrow(newdata) # should match previous nrow(newdata)


# Fix a weird entry: only SST entry in 2010
	i = which(newdata$haulid == 'NED2010027-201')
		length(i)
	newdata$surftemp[i] = NA

# Create list of all tows now that I've solved duplicates problem
	goodhauls = newdata[!duplicated(newdata$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhauls) # 7267
	head(goodhauls)
	length(unique(data$haulid)) # also 7267


# Number of unique tows matches number of unique year/lat/lon?
	length(unique(paste(newdata$cruise, newdata$tow))) # 7267
	length(unique(paste(newdata$year, newdata$lat, newdata$lon))) # 7267
		
# How many spp?
	sort(unique(newdata$spp))
	length(unique(newdata$spp)) #425
	
	# remove unidentified spp
	i = grep('UNIDENT| UNID|^UNID', newdata$spp)
	sort(unique(newdata$spp[i]))
		dim(newdata)
	newdata = newdata[-i,]
		dim(newdata) # 91262

	length(unique(newdata$spp)) #416
		
# Add a region column
newdata$region = "DFO_ScotianShelf"

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
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Scotian Shelf')
data = read.csv('Output/data_2012-11-15.csv', row.names=1)
goodhauls = read.csv('Output/goodhauls_2012-11-15.csv', row.names=1)

# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	spplist = aggregate(list(count=data$numcpue, weight = data$wtcpue, pres = data$wtcpue>0), by=list(spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(count=spplist$count, weight=spplist$weight, pres = spplist$pres, presyr = spplist$pres>0), by=list(spp=spplist$spp), FUN=sumna) # presyr col holds # years in which spp was present
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 416 spp
	
	sum(spplist$pres>1) # 311
	sum(spplist$pres>10) # 195
	max(spplist$presyr) # 42 yrs of data
	sum(spplist$presyr == max(spplist$presyr)) # 28 spp present every year

	spplist = spplist[spplist$presyr == max(spplist$presyr),] # take all spp present >= 1x per yr
		nrow(spplist) # 28 spp	

	spplist = merge(spplist, data[!duplicated(data$spp), c('common', 'spp')])
		dim(spplist)
		head(spplist)

# Remove spp not in spplist
	dim(data)
	data = data[data$spp %in% spplist$spp,]
	dim(data) # 60962

# Any spp duplicated in the same haul?
	i = duplicated(paste(data$haulid, data$spp))
	sum(i)

	j = data$haulid == data$haulid[i] & data$spp == data$spp[i]
	sum(j)
		
	k = duplicated(data)
	sum(k) # the whole row is not duplicated. appears to only be different at wtcpue and numcpue

# Add any missing hauls
	inds = which(!(goodhauls$haulid %in% data$haulid))
	length(inds) # 9 missing
		inds
		goodhauls[inds,]
	
	add = goodhauls[inds,]
		dim(add)
		add$region = unique(data$region)
		add$spp = spplist$spp[1] # just need to add something here, so first spp works
		add$common = spplist$common[1]
		add$wtcpue = 0
		add$numcpue = 0
		add$yearsurv = add$year
		add$juliansurv = add$julian
		add
		setdiff(names(add), names(data))
		setdiff(names(data), names(add))
	add = add[,names(data)]

	dim(data) 60962
	data = rbind(data, add)
	dim(data) 60971

	# sort by time again
	data = data[order(data$year, data$month, data$day, data$time, data$spp),]

# Fill in zeros
	fin = length(unique(data$haulid))*length(unique(data$spp)) # expected final size
		fin # 203448
	hauls = unique(data$haulid)
		length(hauls)
		nrow(goodhauls) # should match
		
	# set up sets of unique hauls and spp
	newdata = data[!duplicated(data$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'yearsurv', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'juliansurv', 'day')]
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
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datana = data[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(data$numcpue)] = -9999
		datana$wtcpue[is.na(data$wtcpue)] = -9999
	newdata = merge(newdata, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdata) # 203448
		summary(newdata$numcpue)
		summary(newdata$wtcpue)

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdata$numcpue[is.na(newdata$numcpue)] = 0
	newdata$wtcpue[is.na(newdata$wtcpue)] = 0
	newdata$numcpue[newdata$numcpue == -9999] = NA
	newdata$wtcpue[newdata$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdata[,c('haulid', 'spp')]))
	length(inds) # YES, if this is > 0
		#data[inds,]
	
		# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdata$haulid), by=list(spp=newdata$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdata$spp))
		length(inds) # should be 0
		#newdata[inds,]

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdata$month, newdata$year) # surveys don't cross Jan 1, so yearsurv == year
	newdata$yearsurv = newdata$year
	newdata$juliansurv = newdata$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year','yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')

	dim(newdata)
	length(nm)
	setdiff(nm, names(newdata))
	setdiff(names(newdata), nm)

	newdata = newdata[,nm]

# Fix row names
	row.names(newdata) = 1:nrow(newdata)

# Write out
	write.csv(newdata, paste('Output/datatrimwzeros_', Sys.Date(), '.csv', sep=''))

#######################
### Climate Envelope ##
#######################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Scotian Shelf')

## Read in data
load('../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

data = read.csv('Output/datatrimwzeros_2012-11-15.csv', row.names=1)
	dim(data) # 203,448 x 26
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	tab = aggregate(data$wtcpue>0, by=list(spp=data$spp, year = data$year), FUN=sum, na.rm=T)
	tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
	tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
	sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
	sppnames # These are spp to consider using: 17
	
	# Trim data to right spp
	datatrim = data[data$spp %in% sppnames,]
	dim(datatrim) # 123,522 x 26
			
## Add HadISST data (min and max temp)
	datatrim$mintemp = NA
	datatrim$mintempmnth = NA
	datatrim$maxtemp = NA
	datatrim$maxtempmnth = NA

	# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
	datatrim$latgrid = floor(datatrim$lat)+0.5
	datatrim$longrid = floor(datatrim$lon)+0.5
	inds = which(!duplicated(datatrim[,c('latgrid', 'longrid', 'year', 'month')]))
		length(inds) # 1487 to fit
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
	dim(datatrim) # 123,522 x 38

bm = aggregate(list(nummean=datatrim$numcpue), by=list(year=datatrim$year, spp=datatrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datatrim = merge(datatrim, bm, all.x=T)
	dim(datatrim) # 123,522 x 39

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
	dim(datatrim) # 123,522 x 41

## Write out for this species
	write.csv(datatrim, file=paste('Output/dataCEM_', Sys.Date(), '.csv', sep=''))


##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Scotian Shelf')

datatrim = read.csv('Output/dataCEM_2012-02-17.csv', row.names=1)
datatrim = read.csv('Output/datatrimwzeros_2012-11-15.csv', row.names=1)