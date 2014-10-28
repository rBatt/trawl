## How fast are species moving in NEFSC data?


#########################
## Trim to spr or fall ##
#########################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')
data = read.csv('../NEFSCTrawl_2011-10-12.csv')
temps = read.csv('../NEFSCspring_temp.csv', na.strings=c('.', '. ')) # spring trawl temperatures from Laurel Col
strata = read.csv('../NEFSCStrata.csv')

names(data)

# Separate spring and fall surveys
	unique(data$collectioncode)
	dataspr = data[data$collectioncode == "SPRING NMFS NEFSC BOTTOM TRAWL SURVEY",]
		dim(dataspr) # 168646
	datafal = data[data$collectioncode == "FALL NMFS NEFSC BOTTOM TRAWL SURVEY",]
		dim(datafal) # 213557
	rm(data)

# Create a unique haulid
	dataspr$cruise = substr(dataspr$fieldnumber, 1,6) # first get 6-digit cruise number (4-digit year and 2-digit cruise)
	datafal$cruise = substr(datafal$fieldnumber, 1,6)

	dataspr$tow = substr(dataspr$fieldnumber, 12,14) # then get 3-digit tow
	datafal$tow = substr(datafal$fieldnumber, 12,14)

	dataspr$haulid = paste(formatC(dataspr$cruise, width=6, flag=0), formatC(dataspr$locality, width=4, flag=0), formatC(dataspr$tow, width=3, flag=0), sep='-') # don't have vessel and station, so use cruise, stratum, and tow
	datafal$haulid = paste(formatC(datafal$cruise, width=6, flag=0), formatC(datafal$locality, width=4, flag=0), formatC(datafal$tow, width=3, flag=0), sep='-') # don't have vessel and station, so use cruise, stratum, and tow
	length(unique(dataspr$haulid)) # 14177
	length(unique(datafal$haulid)) # 15828
	
# Add temperature and depth records for spring (from Laurel Col), including missing tows
	names(dataspr)[names(dataspr)=='depth'] = 'depthOBIS' # makes sure it's not confused with temps$depth

	names(temps)[names(temps)=='SurfaceTemp'] = 'surftemp'
	names(temps)[names(temps)=='BottomTemp'] = 'bottemp'
	names(temps)[names(temps)=='SetDepth'] = 'depth'
	names(temps)[names(temps)=='Station'] = 'station'
	names(temps)[names(temps)=='Stratum'] = 'stratum'
	names(temps)[names(temps)=='Cruise'] = 'cruise'
	names(temps)[names(temps)=='Tow'] = 'tow'
	names(temps)[names(temps)=='Year'] = 'yearOracle' # I'd rather use OBIS date/times since in UTC
	names(temps)[names(temps)=='Month'] = 'monthOracle'
	names(temps)[names(temps)=='Day'] = 'dayOracle'
	names(temps)[names(temps)=='Lat'] = 'lat'
	names(temps)[names(temps)=='Lon'] = 'lon'
	temps = temps[temps$year<2009,] # trim to years that match dataspr
	temps$haulid = paste(temps$cruise, temps$stratum, formatC(temps$tow, width=3, flag=0), sep='-')
		summary(temps$surftemp)
		summary(temps$bottemp)
	setdiff(dataspr$haulid, temps$haulid) # nothing missing from temps!
	length(setdiff(temps$haulid, dataspr$haulid)) # 605 missing from dataspr
	intersect(names(dataspr), names(temps)) # which names will be used for matching?
	setdiff(names(temps), names(dataspr)) # which names will be added?
	
	dim(dataspr) # 168646
	dataspr2 = merge(dataspr, temps[,names(temps)[!(names(temps) %in% c('cruise', 'tow'))]], all.x=TRUE, all.y=TRUE) # merge on haulid and add tows missing from dataspr
	dim(dataspr2)
	nrow(dataspr2)-605 # 168646: matches earlier, good!
		sum(is.na(dataspr2$stratum)) # 0, good
		sum(dataspr2$yearcollected ==dataspr2$yearOracle, na.rm=T) # 168646: good
		sum(dataspr2$monthcollected ==dataspr2$monthOracle, na.rm=T) # 168006: some months must be off by one in UTC
		sum(dataspr2$daycollected ==dataspr2$dayOracle, na.rm=T) # 140865: some days must be off by one in UTC
			head(dataspr2[dataspr2$daycollected != dataspr2$dayOracle,]) # yes, that's right
			
	dataspr = dataspr2; rm(dataspr2)		
		
		# Turn NAs to 0 where sets not in catch data were added
		i = which(is.na(dataspr$tname) & is.na(dataspr$observedweight))
			length(i) # 605: matches haulids added from temps, good!
		dataspr$tname[i] = sort(unique(dataspr$tname))[1] # add a place-holder spp
		dataspr$observedweight[i] = 0
		dataspr$observedindividualcount[i] = 0
		table(dataspr$stratum[i], dataspr$yearOracle[i]) # new tows are scattered throughout


# Trim to high quality strata
	ts = table(dataspr$stratum, dataspr$yearcollected); ts
		tsr = rowSums(ts>0); as.numeric(tsr)
		tsc = colSums(ts>0); as.numeric(tsc)
		sskeep = rownames(ts[tsr==max(tsr),])  # strata to keep
		ssnye = unique(dataspr$locality[(dataspr$locality >=1010 & dataspr$locality <=1300) | (dataspr$locality >= 1360 & dataspr$locality <= 1400) | (dataspr$locality >=1610 & dataspr$locality <=1760)]) # compare to Nye et al. 2009
		sort(setdiff(sskeep, ssnye))
		sort(setdiff(ssnye, sskeep))
	tf = table(datafal$locality, datafal$yearcollected); tf
		tfr = rowSums(tf>0); as.numeric(tfr)
		tfc = colSums(tf>0); as.numeric(tfc)
		sfkeep = rownames(tf[tfr==max(tfr),]) # strata to keep in the fall

	dataspr = dataspr[dataspr$locality %in% sskeep, ]
		dim(dataspr) # 106529
	datafal = datafal[datafal$locality %in% sfkeep,]
		dim(datafal) # 115354

# Years when surveys available
	table(dataspr$yearcollected, dataspr$monthcollected)
		# spring starts 1968
	table(datafal$yearcollected, datafal$monthcollected)
		# fall starts 1963

# Reasonable depths? 
	hist(dataspr$depth) # spring is all less than 450
	hist(datafal$depth) # fall is mostly less than 400
	range(dataspr$depth, na.rm=T) # 12-470: all deep tows in 1340 in northeast corner
	range(datafal$depth, na.rm=T) # 21-3304: all deep tows in 1340 in northeast corner
	
# Fix column names
	names(dataspr)[names(dataspr)=='yearcollected'] = 'year'
	names(dataspr)[names(dataspr)=='monthcollected'] = 'month'
	names(dataspr)[names(dataspr)=='daycollected'] = 'day'
	names(dataspr)[names(dataspr)=='salinity'] = 'botsal' # assume this is right?
	names(dataspr)[names(dataspr)=='tname'] = 'spp'
	names(dataspr)[names(dataspr)=='observedweight'] = 'wtcpue'
	names(dataspr)[names(dataspr)=='observedindividualcount'] = 'numcpue'

	names(datafal)[names(datafal)=='locality'] = 'stratum'
	names(datafal)[names(datafal)=='yearcollected'] = 'year'
	names(datafal)[names(datafal)=='monthcollected'] = 'month'
	names(datafal)[names(datafal)=='daycollected'] = 'day'
	names(datafal)[names(datafal)=='latitude'] = 'lat'
	names(datafal)[names(datafal)=='longitude'] = 'lon'
	names(datafal)[names(datafal)=='salinity'] = 'botsal' # assume this is right?
	names(datafal)[names(datafal)=='tname'] = 'spp'
	names(datafal)[names(datafal)=='observedweight'] = 'wtcpue'
	names(datafal)[names(datafal)=='observedindividualcount'] = 'numcpue'

	names(dataspr)
	names(datafal)

# Turn -9999 to NA
	summary(dataspr$numcpue)
	summary(dataspr$wtcpue)
		min(dataspr$wtcpue[dataspr$wtcpue>0], na.rm=T)

	summary(datafal$numcpue)
	summary(datafal$wtcpue)

	i = which(dataspr$wtcpue==0 & dataspr$numcpue>0) # rounding errors in wtcpue
	length(i)
	unique(dataspr$spp[i])
	sort(unique(dataspr$year[i])) # even 2008 entries

# Add blank columns that are missing in this region
	dataspr$svvessel = NA
	dataspr$common = NA
	dataspr$surfsal = NA # leave station out of dataspr, we'll add it from temps

	datafal$svvessel = NA
	datafal$station = NA
	datafal$common = NA
	datafal$surftemp = NA
	datafal$bottemp = NA
	datafal$surfsal = NA

# Add time (already in UTC) and julian day
	dataspr$time = substr(dataspr$datecollected, 12,16)
	datafal$time = substr(datafal$datecollected, 12,16)

	require(date)
	dataspr$julian = as.numeric(as.date(paste(dataspr$month, dataspr$day, dataspr$year, sep='/')))-as.numeric(as.date(paste('01/01/', dataspr$year, sep=''))) # julian day since Jan 1

	datafal$julian = as.numeric(as.date(paste(datafal$month, datafal$day, datafal$year, sep='/')))-as.numeric(as.date(paste('01/01/', datafal$year, sep=''))) # julian day since Jan 1

# Add strata areas
	names(strata)[names(strata)=='StratumCode'] = 'stratum'
	strata$stratarea = strata$Areanmi2*(1.852^2)
	sort(setdiff(unique(dataspr$stratum), unique(strata$stratum)))
	dim(dataspr)
	dataspr = merge(dataspr, strata[,c('stratum', 'stratarea')])
	dim(dataspr) # 106529

	sort(setdiff(unique(datafal$stratum), unique(strata$stratum)))
	dim(datafal)
	datafal = merge(datafal, strata[,c('stratum', 'stratarea')])
	dim(datafal) # 115354

# Remove records without lat/long?
	i = is.na(dataspr$lon) | is.na(dataspr$lat)
	sum(i) # should be 0

	i = is.na(datafal$lon) | is.na(datafal$lat)
	sum(i) # should be 0

# Find duplicate tows in same location
	# Spring
	# first sort by time so first index is always the earliest
	dataspr = dataspr[order(dataspr$year, dataspr$month, dataspr$day, dataspr$time, dataspr$spp),]
	
	# Any completely duplicated rows?
	dups = which(duplicated(dataspr))
		sum(dups) # 0

	# find tows that have same lat/lon but different haulid
	inds = which(duplicated(dataspr[,c('year', 'lat', 'lon')]) & !duplicated(dataspr$haulid))
		length(inds) # 1

	# list of rows to drop (those that are duplicated)
	droprows = numeric(0)
	idsdrops = numeric(0) # haulids to drop

	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdataspr = dataspr

	# Trim some unneeded columns from newdata (t_bottom has same bottom temps, depth has mean of depthst and depthend)
	nm = c('svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	newdataspr = newdataspr[,nm]
		dim(newdataspr) # 106529

	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# Find the best entry (least missing data or earliest) and delete the duplicate tows
	for(i in 1:length(inds)){
		if(i %% 10 == 0) print(i)
	
		inds2 = which(dataspr$year == dataspr$year[inds[i]] & dataspr$lat == dataspr$lat[inds[i]] & dataspr$lon == dataspr$lon[inds[i]])
		ids = sort(unique(dataspr$haulid[inds2])) # find the haulids 
		dataspr[inds2,]
			
		j0 = aggregate(list(surftemp = !is.na(dataspr$surftemp[inds2]), bottemp = !is.na(dataspr$bottemp[inds2])), by=list(haulid = dataspr$haulid[inds2]), FUN=sum) # how many complete entries per haulid?
		j0$tot = (j0$surftemp>0) + (j0$bottemp>0) # how many complete variables per haulid?
		j0 = merge(j0, dataspr[!duplicated(dataspr$haulid) & dataspr$haulid %in% ids, c('haulid', 'month', 'day', 'time')])
		j0 = j0[order(j0$month, j0$day, j0$time),] # order by time
		idskeep = j0$haulid[which(j0$tot == max(j0$tot))[1]] # pick the first haulid with the most complete entries
		idsdrop = ids[!(ids %in% idskeep)] # haulids to drop
		idsdrops = c(idsdrops, idsdrop)

		if(idskeep %in% idsdrops) stop('really, you want to keep this haulid? you wanted to drop it before') # make sure the haulid we like wasn't previously marked for removal
			
			# mark all rows in newdata with the other haulid for removal
		droprows = c(droprows, which(dataspr$haulid %in% idsdrop))		
					
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values

	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdataspr$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows, and the tows I'm eliminating
	newdataspr = newdataspr[-droprows,]

	dim(dataspr)
	dim(newdataspr)
	length(droprows) + nrow(newdataspr) # should match nrow(data)


	# Fall
	# first sort by time so first index is always the earliest
	datafal = datafal[order(datafal$year, datafal$month, datafal$day, datafal$time, datafal$spp),]
	
	# Any completely duplicated rows?
	dups = which(duplicated(datafal))
		sum(dups) # 0

	# find tows that have same lat/lon but different haulid
	inds = which(duplicated(datafal[,c('year', 'lat', 'lon')]) & !duplicated(datafal$haulid))
		length(inds) # 2

	# list of rows to drop (those that are duplicated)
	droprows = numeric(0)
	idsdrops = numeric(0) # haulids to drop

	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdatafal = datafal

	# Trim some unneeded columns from newdata (t_bottom has same bottom temps, depth has mean of depthst and depthend)
	nm = c('svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	newdatafal = newdatafal[,nm]
		dim(newdatafal) # 115354

	# Find the best entry (least missing data or earliest) and delete the duplicate tows
	for(i in 1:length(inds)){
		if(i %% 10 == 0) print(i)
	
		inds2 = which(datafal$year == datafal$year[inds[i]] & datafal$lat == datafal$lat[inds[i]] & datafal$lon == datafal$lon[inds[i]])
		ids = sort(unique(datafal$haulid[inds2])) # find the haulids 
		datafal[inds2,]
			
		j0 = aggregate(list(surftemp = !is.na(datafal$surftemp[inds2]), bottemp = !is.na(datafal$bottemp[inds2])), by=list(haulid = datafal$haulid[inds2]), FUN=sum) # how many complete entries per haulid?
		j0$tot = (j0$surftemp>0) + (j0$bottemp>0) # how many complete variables per haulid?
		j0 = merge(j0, datafal[!duplicated(datafal$haulid) & datafal$haulid %in% ids, c('haulid', 'month', 'day', 'time')])
		j0 = j0[order(j0$month, j0$day, j0$time),] # order by time
		idskeep = j0$haulid[which(j0$tot == max(j0$tot))[1]] # pick the first haulid with the most complete entries
		idsdrop = ids[!(ids %in% idskeep)] # haulids to drop
		idsdrops = c(idsdrops, idsdrop)

		if(idskeep %in% idsdrops) stop('really, you want to keep this haulid? you wanted to drop it before') # make sure the haulid we like wasn't previously marked for removal
			
			# mark all rows in newdata with the other haulid for removal
		droprows = c(droprows, which(datafal$haulid %in% idsdrop))		
					
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values

	# make sure no two tows were combined in the same row. they'll have a comma
	i = grep(',', newdatafal$haulid)
	length(i) # should be zero
	
	# drop the duplicated rows, and the tows I'm eliminating
	newdatafal = newdatafal[-droprows,]

	dim(datafal) # 115354
	dim(newdatafal)
	length(droprows) + nrow(newdatafal) # should match nrow(data)

# Create list of all hauls
	goodhaulsspr = newdataspr[!duplicated(newdataspr$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhaulsspr) # 8785
	head(goodhaulsspr)

	goodhaulsfal = newdatafal[!duplicated(newdatafal$haulid), c('svvessel', 'cruise', 'tow', 'haulid', 'year', 'month', 'day', 'julian', 'time', 'station', 'stratum', 'stratarea', 'lat', 'lon', 'depth', 'bottemp', 'surftemp', 'botsal', 'surfsal')]
	nrow(goodhaulsfal) # 8790
	head(goodhaulsfal)

# Adjust spp names for those cases where they've changed
	# don't know of any that should be combined or changed
	
# Remove spp without a scientific name
	i = newdataspr$spp == ''
	sum(i) # 0 

	i = newdatafal$spp == ''
	sum(i) # 0 
	
# Find duplicate rows (repeat rows for the same tow), and combine entries from spp that have now been consolidated. Already checked for duplicate tows in same location (see above)
	# first sort by time so first index is always the earliest
	newdataspr = newdataspr[order(newdataspr$year, newdataspr$month, newdataspr$day, newdataspr$time, newdataspr$spp),]
	newdatafal = newdatafal[order(newdatafal$year, newdatafal$month, newdatafal$day, newdatafal$time, newdatafal$spp),]

	# Any completely duplicated rows?
	# Yes, there are some: different only by sex, but I dropped that column earlier. Now need to add them together.

	# Duplicated rows for the same tow
	inds = which(duplicated(newdataspr[, c('spp', 'year', 'haulid')]))
	length(inds) # 4457

	indf = which(duplicated(newdatafal[, c('spp', 'year', 'haulid')]))
	#indf = which(duplicated(newdatafal[, c('spp', 'year', 'lat', 'lon', 'sex')]))
	length(indf) # 4611

	# new copy of data so that averaging duplicate rows won't modify the original copy
	newdataspr2 = newdataspr
	newdatafal2 = newdatafal

	# Trim some unneeded columns from newdata (t_bottom has same bottom temps, depth has mean of depthst and depthend)
	nm = c('svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'month', 'day', 'julian', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')

	newdataspr2 = newdataspr2[,nm]
	newdatafal2 = newdatafal2[,nm]

	# Rows to drop
	droprows = numeric(0)
	droprowf = numeric(0)
	
	# Spring
	for(i in 1:length(inds)){
		if(i %% 100 == 0) print(i)
	
		inds2 = which(newdataspr$spp == newdataspr$spp[inds[i]] & newdataspr$year == newdataspr$year[inds[i]] & newdataspr$haulid == newdataspr$haulid[inds[i]])
		newdataspr[inds2,]

		# put sums in the first row if they're from the same tow
		temp = data.frame(
		numcpue = sumna(newdataspr$numcpue[inds2]), 
		wtcpue = sumna(newdataspr$wtcpue[inds2]))	

		newdataspr2$numcpue[inds2[1]] = temp$numcpue
		newdataspr2$wtcpue[inds2[1]] = temp$wtcpue

		# mark the following row(s) for removal
		droprows = c(droprows, inds2[2:length(inds2)])
	}
	droprows = sort(unique(droprows)) # trim droprows to just the unique values
	length(droprows)
	length(inds)
		
	# drop the duplicated rows
	dim(newdataspr2)
	newdataspr2 = newdataspr2[-droprows,]
	dim(newdataspr2)

	dim(newdataspr)
	dim(newdataspr2)
	length(droprows)
	length(droprows) + nrow(newdataspr2) # should match nrow(dataspr)
	
	# Fall
	for(i in 1:length(indf)){
		if(i %% 100 == 0) print(i)
	
		indf2 = which(newdatafal$spp == newdatafal$spp[indf[i]] & newdatafal$year == newdatafal$year[indf[i]] & newdatafal$haulid == newdatafal$haulid[indf[i]])
		newdatafal[indf2,]

		# put sums in the first row if they're from the same tow
		temp = data.frame(
		numcpue = sumna(newdatafal$numcpue[indf2]), 
		wtcpue = sumna(newdatafal$wtcpue[indf2]))	

		newdatafal2$numcpue[indf2[1]] = temp$numcpue
		newdatafal2$wtcpue[indf2[1]] = temp$wtcpue

		# mark the following row(s) for removal
		droprowf = c(droprowf, indf2[2:length(indf2)])			
	}
	droprowf = sort(unique(droprowf)) # trim droprowf to just the unique values
	length(droprowf)
	length(indf)
		
	# drop the duplicated rows
	dim(newdatafal2)
	newdatafal2 = newdatafal2[-droprowf,]
	dim(newdatafal2)

	dim(newdatafal)
	dim(newdatafal2)
	length(droprowf)
	length(droprowf) + nrow(newdatafal2) # should match nrow(datafal)
	
# How many tows?
	length(unique(paste(newdataspr2$year, newdataspr2$lat, newdataspr2$lon))) # 8785 unique locations
	length(unique(newdataspr2$haulid)) # 8785: good that this matches # locations

	length(unique(paste(newdatafal2$year, newdatafal2$lat, newdatafal2$lon))) # 8790 unique locations
	length(unique(newdatafal2$haulid)) # 8790: good that this matches # locations

# How many spp?
	length(unique(newdataspr2$spp)) #284
	length(unique(newdatafal2$spp)) #317

# Add a region column
newdataspr2$region = "NEFSC_Spring"
newdatafal2$region = "NEFSC_Fall"

# How many hauls missing from data?
	setdiff(goodhaulsspr$haulid, unique(newdataspr2$haulid)) # lose 0 hauls
	setdiff(goodhaulsfal$haulid, unique(newdatafal2$haulid)) # lose 0 hauls

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdataspr2$month, newdataspr2$year) # surveys don't cross Jan 1, so yearsurv == year
	newdataspr2$yearsurv = newdataspr2$year
	newdataspr2$juliansurv = newdataspr2$julian	

	table(newdatafal2$month, newdatafal2$year) # surveys don't cross Jan 1, so yearsurv == year
	newdatafal2$yearsurv = newdatafal2$year
	newdatafal2$juliansurv = newdatafal2$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')
	ncol(newdataspr2)
	ncol(newdatafal2)
	length(nm)
	setdiff(nm, names(newdataspr2))
	setdiff(names(newdataspr2), nm)

	setdiff(nm, names(newdatafal2))
	setdiff(names(newdatafal2), nm)

newdataspr2out = newdataspr2[,nm]
	dim(newdataspr2)
	dim(newdataspr2out) # 102,058 x 26

newdatafal2out = newdatafal2[,nm]
	dim(newdatafal2)
	dim(newdatafal2out) # 110,718 x 26

# Write out
	write.csv(newdataspr2out, paste('../Output/dataspr_', Sys.Date(), '.csv', sep=''))
	write.csv(newdatafal2out, paste('../Output/datafal_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhaulsspr, paste('../Output/goodhaulsspr_', Sys.Date(), '.csv', sep=''))
	write.csv(goodhaulsfal, paste('../Output/goodhaulsfal_', Sys.Date(), '.csv', sep=''))

#########################################	
## Trim to spp with data and add zeros ##
#########################################	
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range shifts')

dataspr = read.csv('../Output/dataspr_2012-04-02.csv', row.names=1)
datafal = read.csv('../Output/datafal_2012-04-02.csv', row.names=1)
goodhaulsspr = read.csv('../Output/goodhaulsspr_2012-04-02.csv', row.names=1)
goodhaulsfal = read.csv('../Output/goodhaulsfal_2012-04-02.csv', row.names=1)

# Identify the spp to analyze
	# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	# Spring
	spplistspr = aggregate(list(count=dataspr$numcpue, weight = dataspr$wtcpue, pres = dataspr$wtcpue>0), by=list(spp=dataspr$spp, year=dataspr$year), FUN=sumna)
	spplistspr = aggregate(list(count=spplistspr$count, weight=spplistspr$weight, pres = spplistspr$pres, presyr = spplistspr$pres>0), by=list(spp=spplistspr$spp), FUN=sum) # presyr col holds # years in which spp was present
	rownames(spplistspr) = 1:nrow(spplistspr)
	nrow(spplistspr) # 284 spp
	
	sum(spplistspr$pres>1) # 190
	sum(spplistspr$pres>10) # 124
	max(spplistspr$presyr) # 41 yrs of data
	sum(spplistspr$presyr == max(spplistspr$presyr)) # 41 spp present every year

	spplistspr = spplistspr[spplistspr$presyr == max(spplistspr$presyr),] # take all spp present >= 1x per yr
		nrow(spplistspr) # 41 spp	

	spplistspr = merge(spplistspr, dataspr[!duplicated(dataspr$spp), c('common', 'spp')])
		dim(spplistspr)
		head(spplistspr)

	# Fall
	spplistfal = aggregate(list(count=datafal$numcpue, weight = datafal$wtcpue, pres = datafal$wtcpue>0), by=list(spp=datafal$spp, year=datafal$year), FUN=sumna)
	spplistfal = aggregate(list(count=spplistfal$count, weight=spplistfal$weight, pres = spplistfal$pres, presyr = spplistfal$pres>0), by=list(spp=spplistfal$spp), FUN=sumna) # presyr col holds # years in which spp was present
	rownames(spplistfal) = 1:nrow(spplistfal)
	nrow(spplistfal) # 317 spp
	
	sum(spplistfal$pres>1) # 215
	sum(spplistfal$pres>10) # 125
	max(spplistfal$presyr) # 46 yrs of data
	sum(spplistfal$presyr == max(spplistfal$presyr)) # 34 spp present every year

	spplistfal = spplistfal[spplistfal$presyr == max(spplistfal$presyr),] # take all spp present >= 1x per yr
		nrow(spplistfal) # 34 spp	

	spplistfal = merge(spplistfal, datafal[!duplicated(datafal$spp), c('common', 'spp')])
		dim(spplistfal)
		head(spplistfal)

# Remove spp not in spplist
	dim(dataspr)
	dataspr = dataspr[dataspr$spp %in% spplistspr$spp,]
	dim(dataspr) # 84530

	dim(datafal)
	datafal = datafal[datafal$spp %in% spplistfal$spp,]
	dim(datafal) # 74564

# Any spp duplicated in the same haul?
	i = duplicated(paste(dataspr$haulid, dataspr$spp))
	sum(i)

	j = dataspr$haulid == dataspr$haulid[i] & dataspr$spp == dataspr$spp[i]
	sum(j)
		
	k = duplicated(dataspr)
	sum(k) # the whole row is not duplicated. appears to only be different at wtcpue and numcpue

	i = duplicated(paste(datafal$haulid, datafal$spp))
	sum(i)

	j = datafal$haulid == datafal$haulid[i] & datafal$spp == datafal$spp[i]
	sum(j)
		
	k = duplicated(datafal)
	sum(k) # the whole row is not duplicated. appears to only be different at wtcpue and numcpue

# Add any missing hauls
	# spring
	inds = which(!(goodhaulsspr$haulid %in% dataspr$haulid))
	length(inds) # 4 missing
		inds
		goodhaulsspr[inds,]
	
	add = goodhaulsspr[inds,]
		dim(add)
		add$region = unique(dataspr$region)
		add$spp = spplistspr$spp[1] # just need to add something here, so first spp works
		add$common = spplistspr$common[1]
		add$wtcpue = 0
		add$numcpue = 0
	add = add[,names(dataspr)]

	dim(dataspr)
	dataspr = rbind(dataspr, add)
	dim(dataspr)

	# fall
	inds = which(!(goodhaulsfal$haulid %in% datafal$haulid))
	length(inds) # 47 missing
		inds
		goodhaulsfal[inds,]
	
	add = goodhaulsfal[inds,]
		dim(add)
		add$region = unique(datafal$region)
		add$spp = spplistfal$spp[1] # just need to add something here, so first spp works
		add$common = spplistfal$common[1]
		add$wtcpue = 0
		add$numcpue = 0
	add = add[,names(datafal)]

	dim(datafal)
	datafal = rbind(datafal, add)
	dim(datafal)

# Fill in zeros
	# Spring
	fin = length(unique(dataspr$haulid))*length(unique(dataspr$spp)) # expected final size
		fin
	hauls = unique(dataspr$haulid)
		length(hauls)
		nrow(goodhaulsspr) # should match
		
	# set up sets of unique hauls and spp
	newdataspr = dataspr[!duplicated(dataspr$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'day')]
		dim(newdataspr)
		newdataspr = newdataspr[order(newdataspr$haulid),] # sort by haulid
		rownames(newdataspr) = 1:nrow(newdataspr)
	spps = dataspr[!duplicated(dataspr$spp), c('spp', 'common')]
		dim(spps)
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdataspr = newdataspr[rep(rownames(newdataspr), rep(nrow(spps), nrow(newdataspr))),]
		dim(newdataspr)
			
		# add spp info, replicated so that each haul gets a species
	newdataspr = cbind(newdataspr, spps[rep(rownames(spps), length(unique(newdataspr$haulid))),])
		dim(newdataspr)
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datana = dataspr[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(dataspr$numcpue)] = -9999
		datana$wtcpue[is.na(dataspr$wtcpue)] = -9999
	newdataspr = merge(newdataspr, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdataspr) # 360185
		summary(newdataspr$numcpue)
		summary(newdataspr$wtcpue)

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdataspr$numcpue[is.na(newdataspr$numcpue)] = 0
	newdataspr$wtcpue[is.na(newdataspr$wtcpue)] = 0
	newdataspr$numcpue[newdataspr$numcpue == -9999] = NA
	newdataspr$wtcpue[newdataspr$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdataspr[,c('haulid', 'spp')]))
	length(inds) # YES, if this is > 0

	#data[inds,]
	
		# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdataspr$haulid), by=list(spp=newdataspr$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdataspr$spp))
		length(inds) # should be 0
		#newdata[inds,]
		
		#newdata = newdata[-inds,]
		#dim(newdata)

		# sort by time so first index is always the earliest
		newdataspr = newdataspr[order(newdataspr$year, newdataspr$month, newdataspr$day, newdataspr$time, newdataspr$spp),]

	# Fall
	fin = length(unique(datafal$haulid))*length(unique(datafal$spp)) # expected final size
		fin
	hauls = unique(datafal$haulid)
		length(hauls)
		nrow(goodhaulsfal) # should match
		
	# set up sets of unique hauls and spp
	newdatafal = datafal[!duplicated(datafal$haulid), c('region', 'lat', 'lon', 'station', 'stratum', 'stratarea', 'year', 'time', 'depth', 'bottemp', 'surftemp', 'surfsal', 'botsal',  'svvessel', 'cruise', 'tow', 'month', 'haulid', 'julian', 'day')]
		dim(newdatafal)
		newdatafal = newdatafal[order(newdatafal$haulid),] # sort by haulid
		rownames(newdatafal) = 1:nrow(newdatafal)
	spps = datafal[!duplicated(datafal$spp), c('spp', 'common')]
		dim(spps)
		spps = spps[order(spps$spp),]
		rownames(spps) = 1:nrow(spps)
		
	# expand so that each spp can get a haul
	newdatafal = newdatafal[rep(rownames(newdatafal), rep(nrow(spps), nrow(newdatafal))),]
		dim(newdatafal)
			
		# add spp info, replicated so that each haul gets a species
	newdatafal = cbind(newdatafal, spps[rep(rownames(spps), length(unique(newdatafal$haulid))),])
		dim(newdatafal)
		
		# add catch info. set NAs to -9999 so they're not confused with an absence
		datana = datafal[,c('spp', 'haulid', 'numcpue', 'wtcpue')]
		datana$numcpue[is.na(datafal$numcpue)] = -9999
		datana$wtcpue[is.na(datafal$wtcpue)] = -9999
	newdatafal = merge(newdatafal, datana[,c('spp', 'haulid', 'numcpue', 'wtcpue')], all.x=TRUE, by=c('spp', 'haulid'))
		dim(newdatafal) # 298860
		summary(newdatafal$numcpue)
		summary(newdatafal$wtcpue)

		# set catch NAs to zero (absences) and -9999 to NA (true missing data)
	newdatafal$numcpue[is.na(newdatafal$numcpue)] = 0
	newdatafal$wtcpue[is.na(newdatafal$wtcpue)] = 0
	newdatafal$numcpue[newdatafal$numcpue == -9999] = NA
	newdatafal$wtcpue[newdatafal$wtcpue == -9999] = NA

	# If dim(data) and fin don't match, maybe there's a duplicate entry
	inds = which(duplicated(newdatafal[,c('haulid', 'spp')]))
	length(inds) # YES, if this is > 0

	#data[inds,]
	
		# How many hauls per spp?
		lunique = function(x) return(length(unique(x)))
		x=aggregate(list(nhauls=newdatafal$haulid), by=list(spp=newdatafal$spp), FUN=lunique)
			head(x)
		which(x$nhauls != max(x$nhauls)) # should be 0 spp that don't have all their hauls
		
		# A blank row?
		inds = which(is.na(newdatafal$spp))
		length(inds) # should be 0
		#newdata[inds,]
		
		#newdata = newdata[-inds,]
		#dim(newdata)

		# sort by time so first index is always the earliest
		newdatafal = newdatafal[order(newdatafal$year, newdatafal$month, newdatafal$day, newdatafal$time, newdatafal$spp),]

# Add yearsurv and juliansurv for compatibility with Newfoundland
	table(newdataspr$month, newdataspr$year) # surveys don't cross Jan 1, so yearsurv == year
	newdataspr$yearsurv = newdataspr$year
	newdataspr$juliansurv = newdataspr$julian	

	table(newdatafal$month, newdatafal$year) # surveys don't cross Jan 1, so yearsurv == year
	newdatafal$yearsurv = newdatafal$year
	newdatafal$juliansurv = newdatafal$julian	

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'stratarea', 'tow', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'spp', 'common', 'wtcpue', 'numcpue')

	# Spring
	dim(newdataspr)
	length(nm)
	setdiff(nm, names(newdataspr))
	setdiff(names(newdataspr), nm)

	newdataspr = newdataspr[,nm]

	# Fall
	dim(newdatafal)
	length(nm)
	setdiff(nm, names(newdatafal))
	setdiff(names(newdatafal), nm)

	newdatafal = newdatafal[,nm]

# Fix row names
	row.names(newdataspr) = 1:nrow(newdataspr)
	row.names(newdatafal) = 1:nrow(newdatafal)

# Write out
	write.csv(newdataspr, paste('../Output/datatrimwzerosspr_', Sys.Date(), '.csv', sep=''))
	write.csv(newdatafal, paste('../Output/datatrimwzerosfal_', Sys.Date(), '.csv', sep=''))
	

#######################
### Climate Envelope ##
#######################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range Shifts')

## Read in data
load('../../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

dataspr = read.csv('../Output/datatrimwzerosspr_2012-07-23.csv', row.names=1)
	dim(dataspr) # 360,185 x 26
datafal = read.csv('../Output/datatrimwzerosfal_2012-07-23.csv', row.names=1)
	dim(datafal) # 298,860 x 26
	
## Figure out which names we need to use and trim the data
# only keep spp that are caught > 10 times/yr
	# SPRING
		tab = aggregate(dataspr$wtcpue>0, by=list(spp=dataspr$spp, year = dataspr$year), FUN=sum, na.rm=T)
		tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
		tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
		sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
		sppnames # These are spp to consider using: 26
		
		# Trim data to right spp
		datasprtrim = dataspr[dataspr$spp %in% sppnames,]
		dim(datasprtrim) # 228,410

	# FALL
		tab = aggregate(datafal$wtcpue>0, by=list(spp=datafal$spp, year = datafal$year), FUN=sum, na.rm=T)
		tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
		tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
		sppnames = tab2$spp[tabsum == (ncol(tab2)-1)] # only keep where the spp meets this criterion in every year
		sppnames # These are spp to consider using: 20
		
		# Trim data to right spp
		datafaltrim = datafal[datafal$spp %in% sppnames,]
		dim(datafaltrim) # 175,800
			
## Add HadISST data (min and max temp)
	# SPRING
		datasprtrim$mintemp = NA
		datasprtrim$mintempmnth = NA
		datasprtrim$maxtemp = NA
		datasprtrim$maxtempmnth = NA
	
		# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
		datasprtrim$latgrid = floor(datasprtrim$lat)+0.5
		datasprtrim$longrid = floor(datasprtrim$lon)+0.5
		inds = which(!duplicated(datasprtrim[,c('latgrid', 'longrid', 'year', 'month')]))
			length(inds) # 1663 combinations to process
		for(i in 1:length(inds)){
			if(i %% 50 == 0) print(i)
			lat = as.character(datasprtrim$latgrid[inds[i]]) # to match hadisst grid
			long = as.character(datasprtrim$longrid[inds[i]]) # to match hadisst grid
			yr = as.character(datasprtrim$year[inds[i]])
			lastyr = as.character(as.numeric(yr)-1)
			thesemons = as.character(1:datasprtrim$month[inds[i]]) # months in this year, converted to char
			lastmons = as.character((datasprtrim$month[inds[i]]+1):12) # months we want from last year
	
			j = datasprtrim$latgrid == datasprtrim$latgrid[inds[i]] & datasprtrim$longrid == datasprtrim$longrid[inds[i]] & datasprtrim$year == datasprtrim$year[inds[i]] & datasprtrim$month == datasprtrim$month[inds[i]]
	
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
		datafaltrim$longrid = floor(datafaltrim$lon)+0.5
		inds = which(!duplicated(datafaltrim[,c('latgrid', 'longrid', 'year', 'month')]))
			length(inds) # 1609 combinations to process
		for(i in 1:length(inds)){
			if(i %% 50 == 0) print(i)
			lat = as.character(datafaltrim$latgrid[inds[i]]) # to match hadisst grid
			long = as.character(datafaltrim$longrid[inds[i]]) # to match hadisst grid
			yr = as.character(datafaltrim$year[inds[i]])
			lastyr = as.character(as.numeric(yr)-1)
			thesemons = as.character(1:datafaltrim$month[inds[i]]) # months in this year, converted to char
			if(max(thesemons)<12){
				lastmons = as.character((datafaltrim$month[inds[i]]+1):12) # months we want from last year
			} else {
				lastmons = character(0)
			}


			j = datafaltrim$latgrid == datafaltrim$latgrid[inds[i]] & datafaltrim$longrid == datafaltrim$longrid[inds[i]] & datafaltrim$year == datafaltrim$year[inds[i]] & datafaltrim$month == datafaltrim$month[inds[i]]
	
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
bm = aggregate(list(biomassmean=datasprtrim$wtcpue), by=list(year=datasprtrim$year, spp=datasprtrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datasprtrim = merge(datasprtrim, bm, all.x=T)
	dim(datasprtrim) # 228,410 x 38

bm = aggregate(list(nummean=datasprtrim$numcpue), by=list(year=datasprtrim$year, spp=datasprtrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datasprtrim = merge(datasprtrim, bm, all.x=T)
	dim(datasprtrim) # 228,410 x 39

bm = aggregate(list(biomassmean=datafaltrim$wtcpue), by=list(year=datafaltrim$year, spp=datafaltrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datafaltrim = merge(datafaltrim, bm, all.x=T)
	dim(datafaltrim) # 175,800 x 38

bm = aggregate(list(nummean=datafaltrim$numcpue), by=list(year=datafaltrim$year, spp=datafaltrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datafaltrim = merge(datafaltrim, bm, all.x=T)
	dim(datafaltrim) # 175,800 x 39

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


# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datasprtrim)
	length(nm)
	
	setdiff(nm, names(datasprtrim))
	setdiff(names(datasprtrim), nm) # will drop common and stratarea
	 	 
	datasprtrim = datasprtrim[,nm]
		dim(datasprtrim) # 228,410 x 41

	setdiff(nm, names(datafaltrim))
	setdiff(names(datafaltrim), nm) # will drop common and stratarea
	 	 
	datafaltrim = datafaltrim[,nm]
		dim(datafaltrim) # 175,800 x 41

## Write out for this species
	write.csv(datasprtrim, file=paste('../Output/datasprCEM_', Sys.Date(), '.csv', sep=''))
	write.csv(datafaltrim, file=paste('../Output/datafalCEM_', Sys.Date(), '.csv', sep=''))


##################
## Examine data ##
##################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/DFO Southern Gulf')

datasprtrim = read.csv('Output/datasprCEM_2012-07-23.csv', row.names=1)
datafaltrim = read.csv('Output/datafalCEM_2012-07-23.csv', row.names=1)

	
	
###########################################
### Climate Envelope for Summer Flounder ##
###########################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/NEFSC/Range Shifts')

## Read in data
load('../../HadISST 1.1/Output/hadisst_2012-07-02.RData') # load SST by month

dataspr = read.csv('../Output/datatrimwzerosspr_2012-07-23.csv', row.names=1)
	dim(dataspr) # 360,185 x 26

## Figure out which names we need to use and trim the data
	# SPRING
		tab = aggregate(dataspr$wtcpue>0, by=list(spp=dataspr$spp, year = dataspr$year), FUN=sum, na.rm=T)
		tab2 = reshape(tab, direction='wide', timevar='year', idvar='spp')
		tabsum = rowSums(tab2[2:ncol(tab2)]>10, na.rm=T) # number of years where a spp was caught > 10 times
		tab2[tab2$spp == 'Paralichthys dentatus',] # was rare in 1969 and 1970

		# Trim data to right spp
		datasprtrim = dataspr[dataspr$spp %in% 'Paralichthys dentatus',]
		dim(datasprtrim) # 8785

			
## Add HadISST data (min and max temp)
	# SPRING
		datasprtrim$mintemp = NA
		datasprtrim$mintempmnth = NA
		datasprtrim$maxtemp = NA
		datasprtrim$maxtempmnth = NA
	
		# Find the unique combinations of lat cell/long cell/year/month for matching to HadISST (rather than doing each row individaully = very long
		datasprtrim$latgrid = floor(datasprtrim$lat)+0.5
		datasprtrim$longrid = floor(datasprtrim$lon)+0.5
		inds = which(!duplicated(datasprtrim[,c('latgrid', 'longrid', 'year', 'month')]))
			length(inds) # 1663 combinations to process
		for(i in 1:length(inds)){
			if(i %% 50 == 0) print(i)
			lat = as.character(datasprtrim$latgrid[inds[i]]) # to match hadisst grid
			long = as.character(datasprtrim$longrid[inds[i]]) # to match hadisst grid
			yr = as.character(datasprtrim$year[inds[i]])
			lastyr = as.character(as.numeric(yr)-1)
			thesemons = as.character(1:datasprtrim$month[inds[i]]) # months in this year, converted to char
			lastmons = as.character((datasprtrim$month[inds[i]]+1):12) # months we want from last year
	
			j = datasprtrim$latgrid == datasprtrim$latgrid[inds[i]] & datasprtrim$longrid == datasprtrim$longrid[inds[i]] & datasprtrim$year == datasprtrim$year[inds[i]] & datasprtrim$month == datasprtrim$month[inds[i]]
	
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


# Add a date and a time column (rather than DATETIME)
datasprtrim$date = paste(formatC(datasprtrim$month, width=2, flag=0), '/', formatC(datasprtrim$day, width=2, flag=0), '/', datasprtrim$year, sep='')

# Transform data
	datasprtrim$lsurftemp = log(datasprtrim$surftemp+1)
	datasprtrim$lbottemp = log(datasprtrim$bottemp+1)
	datasprtrim$lmintemp = log(datasprtrim$mintemp+1)	

# Add pres/abs
	datasprtrim$pres = datasprtrim$numcpue>0 | (is.na(datasprtrim$numcpue) & !is.na(datasprtrim$wtcpue) & datasprtrim$wtcpue>0)

## Add mean biomass as a predictor
bm = aggregate(list(biomassmean=datasprtrim$wtcpue), by=list(year=datasprtrim$year, spp=datasprtrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datasprtrim = merge(datasprtrim, bm, all.x=T)
	dim(datasprtrim) # 8785 x 38

bm = aggregate(list(nummean=datasprtrim$numcpue), by=list(year=datasprtrim$year, spp=datasprtrim$spp), FUN=mean, na.rm=T)
	dim(bm)
	datasprtrim = merge(datasprtrim, bm, all.x=T)
	dim(datasprtrim) # 8785 x 39


# Have a cpue that never goes to zero (useful for fitting log-links)
datasprtrim$wtcpuena = datasprtrim$wtcpue
datasprtrim$wtcpuena[datasprtrim$wtcpuena == 0] = 1e-4
datasprtrim$wtcpuenal = log(datasprtrim$wtcpuena)

datasprtrim$numcpuena = datasprtrim$numcpue
datasprtrim$numcpuena[datasprtrim$numcpuena == 0] = 1e-4
datasprtrim$numcpuenal = log(datasprtrim$numcpuena)

# Rearrange columns
nm = c('region', 'svvessel', 'cruise', 'station', 'haulid', 'stratum', 'tow', 'date', 'time', 'year', 'yearsurv', 'month', 'day', 'julian', 'juliansurv', 'lat', 'lon', 'latgrid', 'longrid', 'depth', 'surftemp', 'bottemp', 'surfsal', 'botsal', 'mintemp', 'maxtemp', 'mintempmnth', 'maxtempmnth', 'lsurftemp', 'lbottemp', 'lmintemp', 'spp', 'biomassmean', 'nummean', 'wtcpue', 'numcpue', 'wtcpuena', 'numcpuena', 'wtcpuenal', 'numcpuenal', 'pres')
	dim(datasprtrim)
	length(nm)
	
	setdiff(nm, names(datasprtrim))
	setdiff(names(datasprtrim), nm) # will drop common and stratarea
	 	 
	datasprtrim = datasprtrim[,nm]
		dim(datasprtrim) # 8785 x 41

## Write out for this species
	write.csv(datasprtrim, file=paste('../Output/datasprCEM_Paralichthysdentatus_', Sys.Date(), '.csv', sep=''))

