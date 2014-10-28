library(data.table) # much of this code could be sped up by converting to data.tables
library(PBSmapping) # for calculating stratum areas
library(maptools) # for calculating stratum areas
library(Hmisc)

setwd('/Users/mpinsky/Documents/Rutgers/CINAR Climate indicators/')

# useful function: acts like sum(na.rm=T) but returns NA if all are NA
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

# function to calculate convex hull area in km2
	#developed from http://www.nceas.ucsb.edu/files/scicomp/GISSeminar/UseCases/CalculateConvexHull/CalculateConvexHullR.html
	calcarea = function(lonlat){
		hullpts = chull(x=lonlat[,1], y=lonlat[,2]) # find indices of vertices
		hullpts = c(hullpts,hullpts[1]) # close the loop
		ps = appendPolys(NULL,mat=as.matrix(lonlat[hullpts,]),1,1,FALSE) # create a Polyset object
		attr(ps,"projection") = "LL" # set projection to lat/lon
		psUTM = convUL(ps, km=TRUE) # convert to UTM in km
		polygonArea = calcArea(psUTM,rollup=1)
		return(polygonArea$area)
	}

###############################
## Read in and reformat data ##
###############################
# Read in data
	# Scientific name to common name table
	tax = read.csv('data/spptaxonomy_2014-07-29.csv')

	# Aleutians
	ai1 = read.csv('data/ai1983_1997.csv')
	ai2 = read.csv('data/ai2000_2010.csv')	
	aistrata = read.csv('data/aiStrata.csv')
	ai = rbind(ai1, ai2)
	ai = merge(ai, aistrata[,c('StratumCode', 'Areakm2')], by.x='STRATUM', by.y='StratumCode', all.x=TRUE)
	rm(ai1, ai2, aistrata)

	# Eastern Bering Sea
	ebs1 = read.csv('data/ebs1982_1984.csv')
	ebs2 = read.csv('data/ebs1985_1989.csv')
	ebs3 = read.csv('data/ebs1990_1994.csv')
	ebs4 = read.csv('data/ebs1995_1999.csv')
	ebs5 = read.csv('data/ebs2000_2004.csv')
	ebs6 = read.csv('data/ebs2005_2008.csv')
	ebs7 = read.csv('data/ebs2009_2012.csv')
	ebs8 = read.csv('data/ebs2013.csv')
	ebsstrata = read.csv('data/ebsStrata.csv')
	ebs = rbind(ebs1, ebs2, ebs3, ebs4, ebs5, ebs6, ebs7, ebs8)
	ebs = merge(ebs, ebsstrata[,c('StratumCode', 'Areakm2')], by.x='STRATUM', by.y='StratumCode', all.x=TRUE)
	rm(ebs1, ebs2, ebs3, ebs4, ebs5, ebs6, ebs7, ebsstrata)

	# Gulf of Alaska
	goa1 = read.csv('data/goa1984_1987.csv')
	goa2 = read.csv('data/goa1990_1999.csv')
	goa3 = read.csv('data/goa2001_2005.csv')
	goa4 = read.csv('data/goa2007_2011.csv')
	goastrata = read.csv('data/goaStrata.csv')
	goa = rbind(goa1, goa2, goa3, goa4)
	goa = merge(goa, goastrata[,c('StratumCode', 'Areakm2')], by.x='STRATUM', by.y='StratumCode', all.x=TRUE)
	rm(goa1, goa2, goa3, goa4, goastrata)

	# Northeast US
	load("data/neus1963_2012.Rdata") # comes as a data.table
	load("data/neusSVSPP.RData")
	neusstrata = read.csv('data/neusStrata.csv') # strata areas
	setkey(survdat, CRUISE6, STATION, STRATUM, SVSPP, CATCHSEX)
	neus <- unique(survdat) # drops length data
	neus[, c('LENGTH', 'NUMLEN') := NULL] # remove length columns
	neus = neus[,sum(BIOMASS),by=list(YEAR, SEASON, LAT, LON, DEPTH, CRUISE6, STATION, STRATUM, SVSPP)] # sum different sexes of same spp together
	setnames(neus, 'V1', 'wtcpue')
	neus = neus[SEASON=='SPRING',] # trim to spring survey only
	spp[,c('ITISSPP', 'COMNAME', 'AUTHOR') := NULL] # remove some columns from spp data.table
	neus = merge(neus, spp, by='SVSPP') # add species names
	neus = as.data.frame(neus) # this makes the calculations less efficient... but avoids having to rewrite the code for data.tables
	neus = merge(neus, neusstrata[,c('StratumCode', 'Areanmi2')], by.x='STRATUM', by.y = 'StratumCode', all.x=TRUE)
	rm(survdat, neusstrata)

	# West Coast Trienniel (1977-2004)
	wctricatch = read.csv('data/wctri1977_2004catch.csv')
	wctrihaul = read.csv('data/wctri1977_2004haul.csv')
	wctrispecies = read.csv('data/wctri1977_2004species.csv')
	wctri = merge(wctricatch[,c('CRUISEJOIN', 'HAULJOIN', 'VESSEL', 'CRUISE', 'HAUL', 'SPECIES_CODE', 'WEIGHT')], wctrihaul[,c('CRUISEJOIN', 'HAULJOIN', 'VESSEL', 'CRUISE', 'HAUL', 'HAUL_TYPE', 'PERFORMANCE', 'START_TIME', 'DURATION', 'DISTANCE_FISHED', 'NET_WIDTH', 'STRATUM', 'START_LATITUDE', 'END_LATITUDE', 'START_LONGITUDE', 'END_LONGITUDE', 'STATIONID', 'BOTTOM_DEPTH')], all.x=TRUE) # Add haul info to catch data
	wctri = merge(wctri, wctrispecies[,c('SPECIES_CODE', 'SPECIES_NAME', 'COMMON_NAME')]) #  add species names
	wctri = wctri[wctri$HAUL_TYPE==3 & wctri$PERFORMANCE==0,] # trim to standard hauls and good performance
	rm(wctricatch, wctrihaul, wctrispecies)
	
	# West Coast annual (2003-2012)
	wcannfish = read.csv('data/wcann2003_2012fish.csv')
	wcannhaul = read.csv('data/wcann2003_2012haul.csv')
	wcanninvert = read.csv('data/wcann2003_2012invert.csv')
	wcanncatch = rbind(wcannfish[,names(wcanninvert)], wcanninvert) # wcannfish has an extra column, so trim it out while combining with inverts
	wcann = merge(wcannhaul, wcanncatch)
	rm(wcannfish, wcannhaul, wcanninvert, wcanncatch)

	# Gulf of Mexico: requires a lot of prep before a single data.frame can be created
	gmexstation = read.csv('data/gmexSTAREC.csv')
	gmextow = read.csv('data/gmexINVREC.csv')
	gmexspp = read.csv('data/gmexNEWBIOCODESBIG.csv')
	gmexcruise = read.csv('data/gmexCRUISES.csv')
	test = read.csv('data/gmexBGSREC.csv', nrows=2) # gmexbio is a large file: only read in some columns
	biocols = c('CRUISEID', 'STATIONID', 'VESSEL', 'CRUISE_NO', 'P_STA_NO', 'GENUS_BGS', 'SPEC_BGS', 'BGSCODE', 'BIO_BGS', 'SELECT_BGS')
	colstoread = rep('NULL', ncol(test)) # NULL means don't read that column (see ?read.csv)
	colstoread[names(test) %in% biocols] = NA # NA means read that column
	gmexbio = read.csv('data/gmexBGSREC.csv', colClasses=colstoread) # sped up by reading in only some columns
		# trim out young of year records (only useful for count data) and those with UNKNOWN species
		gmexbio = gmexbio[gmexbio$BGSCODE != 'T' & gmexbio$GENUS_BGS != 'UNKNOWN',]
		gmexbio = gmexbio[!duplicated(gmexbio),] # remove the few rows that are still duplicates
	newspp = data.frame(Key1 = c(503,5770), TAXONOMIC = c('ANTHIAS TENUIS AND WOODSI', 'MOLLUSCA AND UNID.OTHER #01'), CODE=c(170026003, 300000000), TAXONSIZECODE=NA, isactive=-1, common_name=c('threadnose and swallowtail bass', 'molluscs or unknown'), tsn = NA) # make two combined records where multiple species records share the same species code
	gmexspp = gmexspp[!(gmexspp$CODE %in% gmexspp$CODE[which(duplicated(gmexspp$CODE))]),] # remove the duplicates that were just combined
	gmexspp = rbind(gmexspp[,names(newspp)], newspp) # add the combined records on to the end. trim out extra columns from gmexspp

	gmex = merge(gmexbio, gmextow[gmextow$GEAR_TYPE=='ST', c('STATIONID', 'CRUISE_NO', 'P_STA_NO', 'INVRECID', 'GEAR_SIZE', 'GEAR_TYPE', 'MESH_SIZE', 'MIN_FISH', 'OP')], all.x=TRUE) # merge tow information with catch data, but only for shrimp trawl tows (ST)
	gmex = merge(gmex, gmexstation[,c('STATIONID', 'CRUISEID', 'CRUISE_NO', 'P_STA_NO', 'TIME_ZN', 'TIME_MIL', 'S_LATD', 'S_LATM', 'S_LOND', 'S_LONM', 'E_LATD', 'E_LATM', 'E_LOND', 'E_LONM', 'DEPTH_SSTA', 'MO_DAY_YR', 'VESSEL_SPD', 'COMSTAT')], all.x=TRUE) # add station location and related data
	gmex = merge(gmex, gmexspp[,c('CODE', 'TAXONOMIC')], by.x='BIO_BGS', by.y='CODE', all.x=TRUE) # add scientific name
	gmex = merge(gmex, gmexcruise[,c('CRUISEID', 'VESSEL', 'TITLE')], all.x=TRUE) # add cruise title
	gmex = gmex[gmex$TITLE %in% c('Summer SEAMAP Groundfish Survey', 'Summer SEAMAP Groundfish Suvey') & gmex$GEAR_SIZE==40 & gmex$MESH_SIZE == 1.63 & !is.na(gmex$MESH_SIZE) & gmex$OP %in% c(''),] # # Trim to high quality SEAMAP summer trawls, based off the subset used by Jeff Rester's GS_TRAWL_05232011.sas

# Create a unique haulid
	ai$haulid = paste(formatC(ai$VESSEL, width=3, flag=0), formatC(ai$CRUISE, width=3, flag=0), formatC(ai$HAUL, width=3, flag=0), sep='-')
	ebs$haulid = paste(formatC(ebs$VESSEL, width=3, flag=0), formatC(ebs$CRUISE, width=3, flag=0), formatC(ebs$HAUL, width=3, flag=0), sep='-')
	goa$haulid = paste(formatC(goa$VESSEL, width=3, flag=0), formatC(goa$CRUISE, width=3, flag=0), formatC(goa$HAUL, width=3, flag=0), sep='-')
	neus$haulid = paste(formatC(neus$CRUISE6, width=6, flag=0), formatC(neus$STATION, width=3, flag=0), formatC(neus$STRATUM, width=4, flag=0), sep='-') 
	wctri$haulid = paste(formatC(wctri$VESSEL, width=3, flag=0), formatC(wctri$CRUISE, width=3, flag=0), formatC(wctri$HAUL, width=3, flag=0), sep='-')
	wcann$haulid = wcann$Trawl.Id
	gmex$haulid = paste(formatC(gmex$VESSEL, width=3, flag=0), formatC(gmex$CRUISE_NO, width=3, flag=0), formatC(gmex$P_STA_NO, width=5, flag=0, format='d'), sep='-')

# Extract year where needed
	wctri$year = as.numeric(substr(wctri$CRUISE, 1,4))
	wcann$year = as.numeric(gsub('Cycle ', '', wcann$Survey.Cycle))
	gmex$year = as.numeric(unlist(strsplit(as.character(gmex$MO_DAY_YR), split='-'))[seq(1,by=3,length=nrow(gmex))])

# Calculate decimal lat and lon, depth in m, where needed
	gmex$S_LATD[gmex$S_LATD == 0] = NA
	gmex$S_LOND[gmex$S_LOND == 0] = NA
	gmex$E_LATD[gmex$E_LATD == 0] = NA
	gmex$E_LOND[gmex$E_LOND == 0] = NA
	gmex$lat = rowMeans(cbind(gmex$S_LATD + gmex$S_LATM/60, gmex$E_LATD + gmex$E_LATM/60), na.rm=T) # mean of start and end positions, but allow one to be NA (missing)
	gmex$lon = -rowMeans(cbind(gmex$S_LOND + gmex$S_LONM/60, gmex$E_LOND + gmex$E_LONM/60), na.rm=T) # need negative sign since western hemisphere
	gmex$depth = gmex$DEPTH_SSTA*1.8288 # convert fathoms to meters

# Add "strata" (define by lat, lon and depth bands) where needed
	stratlatgrid = floor(wctri$START_LATITUDE)+0.5 # degree bins
	stratdepthgrid = floor(wctri$BOTTOM_DEPTH/100)*100 + 50 # 100 m bins
	wctri$stratum = paste(stratlatgrid, stratdepthgrid, sep='-') # no need to use lon grids on west coast (so narrow)

	stratlatgrid = floor(wcann$Best.Latitude..dd.)+0.5 # degree bins
	stratdepthgrid = floor(wcann$Best.Depth..m./100)*100 + 50 # 100 m bins
	wcann$stratum = paste(stratlatgrid, stratdepthgrid, sep='-') # no need to use lon grids on west coast (so narrow)

	stratlatgrid = floor(gmex$lat)+0.5 # degree bins
	stratlongrid = floor(gmex$lon)+0.5 # degree bins
	stratdepthgrid = floor(gmex$depth/100)*100 + 50 # 100 m bins
	gmex$stratum = paste(stratlatgrid, stratlongrid, stratdepthgrid, sep='-')
	rm(stratlatgrid, stratdepthgrid)
	
# Trim to high quality strata UPDATE TO SET ALASKA STRATA TO POSITIVE CHOICES
	ai = ai[!(ai$STRATUM %in% c(221, 411, 421, 521, 611)),]
	ebs = ebs[!(ebs$STRATUM %in% c(82,90)),]
	goa = goa[!(goa$STRATUM %in% c(50, 210, 410, 420, 430, 440, 450, 510, 520, 530, 540, 550)),] # strata to remove.
	neus = neus[neus$STRATUM %in% c("1010", "1020", "1030", "1040", "1050", "1060", "1070", "1080", "1090", "1100", "1110", "1130", "1140", "1150", "1160", "1170", "1190", "1200", "1210", "1220", "1230", "1240", "1250", "1260", "1270", "1280", "1290", "1300", "1340", "1360", "1370", "1380", "1400", "1650", "1660", "1670", "1680", "1690", "1700", "1710", "1730", "1740", "1750"), ] # strata to keep (based on Nye et al. MEPS)

	wctri = wctri[wctri$stratum %in% c("36.5-50", "37.5-150", "37.5-50", "38.5-150", "38.5-250", "38.5-350", "38.5-50", "39.5-150", "39.5-50", "40.5-150", "40.5-250", "41.5-150", "41.5-250", "41.5-50", "42.5-150", "42.5-250", "42.5-50", "43.5-150", "43.5-250", "43.5-350", "43.5-50", "44.5-150", "44.5-250", "44.5-350", "44.5-50", "45.5-150", "45.5-350", "45.5-50", "46.5-150", "46.5-250", "46.5-50", "47.5-150", "47.5-50", "48.5-150", "48.5-250", "48.5-50"),]

	wcann = wcann[wcann$stratum %in% c("36.5-50", "37.5-150", "37.5-50", "38.5-150", "38.5-250", "38.5-350", "38.5-50", "39.5-150", "39.5-50", "40.5-150", "40.5-250", "41.5-150", "41.5-250", "41.5-50", "42.5-150", "42.5-250", "42.5-50", "43.5-150", "43.5-250", "43.5-350", "43.5-50", "44.5-150", "44.5-250", "44.5-350", "44.5-50", "45.5-150", "45.5-350", "45.5-50", "46.5-150", "46.5-250", "46.5-50", "47.5-150", "47.5-50", "48.5-150", "48.5-250", "48.5-50"),] # trim wcann to same footprint as wctri

	gmex = gmex[gmex$stratum %in% c("26.5--96.5-50", "26.5--97.5-50", "27.5--96.5-50", "27.5--97.5-50", "28.5--90.5-50", "28.5--91.5-50", "28.5--92.5-50", "28.5--93.5-50", "28.5--94.5-50", "28.5--95.5-50", "28.5--96.5-50", "29.5--88.5-50", "29.5--89.5-50", "29.5--92.5-50", "29.5--93.5-50", "29.5--94.5-50"),]

# Trim to high-quality years (sample all strata)	
	goa = goa[!(goa$YEAR %in% 2001),] # 2001 didn't sample many strata
	gmex = gmex[!(gmex$year %in% c(1982, 1983)),] # 1982 and 1983 didn't sample many strata

# Trim out or fix speed and duration records
	gmex = gmex[gmex$MIN_FISH<=60 & gmex$MIN_FISH > 0 & !is.na(gmex$MIN_FISH),] # trim out tows of 0, >60, or unknown minutes
	gmex$VESSEL_SPD[gmex$VESSEL_SPD==30] = 3 # fix typo according to Jeff Rester: 30 = 3	
	gmex = gmex[gmex$VESSEL_SPD <= 5 & gmex$VESSEL_SPD > 0  & !is.na(gmex$VESSEL_SPD),] # trim out vessel speeds 0, unknown, or >5 (need vessel speed to calculate area trawled)

# Calculate stratum area where needed (use convex hull approach)
	neus$stratumarea = 	neus$Areanmi2 * 3.429904 # convert square nautical miles to square kilometers
	
	wctristrats = summarize(wctri[,c('START_LONGITUDE', 'START_LATITUDE')], by=list(stratum=wctri$stratum), FUN=calcarea, stat.name = 'stratumarea')
	wctri = merge(wctri, wctristrats[,c('stratum', 'stratumarea')], by.x='stratum', by.y='stratum', all.x=TRUE)

	wcannstrats = summarize(wcann[,c('Best.Longitude..dd.', 'Best.Latitude..dd.')], by=list(stratum=wcann$stratum), FUN=calcarea, stat.name = 'stratumarea')
	wcann = merge(wcann, wcannstrats[,c('stratum', 'stratumarea')], by.x='stratum', by.y='stratum', all.x=TRUE)

	gmexstrats = summarize(gmex[,c('lon', 'lat')], by=list(stratum=gmex$stratum), FUN=calcarea, stat.name = 'stratumarea')
	gmex = merge(gmex, gmexstrats[,c('stratum', 'stratumarea')], by.x='stratum', by.y='stratum', all.x=TRUE)

# Fix column names
	names(ai)[names(ai)=='STRATUM'] = 'stratum'
	names(ai)[names(ai)=='YEAR'] = 'year'
	names(ai)[names(ai)=='LATITUDE'] = 'lat'
	names(ai)[names(ai)=='LONGITUDE'] = 'lon' 
	names(ai)[names(ai)=='BOT_DEPTH'] = 'depth'
	names(ai)[names(ai)=='SCIENTIFIC'] = 'spp'
	names(ai)[names(ai)=='WTCPUE'] = 'wtcpue'
	names(ai)[names(ai)=='Areakm2'] = 'stratumarea'

	names(ebs)[names(ebs)=='STRATUM'] = 'stratum'
	names(ebs)[names(ebs)=='YEAR'] = 'year'
	names(ebs)[names(ebs)=='LATITUDE'] = 'lat'
	names(ebs)[names(ebs)=='LONGITUDE'] = 'lon' # use the adjusted longitude
	names(ebs)[names(ebs)=='BOT_DEPTH'] = 'depth'
	names(ebs)[names(ebs)=='SCIENTIFIC'] = 'spp'
	names(ebs)[names(ebs)=='WTCPUE'] = 'wtcpue'
	names(ebs)[names(ebs)=='Areakm2'] = 'stratumarea'

	names(goa)[names(goa)=='STRATUM'] = 'stratum'
	names(goa)[names(goa)=='YEAR'] = 'year'
	names(goa)[names(goa)=='LATITUDE'] = 'lat'
	names(goa)[names(goa)=='LONGITUDE'] = 'lon'
	names(goa)[names(goa)=='BOT_DEPTH'] = 'depth'
	names(goa)[names(goa)=='SCIENTIFIC'] = 'spp'
	names(goa)[names(goa)=='WTCPUE'] = 'wtcpue'
	names(goa)[names(goa)=='Areakm2'] = 'stratumarea'

	names(neus)[names(neus)=='YEAR'] = 'year'
	names(neus)[names(neus)=='SCINAME'] = 'spp'
	names(neus)[names(neus)=='LAT'] = 'lat'
	names(neus)[names(neus)=='LON'] = 'lon'
	names(neus)[names(neus)=='DEPTH'] = 'depth'
	names(neus)[names(neus)=='STRATUM'] = 'stratum'
	
	names(wctri)[names(wctri)=='VESSEL'] = 'svvessel'
	names(wctri)[names(wctri) == 'START_LATITUDE'] = 'lat'
	names(wctri)[names(wctri) == 'START_LONGITUDE'] = 'lon'
	names(wctri)[names(wctri) == 'BOTTOM_DEPTH'] = 'depth'
	names(wctri)[names(wctri) == 'SPECIES_NAME'] = 'spp'
	names(wctri)[names(wctri)=='WEIGHT'] = 'wtcpue'

	names(wcann)[names(wcann)=='Best.Latitude..dd.'] = 'lat'
	names(wcann)[names(wcann)=='Best.Longitude..dd.'] = 'lon'
	names(wcann)[names(wcann)=='Best.Depth..m.'] = 'depth'
	names(wcann)[names(wcann)=='Species'] = 'spp'

	names(gmex)[names(gmex)=='TAXONOMIC'] = 'spp'

# Turn -9999 to NA where needed
	ai$wtcpue[ai$wtcpue==-9999] = NA
	ebs$wtcpue[ebs$wtcpue==-9999] = NA
	goa$wtcpue[goa$wtcpue==-9999] = NA

# Adjust for towed area where needed
	wctri$wtcpue = wctri$wtcpue*10000/(wctri$DISTANCE_FISHED*1000*wctri$NET_WIDTH) # weight per hectare (10,000 m2)	
	wcann$wtcpue = wcann$Haul.Weight..kg./wcann$Area.Swept.by.the.Net..hectares. # kg per hectare (10,000 m2)	
	gmex$wtcpue = 10000*gmex$SELECT_BGS /(gmex$VESSEL_SPD * 1.85200 * 1000 * gmex$MIN_FISH / 60 * gmex$GEAR_SIZE * 0.3048) # kg per 10000m2. calc area trawled in m2: knots * 1.8 km/hr/knot * 1000 m/km * minutes * 1 hr/60 min * width of gear in feet * 0.3 m/ft # biomass per standard tow

# Remove a tow when paired tows exist (same lat/lon/year but different haulid, only Gulf of Mexico)
	dups = which(duplicated(gmex[,c('year', 'lat', 'lon')]) & !duplicated(gmex$haulid)) # identify duplicate tows at same year/lat/lon
	dupped = gmex[paste(gmex$year, gmex$lat, gmex$lon) %in% paste(gmex$year[dups], gmex$lat[dups], gmex$lon[dups]),] # all tows at these year/lat/lon
		# sum(!duplicated(dupped$haulid)) # 26 (13 pairs of haulids)
	gmex = gmex[!(gmex$haulid %in% unique(dupped$haulid[grep('PORT', dupped$COMSTAT)])),] # remove the port haul (this is arbitrary, but seems to be right based on the notes associated with these hauls)

# Removes rows without scientific names or that aren't fish or inverts
	ai = ai[ai$spp != '' & !(ai$spp %in% c("Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")),]
	ebs = ebs[ebs$spp != '' & !(ebs$spp %in% c("Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")),]
	goa = goa[goa$spp != '' & !(goa$spp %in% c("Decapodiformesunid.egg", "Volutopsiussp.eggs", "Bathyrajaaleuticaeggcase", "Bathyrajainterruptaeggcase", "Bathyrajamaculataeggcase", "Bathyrajaparmiferaeggcase", "Bathyrajasp.", "Bathyrajasp.eggcase", "Bathyrajataranetzieggcase", "Beringiussp.eggs", "Buccinumsp.Eggs", "Fusitritonoregonensiseggs", "gastropodeggs", "Hemitripterusbolinieggs", "Naticidaeeggs", "Neptuneasp.eggs", "Pyrulofusussp.eggs", "Rajabadiaeggcase", "Rossiapacificaeggs", "Bathyraja aleutica egg case", "Bathyraja interrupta egg case", "Bathyraja parmifera egg case", "Bathyraja sp. egg case", "gastropod eggs", "Neptunea sp. eggs", "Rajarhinaeggcase", "Rajasp.eggcase", "Apristurus brunneus egg case", "Selachimorpha egg case")),]
	neus = neus[!(neus$spp == '' | is.na(neus$spp)),]
	neus = neus[!(neus$spp %in% c('UNIDENTIFIED FISH', 'ILLEX ILLECEBROSUS EGG MOPS', 'LOLIGO PEALEII EGG MOPS')),] # remove unidentified spp and non-species
	wctri = wctri[wctri$spp != '' & !(wctri$spp %in% c("Apristurus brunneus egg case", "fish eggs unident.", "Raja binoculata egg case", "Raja sp. egg case", "Rajiformes egg case", "Shark egg case unident.")),]
	wcann = wcann[wcann$spp != '' & !(wcann$spp %in% c("Apristurus brunneus egg case", "gastropod eggs", "Selachimorpha egg case")),]
	gmex = gmex[!(gmex$spp == '' | is.na(gmex$spp)),]
	gmex = gmex[!(gmex$spp %in% c('UNID CRUSTA', 'UNID OTHER', 'UNID.FISH', 'CRUSTACEA(INFRAORDER) BRACHYURA', 'MOLLUSCA AND UNID.OTHER #01', 'ALGAE', 'MISCELLANEOUS INVERTEBR', 'OTHER INVERTEBRATES')),]	# remove unidentified spp

# Adjust spp names for those cases where they've changed or where matching failed (GMex)
	# first convert factors to strings so that we can modify them
	i <- sapply(ai, is.factor); ai[i] <- lapply(ai[i], as.character)
	i <- sapply(ebs, is.factor); ebs[i] <- lapply(ebs[i], as.character)
	i <- sapply(goa, is.factor); goa[i] <- lapply(goa[i], as.character)
	i <- sapply(neus, is.factor); neus[i] <- lapply(neus[i], as.character)
	i <- sapply(wctri, is.factor); wctri[i] <- lapply(wctri[i], as.character)
	i <- sapply(wcann, is.factor); wcann[i] <- lapply(wcann[i], as.character)
	i <- sapply(gmex, is.factor); gmex[i] <- lapply(gmex[i], as.character)


	ai$spp[ai$spp %in% c('Atheresthesevermanni', 'Atheresthesstomias')] = 'Atheresthessp.'
	ai$spp[ai$spp %in% c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')] = 'Lepidopsettasp.'
	ai$spp[ai$spp %in% c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')] = 'Myoxocephalussp.'
	ai$spp[ai$spp %in% c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajainterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')] = 'Bathyrajasp.'

	ebs$spp[ebs$spp %in% c('Atheresthes evermanni', 'Atheresthes stomias')] = 'Atheresthes sp.'
	ebs$spp[ebs$spp %in% c('Lepidopsetta polyxystra', 'Lepidopsetta bilineata')] = 'Lepidopsetta sp.'
	ebs$spp[ebs$spp %in% c('Hippoglossoides elassodon', 'Hippoglossoides robustus')] = 'Hippoglossoides sp.'
	ebs$spp[ebs$spp %in% c('Myoxocephalus jaok', 'Myoxocephalus niger', 'Myoxocephalus polyacanthocephalus', 'Myoxocephalus quadricornis', 'Myoxocephalus verrucosus', 'Myoxocephalus scorpioides')] = 'Myoxocephalus sp.'
	ebs$spp[ebs$spp %in% c('Bathyraja abyssicola', 'Bathyraja aleutica', 'Bathyraja interrupta', 'Bathyraja lindbergi', 'Bathyraja maculata', 'Bathyraja mariposa', 'Bathyraja minispinosa', 'Bathyraja parmifera', 'Bathyraja smirnovi', 'Bathyraja sp.', 'Bathyraja sp.cf.parmifera(Orretal.)', 'Bathyraja spinosissima', 'Bathyraja taranetzi', 'Bathyraja trachura', 'Bathyraja violacea')] = 'Bathyraja sp.'

	goa$spp[goa$spp %in% c('Lepidopsettapolyxystra', 'Lepidopsettabilineata')] = 'Lepidopsettasp.'
	goa$spp[goa$spp %in% c('Myoxocephalusjaok', 'Myoxocephalusniger', 'Myoxocephaluspolyacanthocephalus', 'Myoxocephalusquadricornis', 'Myoxocephalusverrucosus')] = 'Myoxocephalussp.'
	goa$spp[goa$spp %in% c('Bathyrajaabyssicola', 'Bathyrajaaleutica', 'Bathyrajainterrupta', 'Bathyrajalindbergi', 'Bathyrajamaculata', 'Bathyrajamariposa', 'Bathyrajaminispinosa', 'Bathyrajaparmifera', 'Bathyrajasmirnovi', 'Bathyrajasp.cf.parmifera(Orretal.)', 'Bathyrajaspinosissima', 'Bathyrajataranetzi', 'Bathyrajatrachura', 'Bathyrajaviolacea')] = 'Bathyrajasp.'

	# For this first draft, we don't need to clean up NEUS names here

	wctri$spp[wctri$spp %in% c('Lepidopsetta polyxystra', 'Lepidopsetta bilineata')] = 'Lepidopsetta sp.'
	wctri$spp[wctri$spp %in% c('Bathyraja interrupta', 'Bathyraja trachura', 'Bathyraja parmifera', 'Bathyraja spinosissima')] = 'Bathyrajasp.'

	wcann$spp[wcann$spp %in% c('Lepidopsetta polyxystra', 'Lepidopsetta bilineata')] = 'Lepidopsetta sp.' # so that species match wctri
	wcann$spp[wcann$spp %in% c('Bathyraja abyssicola', 'Bathyraja aleutica', 'Bathyraja kincaidii (formerly B. interrupta)', 'Bathyraja sp. ', 'Bathyraja trachura', 'Bathyraja parmifera', 'Bathyraja spinosissima')] = 'Bathyrajasp.'

	i = gmex$GENUS_BGS == 'PELAGIA' & gmex$SPEC_BGS == 'NOCTUL'; gmex$spp[i] = 'PELAGIA NOCTILUCA'; gmex$BIO_BGS[i] = 618030201
	i = gmex$GENUS_BGS == 'MURICAN' & gmex$SPEC_BGS == 'FULVEN'; gmex$spp[i] = 'MURICANTHUS FULVESCENS'; gmex$BIO_BGS[i] = 308011501
	i = gmex$spp %in% c('APLYSIA BRASILIANA', 'APLYSIA WILLCOXI'); gmex$spp[i] = 'APLYSIA'
	i = gmex$spp %in% c('AURELIA AURITA'); gmex$spp[i] = 'AURELIA'
	i = gmex$spp %in% c('BOTHUS LUNATUS', 'BOTHUS OCELLATUS', 'BOTHUS ROBINSI'); gmex$spp[i] = 'BOTHUS'
	i = gmex$spp %in% c('CLYPEASTER PROSTRATUS', 'CLYPEASTER RAVENELII'); gmex$spp[i] = 'CLYPEASTER'
	i = gmex$spp %in% c('CONUS AUSTINI', 'CONUS STIMPSONI'); gmex$spp[i] = 'CONUS'
	i = gmex$spp %in% c('CYNOSCION ARENARIUS', 'CYNOSCION NEBULOSUS', 'CYNOSCION NOTHUS'); gmex$spp[i] = 'CYNOSCION'
	i = gmex$spp %in% c('ECHINASTER SENTUS', 'ECHINASTER SERPENTARIUS'); gmex$spp[i] = 'ECHINASTER'
	i = gmex$spp %in% c('ECHINASTER SENTUS', 'ECHINASTER SERPENTARIUS'); gmex$spp[i] = 'ECHINASTER'
	i = gmex$spp %in% c('OPISTOGNATHUS AURIFRONS', 'OPISTOGNATHUS LONCHURUS'); gmex$spp[i] = 'OPISTOGNATHUS'
	i = gmex$spp %in% c('OPSANUS BETA', 'OPSANUS PARDUS', 'OPSANUS TAU'); gmex$spp[i] = 'OPSANUS'
	i = gmex$spp %in% c('ROSSIA BULLISI'); gmex$spp[i] = 'ROSSIA'
	i = gmex$spp %in% c('SOLENOCERA ATLANTIDIS', 'SOLENOCERA NECOPINA', 'SOLENOCERA VIOSCAI'); gmex$spp[i] = 'SOLENOCERA'
	i = gmex$spp %in% c('TRACHYPENEUS CONSTRICTUS', 'TRACHYPENEUS SIMILIS'); gmex$spp[i] = 'TRACHYPENEUS'

# Combine entries from spp that have now been consolidated
	ai2 = aggregate(list(wtcpue = ai$wtcpue), by = list(haulid = ai$haulid, stratum = ai$stratum, stratumarea = ai$stratumarea, year = ai$year, lat = ai$lat, lon = ai$lon, depth = addNA(ai$depth), spp = ai$spp), FUN=sumna) # use addNA() for depth so that NA values are not dropped by aggregate()
		ai2$depth = as.numeric(as.character(ai2$depth)) # convert depth back to a numeric

	ebs2 = aggregate(list(wtcpue = ebs$wtcpue), by = list(haulid = ebs$haulid, stratum = ebs$stratum, stratumarea = ebs$stratumarea, year = ebs$year, lat = ebs$lat, lon = ebs$lon, depth = addNA(ebs$depth), spp = ebs$spp), FUN=sumna) # use addNA() for depth so that NA values are not dropped by aggregate()
		ebs2$depth = as.numeric(as.character(ebs2$depth)) # convert depth back to a numeric

	goa2 = aggregate(list(wtcpue = goa$wtcpue), by = list(haulid = goa$haulid, stratum = goa$stratum, stratumarea = goa$stratumarea, year = goa$year, lat = goa$lat, lon = goa$lon, depth = addNA(goa$depth), spp = goa$spp), FUN=sumna) # use addNA() for depth so that NA values are not dropped by aggregate()
		goa2$depth = as.numeric(as.character(goa2$depth)) # convert depth back to a numeric

	neus2 = aggregate(list(wtcpue = neus$wtcpue), by = list(haulid = neus$haulid, stratum = neus$stratum, stratumarea = neus$stratumarea, year = neus$year, lat = neus$lat, lon = neus$lon, depth = addNA(neus$depth), spp = neus$spp), FUN=sumna) # use addNA() for depth so that NA values are not dropped by aggregate()
		neus2$depth = as.numeric(as.character(neus2$depth)) # convert depth back to a numeric

	wctri2 = aggregate(list(wtcpue = wctri$wtcpue), by = list(haulid = wctri$haulid, stratum = wctri$stratum, stratumarea = wctri$stratumarea, year = wctri$year, lat = wctri$lat, lon = wctri$lon, depth = wctri$depth, spp = wctri$spp), FUN=sumna)

	wcann2 = aggregate(list(wtcpue = wcann$wtcpue), by = list(haulid = wcann$haulid, stratum = wcann$stratum, stratumarea = wcann$stratumarea, year = wcann$year, lat = wcann$lat, lon = wcann$lon, depth = wcann$depth, spp = wcann$spp), FUN=sumna)

	gmex2 = aggregate(list(wtcpue = gmex$wtcpue), by=list(haulid = gmex$haulid, stratum = gmex$stratum, stratumarea = gmex$stratumarea, year = gmex$year, lat = gmex$lat, lon = gmex$lon, depth = gmex$depth, spp = gmex$spp), FUN=sumna)
	

# Calculate a corrected longitude for Aleutians (all in western hemisphere coordinates)
	ai2$lon[ai2$lon>0] = ai2$lon[ai2$lon>0] - 360	

# Add a region column
ai2$region = "AFSC_Aleutians"
ebs2$region = "AFSC_EBS"
goa2$region = "AFSC_GOA"
neus2$region = "NEFSC_NEUSSpring"
wctri2$region = "AFSC_WCTri"
wcann2$region = "NWFSC_WCAnn"
gmex2$region = "SEFSC_GOMex"

# Rearrange and trim columns
nm = c('region', 'haulid', 'year', 'lat', 'lon', 'stratum', 'stratumarea', 'depth', 'spp', 'wtcpue')
ai2 = ai2[,nm]
ebs2 = ebs2[,nm]
goa2 = goa2[,nm]
neus2 = neus2[,nm]	
wctri2 = wctri2[,nm]
wcann2 = wcann2[,nm]
gmex2 = gmex2[,nm]

# combine together
dat = rbind(ai2, ebs2, goa2, neus2, wctri2, wcann2, gmex2)
# dim(dat)


# Remove NA values in wtcpue
dat = dat[!is.na(dat$wtcpue),]

# add a nice spp and common name
dat2 = merge(dat, tax[,c('taxon', 'name', 'common')], by.x='spp', by.y='taxon')
dat2$spp = dat2$name
dat2 = dat2[,c('region', 'haulid', 'year', 'lat', 'lon', 'stratum', 'stratumarea', 'depth', 'spp', 'common', 'wtcpue')]

# check for errors in name matching
if(sum(dat2$spp == 'NA') > 0 | sum(is.na(dat2$spp)) > 0) stop('Did not match on some taxon names')

# Write out
dat = dat2
save(dat, file=paste('output/trawl_allregions_', Sys.Date(), '.RData', sep='')) # 20.4MB as binary

