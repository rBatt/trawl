setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')

###########################################
## Assemble the regions into one dataset ##
###########################################

# Read in files (11 surveys in 9 regions)
dataal = read.csv('AFSC Aleutians/Output/datatrimwzeros_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
dataebs = read.csv('AFSC Eastern Bering Shelf/Output/datatrimwzeros_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
datagoa = read.csv('AFSC Gulf of Alaska/Output/datatrimwzeros_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
datawc = read.csv('NWFSC/WCTriSurveysAllCoast77thru04/Output/datatrimwzeros_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
datagom = read.csv('SEFSC Gulf/Output/datatrimwzeros_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
dataecspr = read.csv('NEFSC/Output/datatrimwzerosspr_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
dataecfal = read.csv('NEFSC/Output/datatrimwzerosfal_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
datass = read.csv('DFO Scotian Shelf/Output/datatrimwzeros_2012-11-15.csv', row.names=1, stringsAsFactors=FALSE)
datasgsl = read.csv('DFO Southern Gulf/Output/datatrimwzeros_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
datanfspr = read.csv('DFO Newfoundland/Output/datasprtrimwzeros_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
datanffal = read.csv('DFO Newfoundland/Output/datafaltrimwzeros_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)

data = rbind(dataal, dataebs, datagoa, datawc, datagom, dataecspr, dataecfal, datass, datasgsl, datanfspr, datanffal)

	sort(unique(data$region))
	dim(data) # 4,901,566
	data$regspp = paste(data$region, data$spp, sep='_') # unique ID for each region/spp combination
	summary(data)
	rm(dataal, dataebs, datagoa, datawc, datagom, dataecspr, dataecfal, datass, datasgsl, datanfspr, datanffal)

# Write out full data file
write.csv(data, paste('Output/data_allregions_', Sys.Date(), '.csv', sep=''))

################################
# Assemble just the good hauls #
################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
dataal = read.csv('AFSC Aleutians/Output/goodhauls_2012-04-01.csv', row.names=1, stringsAsFactors=FALSE)
	dataal$yearsurv = dataal$year; dataal$juliansurv = dataal$julian
	dataal$region = 'AFSC_Aleutians'
dataebs = read.csv('AFSC Eastern Bering Shelf/Output/goodhauls_2012-04-01.csv', row.names=1, stringsAsFactors=FALSE)
	dataebs$yearsurv = dataebs$year; dataebs$juliansurv = dataebs$julian
	dataebs$region = 'AFSC_EBS'
datagoa = read.csv('AFSC Gulf of Alaska/Output/goodhauls_2012-04-02.csv', row.names=1, stringsAsFactors=FALSE)
	datagoa$yearsurv = datagoa$year; datagoa$juliansurv = datagoa$julian
	datagoa$region = 'AFSC_GOA'
datawc = read.csv('NWFSC/WCTriSurveysAllCoast77thru04/Output/goodhauls_2012-04-02.csv', row.names=1, stringsAsFactors=FALSE)
	datawc$yearsurv = datawc$year; datawc$juliansurv = datawc$julian
	datawc$region = 'WestCoast_Tri'
datagom = read.csv('SEFSC Gulf/Output/goodhauls_2012-07-23.csv', row.names=1, stringsAsFactors=FALSE)
	datagom$yearsurv = datagom$year; datagom$juliansurv = datagom$julian
	datagom$region = 'SEFSC_GOMex'
dataecspr = read.csv('NEFSC/Output/goodhaulsspr_2012-04-02.csv', row.names=1, stringsAsFactors=FALSE)
	dataecspr$yearsurv = dataecspr$year; dataecspr$juliansurv = dataecspr$julian
	dataecspr$region = 'NEFSC_Spring'
dataecfal = read.csv('NEFSC/Output/goodhaulsfal_2012-04-02.csv', row.names=1, stringsAsFactors=FALSE)
	dataecfal$yearsurv = dataecfal$year; dataecfal$juliansurv = dataecfal$julian
	dataecfal$region = 'NEFSC_Fall'
datass = read.csv('DFO Scotian Shelf/Output/goodhauls_2012-11-15.csv', row.names=1, stringsAsFactors=FALSE)
	datass$yearsurv = datass$year; datass$juliansurv = datass$julian
	datass$region = 'DFO_ScotianShelf'
datasgsl = read.csv('DFO Southern Gulf/Output/goodhauls_2012-04-02.csv', row.names=1, stringsAsFactors=FALSE)
	datasgsl$yearsurv = datasgsl$year; datasgsl$juliansurv = datasgsl$julian
	datasgsl$region = 'DFO_SoGulf'
datanfspr = read.csv('DFO Newfoundland/Output/goodhaulspr_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
	datanfspr$region = 'DFO_Newfoundland_Spring'
datanffal = read.csv('DFO Newfoundland/Output/goodhaulfal_2012-08-04.csv', row.names=1, stringsAsFactors=FALSE)
	datanffal$region = 'DFO_Newfoundland_Fall'

	# Any names that don't match?
	setdiff(union(names(dataal), names(dataebs)), intersect(names(dataal), names(dataebs)))
	setdiff(union(names(dataal), names(datagoa)), intersect(names(dataal), names(datagoa)))
	setdiff(union(names(dataal), names(datawc)), intersect(names(dataal), names(datawc)))
	setdiff(union(names(dataal), names(datagom)), intersect(names(dataal), names(datagom)))
	setdiff(union(names(dataal), names(dataecspr)), intersect(names(dataal), names(dataecspr)))
	setdiff(union(names(dataal), names(dataecfal)), intersect(names(dataal), names(dataecfal)))
	setdiff(union(names(dataal), names(datass)), intersect(names(dataal), names(datass)))
	setdiff(union(names(dataal), names(datasgsl)), intersect(names(dataal), names(datasgsl)))
	setdiff(union(names(dataal), names(datanfspr)), intersect(names(dataal), names(datanfspr)))
	setdiff(union(names(dataal), names(datanffal)), intersect(names(dataal), names(datanffal)))

goodhauls = rbind(dataal, dataebs, datagoa, datawc, datagom, dataecspr, dataecfal, datass, datasgsl, datanfspr, datanffal)
	names(goodhauls)

	dim(goodhauls) # 72,526 x 22
	summary(goodhauls)
	rm(dataal, dataebs, datagoa, datawc, datagom, dataecspr, dataecfal, datass, datasgsl, datanfspr, datanffal)

# Write out full data file
write.csv(goodhauls, paste('Output/goodhauls_allregions_', Sys.Date(), '.csv', sep=''))

#########################
# Plots of survey areas #
#########################
require(maps)
require(mapdata)
source('map2.R') # make maps with a specified polygon border color

# Read in full file
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
data = read.csv('Output/goodhauls_allregions_2012-11-19.csv', row.names=1, stringsAsFactors=FALSE)
	dim(data) # 72,526 x 22

# Maps of survey locations (Fig. S1 for paper)
regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')
regsnice = c('Eastern Bering Sea', 'Aleutian Islands', 'Gulf of Alaska', 'West Coast U.S.', 'Gulf of Mexico', 'Northeast U.S.', 'Scotian Shelf', 'So. Gulf of St. Lawrence', 'Newfoundland')
ylabs = c('Latitude (°N)', '', '', '', 'Latitude (°N)', '', '', 'Latitude (°N)', '')
xlabs = c('', '', '', 'Longitude (°E)', '', '', 'Longitude (°E)', 'Longitude (°E)', 'Longitude (°E)')
ylims = list(ebs = c(54,62), al = c(50, 57), goa = c(52, 62), wc = c(36, 50), gom = c(26,30), ne = c(36, 45), ss = c(42, 47), sl = c(45.5, 49.5), nf = c(42, 56))
xlims = list(ebs = c(-180,-154), al = c(-190, -165), goa = c(-170, -133), wc = c(-127, -121), gom = c(-98,-88), ne = c(-76, -65), ss = c(-68, -57), sl = c(-66, -60), nf = c(-60, -45))
#bcol = 'grey50'
bcol = 'light green'
cex = 0.1; col = rgb(0.2, 0.2, 0.2, 0.5) # for paper
#cex = 1.5; col='grey' # for Cheung
data(worldHiresMapEnv)
data(world2HiresMapEnv)

quartz(width=7, height=4.5)
# pdf(width=7, height=4.5, file=paste('Figures/surveymaps_', Sys.Date(), '.pdf', sep=''))
# jpeg(width=7, height=4.5, units='in', res=1200, file=paste('Figures/surveymaps_', Sys.Date(), '.jpg', sep=''))
par(mai=c(0.2, 0.2, 0.2, 0.1), mgp = c(2, 0.6, 0), omi=c(0.2, 0.2, 0, 0), tcl=-0.3)
layout(mat=matrix(c(1,2,3,4,5,6,7,4,8,9,10,11), byrow=TRUE, nrow=3))
for(r in 1:length(regs)){
	inds = data$region == regs[r]
	if(regs[r] != 'AFSC_Aleutians'){
		plot(data$lon[inds], data$lat[inds], pch=16, cex=cex, main=regsnice[r], xlab='', ylab='', col=col, ylim=ylims[[r]], xlim=xlims[[r]], las=1)
	map2('worldHires', xlim=xlims[[r]], ylim=ylims[[r]], fill=TRUE, col=bcol, bcol=bcol, wrap=TRUE, myborder = 0, add=TRUE)
	} else { # for plotting on lon -190 to -170
		x = data$lon[inds]; x[x>0] = x[x>0]-360
		plot(x, data$lat[inds], pch=16, cex=cex, main=regsnice[r], xlab='', ylab='', col=col, ylim=ylims[[r]], xlim=xlims[[r]], las=1)
		map2('worldHires', xlim=xlims[[r]], ylim=ylims[[r]], fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, add=TRUE)
		two = map2('worldHires', xlim=xlims[[r]]+360, ylim=ylims[[r]], fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, plot=FALSE)
		two$x = two$x -360		
		polygon(two, col=bcol, border=NA)	
	}
	mtext(xlabs[r], 1, line=2, cex=0.8)
	mtext(ylabs[r], 2, line=2, cex=0.8)
}

	dev.off()


# Maps of survey region polygons
regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')
regsnice = c('Eastern Bering Sea', 'Aleutian Islands', 'Gulf of Alaska', 'West Coast U.S.', 'Gulf of Mexico', 'Northeast U.S.', 'Scotian Shelf', 'So. Gulf of St. Lawrence', 'Newfoundland')
ylabs = c('Latitude (°N)', '', '', '', 'Latitude (°N)', '', '', 'Latitude (°N)', '')
xlabs = c('', '', '', 'Longitude (°E)', '', '', 'Longitude (°E)', 'Longitude (°E)', 'Longitude (°E)')
ylims = list(ebs = c(54,62), al = c(50, 57), goa = c(52, 62), wc = c(36, 50), gom = c(26,30), ne = c(36, 45), ss = c(42, 47), sl = c(45.5, 49.5), nf = c(42, 56))
xlims = list(ebs = c(-180,-154), al = c(-190, -165), goa = c(-170, -133), wc = c(-127, -121), gom = c(-98,-88), ne = c(-76, -65), ss = c(-68, -57), sl = c(-66, -60), nf = c(-60, -45))
#bcol = 'grey50'
bcol = 'light green'

	# Calculate convex hull
	require(geosphere)
	lonlat = vector('list', length(regs))
	for(i in 1:length(regs)){
		j = data$region == regs[i]
		bd = chull(data$lat[j], data$lon[j]) # the indices for the convex hull bounds
		lonlat[[i]] = data[j,c('lon', 'lat')][bd,]
	}
	
quartz(width=10, height=6.5) # btemp
# pdf(width=10, height=6.5, file=paste('Figures/surveymaps_polygons_', Sys.Date(), '.pdf', sep=''))
par(mai=c(0.3, 0.2, 0.2, 0.1), mgp = c(2.5, 0.8, 0), omi=c(0.5, 0.5, 0, 0))
layout(mat=matrix(c(1,2,3,4,5,6,7,4,8,9,10,11), byrow=TRUE, nrow=3))
for(r in 1:length(regs)){
	inds = data$region == regs[r]
	plot(0, 0, pch=16, cex=0.2, main=regsnice[r], xlab='', ylab='', col=rgb(0.2, 0.2, 0.2, 0.5), ylim=ylims[[r]], xlim=xlims[[r]])
	polygon(lonlat[[r]]$lon, lonlat[[r]]$lat, col='grey', border=NA)
	map2('world', xlim=range(data$lon[inds]), ylim=range(data$lat[inds]), fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, add=TRUE)
	mtext(xlabs[r], 1, line=2.8)
	mtext(ylabs[r], 2, line=2.8)
}

	dev.off()



##############################
#     Prep spppres data      #
# (which species to analyze) #
##############################
# Read in full file
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
data = read.csv('Output/data_allregions_2012-11-15.csv', row.names=1, stringsAsFactors=FALSE)
	dim(data) # 4,901,566 x 27

# Species present >= 1x pr 10x/yr
	sumna = function(x){
		if(!all(is.na(x))) return(sum(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	spplist = aggregate(list(wtcpue = data$wtcpue, pres = data$wtcpue>0), by=list(region=data$region, regspp=data$regspp, spp=data$spp, year=data$year), FUN=sumna)
	spplist = aggregate(list(wtcpue=spplist$wtcpue, pres = spplist$pres, presyr = spplist$pres>0, presyr10 = spplist$pres >= 10), by=list(region=spplist$region, regspp = spplist$regspp, spp=spplist$spp), FUN=sumna) # presyr col holds # years in which spp was present
	rownames(spplist) = 1:nrow(spplist)
	nrow(spplist) # 721 regions/spp
	
	maxreg = aggregate(list(maxpresyr=spplist$presyr), by=list(region=spplist$region), FUN=max) # max years of data in each region
	spplist = merge(spplist,maxreg)
		head(spplist)
	
	spppres1 = spplist[spplist$presyr == spplist$maxpresyr,] # take all spp present >= 1x per yr
		nrow(spppres1) # 721 taxa	
	spppres10 = spplist[spplist$presyr10 == spplist$maxpresyr,] # take all spp present >= 10x per yr
		nrow(spppres10) # 401 taxa	
		
	# Add taxonomy
	spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1) # matches taxon name to taxonomy
	
	## Merge in taxonomy to spppres dataframes
	names(spppres1)[names(spppres1)=='spp'] = 'taxon'
	names(spppres10)[names(spppres10)=='spp'] = 'taxon'
	setdiff(spppres1$taxon, spptax$taxon) # should be 0
	setdiff(spppres10$taxon, spptax$taxon) # should be 0
	
	dim(spppres1)
	spppres1 = merge(spppres1, spptax, by='taxon', all.x=TRUE)
	dim(spppres1)

	dim(spppres10)
	spppres10 = merge(spppres10, spptax, by='taxon', all.x=TRUE)
	dim(spppres10)

		# Examine rows that didn't match
			i = is.na(spppres1$kingdom)
			spppres1[i, c('taxon', 'kingdom')] # 0, good

			i = is.na(spppres10$kingdom)
			spppres10[i, c('taxon', 'kingdom')] # 0, good

	# Flag taxa that aren't specified to genus
	spppres1$use = !is.na(spppres1$genus)
		sort(unique(spppres1$taxon[!spppres1$use])) # 37 taxa to exclude
	spppres10$use = !is.na(spppres10$genus)
		sort(unique(spppres10$taxon[!spppres10$use])) # 13 taxa to exclude

# Write out
	write.csv(spppres1, paste('Output/sppres1_', Sys.Date(), '.csv', sep=''))
	write.csv(spppres10, paste('Output/sppres10_', Sys.Date(), '.csv', sep=''))

######################################
# Calculate basic summary statistics #
######################################
# Read in full file
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
data = read.csv('Output/data_allregions_2012-11-15.csv', row.names=1, stringsAsFactors=FALSE)
	dim(data) # 4,901,566 x 27
goodhauls = read.csv('Output/goodhauls_allregions_2012-11-19.csv', row.names=1, stringsAsFactors=FALSE)
	dim(goodhauls) # 72,526 x 22
spppres1 = read.csv('Output/sppres1_2012-10-16.csv', row.names=1)
spppres10 = read.csv('Output/sppres10_2012-10-16.csv', row.names=1)

regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall') # the regions we analyze in the paper

# Tows
sum(!duplicated(goodhauls[goodhauls$region %in% regs,c('region', 'haulid')])) # 60,394 tows
sum(!duplicated(data[data$region %in% regs,c('region', 'haulid')])) # 60,358 tows

# Area surveyed
require(geosphere)
areas = rep(NA, length(regs))
for(i in 1:length(regs)){
	j = goodhauls$region == regs[i]
	bd = chull(goodhauls$lat[j], goodhauls$lon[j]) # the convex hull bounds
	lonlat = goodhauls[j,c('lon', 'lat')][bd,]
	lonlat$lon[lonlat$lon < -180] = lonlat$lon[lonlat$lon < -180] + 360 # areaPolygon doesn't like lon < -180
	areas[i] = areaPolygon(lonlat)	
}
areas = data.frame(region= regs, areakm2 = areas/1000^2)
areas
	sum(areas$areakm2) # 3,339,354 km2 (convex hull approach)
	
# Depths
summary(goodhauls$depth) # 4 to 3304 m, mean 125m

# Number of individuals
sum(data$numcpue, na.rm=TRUE) # 148M individuals of all taxa in all surveys to use
sum(data$numcpue[data$region %in% regs], na.rm=TRUE) # 128,211,348 individuals of all taxa in surveys to use
sum(data$numcpue[data$spp %in% spppres1$taxon[spppres1$use] & data$region %in% regs], na.rm=TRUE) # 127,763,483 individuals of all taxa to use

# Number of taxa >=1x/yr
	sum(!duplicated(spppres1[,c('species', 'genus', 'family', 'order', 'class', 'superclass', 'subphylum', 'phylum', 'kingdom')])) # 421 total
	sum(spppres1$use & !duplicated(spppres1[,c('species', 'genus', 'family', 'order', 'class', 'superclass', 'subphylum', 'phylum', 'kingdom')])) # 393 resolved at least to genus
	sum(spppres1$region %in% regs & spppres1$use & !duplicated(spppres1[,c('species', 'genus', 'family', 'order', 'class', 'superclass', 'subphylum', 'phylum', 'kingdom')])) # 360 taxa resolved to genus and in surveys to use

# Number of taxa >=10x/yr
	sum(!duplicated(spppres10[,c('species', 'genus', 'family', 'order', 'class', 'superclass', 'subphylum', 'phylum', 'kingdom')])) # 235 total
	sum(spppres10$use & !duplicated(spppres10[,c('species', 'genus', 'family', 'order', 'class', 'superclass', 'subphylum', 'phylum', 'kingdom')])) # 224 resolved at least to genus
	sum(spppres10$region %in% regs & spppres10$use & !duplicated(spppres10[,c('species', 'genus', 'family', 'order', 'class', 'superclass', 'subphylum', 'phylum', 'kingdom')])) # 208 taxa resolved to genus and in surveys to use

# Number of taxa-regions >=1x/yr
	sum(!duplicated(spppres1$regspp)) # 721 total
	sum(spppres1$use & !duplicated(spppres1$regspp)) # 659 resolved at least to genus
	sum(spppres1$region %in% regs & spppres1$use & !duplicated(spppres1$regspp)) # 580 taxa-regions resolved to genus and in surveys to use

# Number of taxa-regions >=10x/yr
	sum(!duplicated(spppres10$regspp)) # 401 total
	sum(spppres10$use & !duplicated(spppres10$regspp)) # 381 resolved at least to genus
	sum(spppres10$region %in% regs & spppres10$use & !duplicated(spppres10$regspp)) # 333 taxa-regions resolved to genus and in surveys to use


#################################################
# Calculate basic statistics by spp/region/year #
#################################################
# Read in full file
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
data = read.csv('Output/data_allregions_2012-11-15.csv', row.names=1, stringsAsFactors=FALSE)
	dim(data) # 4,901,566 x 27
spppres1 = read.csv('Output/sppres1_2012-10-16.csv', row.names=1)


# Histograms of spp tow abundances
	quartz(width = 10, height = 8)
	# pdf(paste('Figures/wtcpue hist by spp_', Sys.Date(), '.pdf', sep=''), width=10, height=8)
	regs = sort(unique(data$region))
	mn = numeric(0)
	for(k in 1:length(regs)){ # for each region
		indreg = data$region == regs[k]
		spp = sort(unique(data$spp[indreg]))
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
		for(i in 1:length(spp)){ # for each spp in this region
			print(paste(regs[k], spp[i]))
			dat = data$wtcpue[data$region==regs[k] & data$spp == spp[i] & data$wtcpue>0]			
			hist(dat, col='grey', breaks=30, main=paste(spp[i], regs[k], sep='\n'))
			mn = c(mn, mean(dat))
#			sk = c(sk,skew(dat))
		}
	}
	
	dev.off()

# Set up dataframes to hold calculations
	require(Hmisc)
	require(isotone)
	
	# list of all region/spp combinations
	taxlist = data[!duplicated(data[,c('region', 'spp')]), c('region', 'spp')]
		taxlist$regspp = paste(taxlist$region, taxlist$spp, sep='_') # unique ID for each region/spp combination
		dim(taxlist) # 721
		length(unique(taxlist$regspp)) # 721
		
	# years 
	yrs = sort(unique(data$yearsurv))

	minlat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	maxlat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	centbiolatindiv = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # not a stratum average
	centbiolatindiv2 = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # ave lat of presences
	centbiolatindiv3 = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # weighted median lat
	centbiolatindiv4 = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # ave lat of 4th root wtcpue
	sdlat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	skewlat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # skewness
	kurtlat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # kurtosis
	minlon = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	maxlon = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	centbiolonindiv = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	sdlon = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	mindepth = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	maxdepth = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	centbiodepthindiv = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	npresences = data.frame(region = sort(unique(taxlist$region))) # how many tows each species appears in each year

	# records the extent of the survey each year (lat, lon, depth, julian)
	surveyminlat = data.frame(region = sort(unique(taxlist$region)))
	surveymeanlat = data.frame(region = sort(unique(taxlist$region)))
	surveymaxlat = data.frame(region = sort(unique(taxlist$region)))
	surveyminlon = data.frame(region = sort(unique(taxlist$region)))
	surveymeanlon = data.frame(region = sort(unique(taxlist$region)))
	surveymaxlon = data.frame(region = sort(unique(taxlist$region)))
	surveymindepth = data.frame(region = sort(unique(taxlist$region)))
	surveymeandepth = data.frame(region = sort(unique(taxlist$region)))
	surveymaxdepth = data.frame(region = sort(unique(taxlist$region)))
	surveyminjulian = data.frame(region = sort(unique(taxlist$region)))
	surveymeanjulian = data.frame(region = sort(unique(taxlist$region)))
	surveymaxjulian = data.frame(region = sort(unique(taxlist$region)))
	surveymintime = data.frame(region = sort(unique(taxlist$region)))
	surveymeantime = data.frame(region = sort(unique(taxlist$region)))
	surveymaxtime = data.frame(region = sort(unique(taxlist$region)))

	# records the mean and SD of temperature each year (not stratified means)
	btmean = data.frame(region = sort(unique(taxlist$region)))
	btsd = data.frame(region = sort(unique(taxlist$region)))
	sstmean = data.frame(region = sort(unique(taxlist$region)))
	sstsd = data.frame(region = sort(unique(taxlist$region)))

	# useful function: acts like min(na.rm=T) but returns NA if all are NA
	minna = function(x){
		if(!all(is.na(x))) return(min(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}

	meanna = function(x){
		if(!all(is.na(x))) return(mean(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}
	maxna = function(x){
		if(!all(is.na(x))) return(max(x, na.rm=T))
		if(all(is.na(x))) return(NA)
	}
	
	# values in col 1, weights in col 2
	wgtmean = function(x, na.rm=FALSE) wtd.mean(x=x[,1], weights=x[,2], na.rm=na.rm)
	wgtmeanpres = function(x, na.rm=FALSE) wtd.mean(x=x[,1], weights=x[,2]>0, na.rm=na.rm) # only considers whether weight >0
	wgtmedian = function(x) weighted.median(y=x[,1], w=x[,2]) # needs isotone package
	wgtmeanpow = function(x, na.rm=FALSE, pow=1) wtd.mean(x=x[,1], weights=x[,2]^pow, na.rm=na.rm)
	wgtsd = function(mat, ...){
		x = mat[,1][mat[,2]>0] # trim to values with weight>0
		w = mat[,2][mat[,2]>0]
		sqrt(wtd.var(x=x, weights=w, ...))
	}
	wgtskew = function(mat, na.rm=FALSE){ # SAS: http://support.sas.com/documentation/cdl/en/proc/61895/HTML/default/viewer.htm#a002473330.htm
		x = mat[,1][mat[,2]>0] # trim to values with weight>0
		w = mat[,2][mat[,2]>0]
		if(na.rm){
			s = !is.na(x+w)
			x = x[s]
			w = w[s]
		}
		n = length(x)
		w = n*w/sum(w) # normalize
		if(n>2){
			c3 = n/((n-1)*(n-2))
			sdv = wgtsd(cbind(x,w), normwt=TRUE, na.rm=na.rm)
			xbar = wtd.mean(x, w, na.rm=na.rm)
			sk = c3*sum(w^(3/2)*((x-xbar)/sdv)^3)
			return(sk)
		} else {
			return(NA)
		}
	}
	wgtkurt = function(mat, na.rm=FALSE){ # SAS: http://support.sas.com/documentation/cdl/en/proc/61895/HTML/default/viewer.htm#a002473330.htm
		x = mat[,1][mat[,2]>0] # trim to values with weight>0
		w = mat[,2][mat[,2]>0]
		if(na.rm){
			s = !is.na(x+w)
			x = x[s]
			w = w[s]
		}
		n = length(x)
		w = n*w/sum(w)
		if(n>3){
			c4 = n*(n+1)/((n-1)*(n-2)*(n-3))
			suf = 3*(n-1)^2/((n-2)*(n-3))
			sdv = wgtsd(cbind(x,w), normwt=TRUE, na.rm=na.rm)
			xbar = wtd.mean(x, w, na.rm=na.rm)
			sk = c4*sum(w^2*((x-xbar)/sdv)^4) - suf
			return(sk)
		} else {
			return(NA)
		}
	}
	
# Prep the time data (parse time column)
	datatime = data.frame(time = data$time)
	t = unlist(strsplit(data$time, split=':', fixed=TRUE))
	datatime$hrs = as.numeric(t[seq(1, length(t), by=2)])
		range(datatime$hrs)
	datatime$mins = as.numeric(t[seq(2, length(t), by=2)])
		range(datatime$mins)
	datatime$julianmins = datatime$hrs * 60 + datatime$mins			
		range(datatime$julianmins)
			
# Run the calculations
	# Doesn't calculate along-shelf distance, which may be more appropriate than lat
	# print warnings as they occur
	options(warn=1)
	
	# Fill the matrices by year for mins, maxes, and moments (2nd, 3rd, 4th)
	length(yrs) #49 
	for(i in 1:length(yrs)){
		print(yrs[i])
		inds = data$yearsurv == yrs[i]
		inds2 = data$yearsurv == yrs[i] & !is.na(data$lat) & !is.na(data$wtcpue) # for lat calculations
		
		# Min lat
		temp = aggregate(data$lat[inds & data$wtcpue>0], by = list(regspp = data$regspp[inds & data$wtcpue>0]), FUN = minna)
			names(temp)[2] = yrs[i]
		minlat = merge(minlat, temp, all.x=TRUE)
	
		# Max lat: only use lats where spp is present
		temp = aggregate(data$lat[inds & data$wtcpue>0], by = list(regspp = data$regspp[inds & data$wtcpue>0]), FUN = maxna)
			names(temp)[2] = yrs[i]
		maxlat = merge(maxlat, temp, all.x=TRUE)
	
		# Center of biomass lat, by individual tows
		temp = summarize(data[inds, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		centbiolatindiv = merge(centbiolatindiv, temp, all.x=TRUE)

		# Center of biomass lat, by individual tows (presence/absence)
		temp = summarize(data[inds2, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds2]), FUN = wgtmeanpres)
			names(temp)[2] = yrs[i]
		centbiolatindiv2 = merge(centbiolatindiv2, temp, all.x=TRUE)

		# Center of biomass lat, by individual tows (weighted median)
		temp = summarize(data[inds2, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds2]), FUN = wgtmedian)
			names(temp)[2] = yrs[i]
			temp$regspp = gsub('.[[:digit:]]*$', '', temp$regspp) # remove strange numbers at end of regspp
		centbiolatindiv3 = merge(centbiolatindiv3, temp, all.x=TRUE)

		# Center of biomass lat, by individual tows (4th root)
		temp = summarize(data[inds2, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds2]), FUN = wgtmeanpow, pow=1/4)
			names(temp)[2] = yrs[i]
		centbiolatindiv4 = merge(centbiolatindiv4, temp, all.x=TRUE)

		# SD lat, weighted by biomass
		temp = summarize(data[inds, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtsd, normwt=TRUE) # need normwt so that sample size is length of x, not sum(weights)
			names(temp)[2] = yrs[i]
		sdlat = merge(sdlat, temp, all.x=TRUE)

		# Skewness lat, weighted by biomass
		temp = summarize(data[inds, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtskew)
			names(temp)[2] = yrs[i]
		skewlat = merge(skewlat, temp, all.x=TRUE)

		# Kurtosis lat, weighted by biomass
		temp = summarize(data[inds, c('lat', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtkurt)
			names(temp)[2] = yrs[i]
		kurtlat = merge(kurtlat, temp, all.x=TRUE)

		# Min lon
		temp = aggregate(data$lon[inds & data$wtcpue>0], by = list(regspp = data$regspp[inds & data$wtcpue>0]), FUN = minna)
			names(temp)[2] = yrs[i]
		minlon = merge(minlon, temp, all.x=TRUE)

		# Max lon
		temp = aggregate(data$lon[inds & data$wtcpue>0], by = list(regspp = data$regspp[inds & data$wtcpue>0]), FUN = maxna)
			names(temp)[2] = yrs[i]
		maxlon = merge(maxlon, temp, all.x=TRUE)
	
		# Center of biomass lon, by individual tows
		temp = summarize(data[inds, c('lon', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		centbiolonindiv = merge(centbiolonindiv, temp, all.x=TRUE)

		# SD lon, weighted by biomass
		temp = summarize(data[inds, c('lon', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtsd, normwt=TRUE) # need normwt so that sample size is length of x, not sum(weights)
			names(temp)[2] = yrs[i]
		sdlon = merge(sdlon, temp, all.x=TRUE)

		# Min depth
		temp = aggregate(data$depth[inds & data$wtcpue>0], by = list(regspp = data$regspp[inds & data$wtcpue>0]), FUN = minna)
			names(temp)[2] = yrs[i]
		mindepth = merge(mindepth, temp, all.x=TRUE)

		# Max depth
		temp = aggregate(data$depth[inds & data$wtcpue>0], by = list(regspp = data$regspp[inds & data$wtcpue>0]), FUN = maxna)
			names(temp)[2] = yrs[i]
		maxdepth = merge(maxdepth, temp, all.x=TRUE)

		# Center of biomass depth, by individual tows
		temp = summarize(data[inds, c('depth', 'wtcpue')], by = list(regspp = data$regspp[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		centbiodepthindiv = merge(centbiodepthindiv, temp, all.x=TRUE)

		# Full survey extent
			# minlat
		temp = aggregate(data$lat[inds], by = list(region = data$region[inds]), FUN = minna)
			names(temp)[2] = yrs[i]
		surveyminlat = merge(surveyminlat, temp, all.x=TRUE)
			# meanlat
		temp = aggregate(data$lat[inds], by = list(region = data$region[inds]), FUN = meanna)
			names(temp)[2] = yrs[i]
		surveymeanlat = merge(surveymeanlat, temp, all.x=TRUE)
			# maxlat
		temp = aggregate(data$lat[inds], by = list(region = data$region[inds]), FUN = maxna)
			names(temp)[2] = yrs[i]
		surveymaxlat = merge(surveymaxlat, temp, all.x=TRUE)
			# minlon
		temp = aggregate(data$lon[inds], by = list(region = data$region[inds]), FUN = minna)
			names(temp)[2] = yrs[i]
		surveyminlon = merge(surveyminlon, temp, all.x=TRUE)
			# meanlon
		temp = aggregate(data$lon[inds], by = list(region = data$region[inds]), FUN = meanna)
			names(temp)[2] = yrs[i]
		surveymeanlon = merge(surveymeanlon, temp, all.x=TRUE)
			# maxlon
		temp = aggregate(data$lon[inds], by = list(region = data$region[inds]), FUN = maxna)
			names(temp)[2] = yrs[i]
		surveymaxlon = merge(surveymaxlon, temp, all.x=TRUE)
			# mindepth
		temp = aggregate(data$depth[inds], by = list(region = data$region[inds]), FUN = minna)
			names(temp)[2] = yrs[i]
		surveymindepth = merge(surveymindepth, temp, all.x=TRUE)
			# meandepth
		temp = aggregate(data$depth[inds], by = list(region = data$region[inds]), FUN = meanna)
			names(temp)[2] = yrs[i]
		surveymeandepth = merge(surveymeandepth, temp, all.x=TRUE)
			# maxdepth
		temp = aggregate(data$depth[inds], by = list(region = data$region[inds]), FUN = maxna)
			names(temp)[2] = yrs[i]
		surveymaxdepth = merge(surveymaxdepth, temp, all.x=TRUE)

			# minjulian
		temp = aggregate(data$juliansurv[inds], by = list(region = data$region[inds]), FUN = minna)
			names(temp)[2] = yrs[i]
		surveyminjulian = merge(surveyminjulian, temp, all.x=TRUE)
			# meanjulian
		temp = aggregate(data$juliansurv[inds], by = list(region = data$region[inds]), FUN = meanna)
			names(temp)[2] = yrs[i]
		surveymeanjulian = merge(surveymeanjulian, temp, all.x=TRUE)
			# maxjulian
		temp = aggregate(data$juliansurv[inds], by = list(region = data$region[inds]), FUN = maxna)
			names(temp)[2] = yrs[i]
		surveymaxjulian = merge(surveymaxjulian, temp, all.x=TRUE)

			# min time of day (in minutes since midnight)
		temp = aggregate(datatime$julianmins[inds], by = list(region = data$region[inds]), FUN = minna)
			names(temp)[2] = yrs[i]
		surveymintime = merge(surveymintime, temp, all.x=TRUE)
			# mean time of day
		temp = aggregate(datatime$julianmins[inds], by = list(region = data$region[inds]), FUN = meanna)
			names(temp)[2] = yrs[i]
		surveymeantime = merge(surveymeantime, temp, all.x=TRUE)
			# max time of day
		temp = aggregate(datatime$julianmins[inds], by = list(region = data$region[inds]), FUN = maxna)
			names(temp)[2] = yrs[i]
		surveymaxtime = merge(surveymaxtime, temp, all.x=TRUE)

		# Temperature
			# mean BT
		temp = aggregate(data$bottemp[inds], by = list(region = data$region[inds]), FUN = mean, na.rm=T)
			names(temp)[2] = yrs[i]
		btmean = merge(btmean, temp, all.x=TRUE)
			# sd BT
		temp = aggregate(data$bottemp[inds], by = list(region = data$region[inds]), FUN = sd, na.rm=T)
			names(temp)[2] = yrs[i]
		btsd = merge(btsd, temp, all.x=TRUE)
			# mean SST
		temp = aggregate(data$surftemp[inds], by = list(region = data$region[inds]), FUN = mean, na.rm=T)
			names(temp)[2] = yrs[i]
		sstmean = merge(sstmean, temp, all.x=TRUE)
			# sd SST
		temp = aggregate(data$surftemp[inds], by = list(region = data$region[inds]), FUN = sd, na.rm=T)
			names(temp)[2] = yrs[i]
		sstsd = merge(sstsd, temp, all.x=TRUE)

	}

	# npresences
	temp = aggregate(list(count = (data$wtcpue>0)), by = list(regspp = data$regspp, region = data$region, spp = data$spp, year = data$yearsurv), FUN=sum)
	npresences = reshape(data=temp, direction='wide', v.names='count', idvar = c('regspp', 'spp', 'region'), timevar = 'year')
		names(npresences) = gsub("count.", "X", names(npresences), fixed=TRUE)
	
# Write out
	write.csv(minlat, paste('Output/minlat_', Sys.Date(), '.csv', sep=''))
	write.csv(maxlat, paste('Output/maxlat_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiolatindiv, paste('Output/centbiolatindiv_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiolatindiv2, paste('Output/centbiolatindiv2_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiolatindiv3, paste('Output/centbiolatindiv3_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiolatindiv4, paste('Output/centbiolatindiv4_', Sys.Date(), '.csv', sep=''))
	write.csv(sdlat, paste('Output/sdlat_', Sys.Date(), '.csv', sep=''))
	write.csv(skewlat, paste('Output/skewlat_', Sys.Date(), '.csv', sep=''))
	write.csv(kurtlat, paste('Output/kurtlat_', Sys.Date(), '.csv', sep=''))
	write.csv(minlon, paste('Output/minlon_', Sys.Date(), '.csv', sep=''))
	write.csv(maxlon, paste('Output/maxlon_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiolonindiv, paste('Output/centbiolonindiv_', Sys.Date(), '.csv', sep=''))
	write.csv(sdlon, paste('Output/sdlon_', Sys.Date(), '.csv', sep=''))
	write.csv(mindepth, paste('Output/mindepth_', Sys.Date(), '.csv', sep=''))
	write.csv(maxdepth, paste('Output/maxdepth_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiodepthindiv, paste('Output/centbiodepthindiv_', Sys.Date(), '.csv', sep=''))
	write.csv(surveyminlat, paste('Output/surveyminlat_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymeanlat, paste('Output/surveymeanlat_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymaxlat, paste('Output/surveymaxlat_', Sys.Date(), '.csv', sep=''))
	write.csv(surveyminlon, paste('Output/surveyminlon_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymeanlon, paste('Output/surveymeanlon_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymaxlon, paste('Output/surveymaxlon_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymindepth, paste('Output/surveymindepth_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymeandepth, paste('Output/surveymeandepth_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymaxdepth, paste('Output/surveymaxdepth_', Sys.Date(), '.csv', sep=''))
	write.csv(surveyminjulian, paste('Output/surveyminjulian_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymeanjulian, paste('Output/surveymeanjulian_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymaxjulian, paste('Output/surveymaxjulian_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymintime, paste('Output/surveymintime_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymeantime, paste('Output/surveymeantime_', Sys.Date(), '.csv', sep=''))
	write.csv(surveymaxtime, paste('Output/surveymaxtime_', Sys.Date(), '.csv', sep=''))
	write.csv(btmean, paste('Output/btmean_', Sys.Date(), '.csv', sep=''))
	write.csv(btsd, paste('Output/btsd_', Sys.Date(), '.csv', sep=''))
	write.csv(sstmean, paste('Output/sstmean_', Sys.Date(), '.csv', sep=''))
	write.csv(sstsd, paste('Output/sstsd_', Sys.Date(), '.csv', sep=''))
	write.csv(npresences, paste('Output/npresences_', Sys.Date(), '.csv', sep=''))


# Calculate julian-day corrected temperatures following Mueter & Litzow
	# records the mean temperature each year
	btmeanmuet = data.frame(region = sort(unique(taxlist$region)))
	sstmeanmuet = btmeanmuet
		for(i in 1:length(yrs)){
			btmeanmuet[[as.character(yrs[i])]] = rep(NA, nrow(btmeanmuet))
			sstmeanmuet[[as.character(yrs[i])]] = rep(NA, nrow(sstmeanmuet))
		}

	require(mgcv)
	for(i in 1:nrow(btmeanmuet)){
		print(as.character(btmeanmuet$region[i]))
		inds = !duplicated(data$haulid) & data$region == btmeanmuet$region[i]
		if(sum(!is.na(data$bottemp[inds]))>10){ # need enough data to fit a model
			btempgam = gam(bottemp ~ s(lon, lat) + s(juliansurv) + as.factor(yearsurv), data=data, subset=inds)
				# summary(btempgam)
				# plot(btempgam, pages=1, se=TRUE, all.terms=TRUE)
			c = coef(btempgam)
			j = grep('yearsurv', names(c))
			names(c) = gsub('as.factor(yearsurv)', '', names(c), fixed=TRUE)
			names(c)[1] = min(data$yearsurv[inds])
			btemp = c(c[1], c[j] + c[1]) # first is the intercept, and it's also the first year
				for(k in 1:length(btemp)){
					btmeanmuet[[names(btemp)[k]]][i] = btemp[k]
				}
		}
			
		if(sum(!is.na(data$surftemp[inds]))>10){ # need enough data to fit a model
			stempgam = gam(surftemp ~ s(lon,lat) + s(juliansurv) + as.factor(yearsurv), data=data, subset=inds)
			#	summary(stempgam)
			#	plot(stempgam, pages=1, se=TRUE, all.terms=TRUE)
			c = coef(stempgam)
			j = grep('yearsurv', names(c))
			names(c) = gsub('as.factor(yearsurv)', '', names(c), fixed=TRUE)
			names(c)[1] = min(data$yearsurv[inds])
			sstemp = c(c[1], c[j] + c[1]) # first is the intercept, and it's also the first year
				for(k in 1:length(sstemp)){
					sstmeanmuet[[names(sstemp)[k]]][i] = sstemp[k]
				}
		}		
	}

	btmeanmuet
	sstmeanmuet

	# Write out
	write.csv(btmeanmuet, paste('Output/btmeanmuet_', Sys.Date(), '.csv', sep=''))
	write.csv(sstmeanmuet, paste('Output/sstmeanmuet_', Sys.Date(), '.csv', sep=''))


# Calculate stratum averages for calculation of stratified mean lat/lon/biomass

	# mean lat/lon/area for each stratum: TIME-CONSUMING
	datastrat = aggregate(list(lat = data$lat, lon = data$lon, depth = data$depth, stratarea = data$stratarea), by=list(region = data$region, stratum = data$stratum), FUN=meanna)
		dim(datastrat) # 477 regions*strata

	# mean wtcpue in each stratum/yr/spp: TIME-CONSUMING
	datastratyr = aggregate(list(wtcpue = data$wtcpue, bottemp = data$bottemp, surftemp = data$surftemp), by=list(regspp = data$regspp, region = data$region, spp = data$spp, stratum = data$stratum, yearsurv=data$yearsurv), FUN=meanna)
		dim(datastratyr) # 628815
		
		datastratyr = merge(datastratyr, datastrat) # add stratum lat/lon/depth/area
		dim(datastratyr)
		
		datastratyr$wttot = datastratyr$wtcpue * datastratyr$stratarea # total estimated biomass per stratum: wtcpue times area

	# dataframes to fill
	centbiolat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	sdlatstrat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp) # weighted standard deviation of position
	centbiolon = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	sdlonstrat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	centbiodepth = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	sddepthstrat = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)
	biomass = data.frame(regspp = taxlist$regspp, region=taxlist$region, spp = taxlist$spp)

	# records the mean and SD of temperature each year (stratified means)
	btmeanstrat = data.frame(region = sort(unique(taxlist$region)))
	btsdstrat = data.frame(region = sort(unique(taxlist$region)))
	sstmeanstrat = data.frame(region = sort(unique(taxlist$region)))
	sstsdstrat = data.frame(region = sort(unique(taxlist$region)))

	# run the calculations
	for(i in 1:length(yrs)){
		print(yrs[i])
		inds = datastratyr$yearsurv == yrs[i]
	
		# Center of biomass lat
		temp = summarize(datastratyr[inds, c('lat', 'wttot')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtmean) # have to use the Hmisc function since input is a dataframe
			names(temp)[2] = yrs[i]
		centbiolat = merge(centbiolat, temp, all.x=TRUE)

		# Stratified SD of lat
		temp = summarize(datastratyr[inds, c('lat', 'wttot')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtsd, normwt=TRUE)
			names(temp)[2] = yrs[i]
		sdlatstrat = merge(sdlatstrat, temp, all.x=TRUE)

		# Center of biomass lon
		temp = summarize(datastratyr[inds, c('lon', 'wttot')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		centbiolon = merge(centbiolon, temp, all.x=TRUE)

		# Stratified SD of lon
		temp = summarize(datastratyr[inds, c('lon', 'wttot')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtsd, normwt=TRUE)
			names(temp)[2] = yrs[i]
		sdlonstrat = merge(sdlonstrat, temp, all.x=TRUE)

		# Center of biomass depth
		temp = summarize(datastratyr[inds, c('depth', 'wttot')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		centbiodepth = merge(centbiodepth, temp, all.x=TRUE)

		# Stratified SD of depth
		temp = summarize(datastratyr[inds, c('depth', 'wttot')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtsd, normwt=TRUE)
			names(temp)[2] = yrs[i]
		sddepthstrat = merge(sddepthstrat, temp, all.x=TRUE)

		# Biomass
		temp = summarize(datastratyr[inds, c('wtcpue', 'stratarea')], by = list(regspp = datastratyr$regspp[inds]), FUN = wgtmean) 
			names(temp)[2] = yrs[i]
		biomass = merge(biomass, temp, all.x=TRUE)

		# BT mean
		temp = summarize(datastratyr[inds, c('bottemp', 'stratarea')], by = list(region = datastratyr$region[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		btmeanstrat = merge(btmeanstrat, temp, all.x=TRUE)

		# BT sd
		temp = summarize(datastratyr[inds, c('bottemp', 'stratarea')], by = list(region = datastratyr$region[inds]), FUN = wgtsd, normwt=TRUE)
			names(temp)[2] = yrs[i]
		btsdstrat = merge(btsdstrat, temp, all.x=TRUE)
		
		# SST mean
		temp = summarize(datastratyr[inds, c('surftemp', 'stratarea')], by = list(region = datastratyr$region[inds]), FUN = wgtmean)
			names(temp)[2] = yrs[i]
		sstmeanstrat = merge(sstmeanstrat, temp, all.x=TRUE)

		# SST sd
		temp = summarize(datastratyr[inds, c('surftemp', 'stratarea')], by = list(region = datastratyr$region[inds]), FUN = wgtsd, normwt=TRUE)
			names(temp)[2] = yrs[i]
		sstsdstrat = merge(sstsdstrat, temp, all.x=TRUE)
	}


# Write out
	write.csv(centbiolat, paste('Output/centbiolat_', Sys.Date(), '.csv', sep=''))
	write.csv(sdlatstrat, paste('Output/sdlatstrat_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiolon, paste('Output/centbiolon_', Sys.Date(), '.csv', sep=''))
	write.csv(sdlonstrat, paste('Output/sdlonstrat_', Sys.Date(), '.csv', sep=''))
	write.csv(centbiodepth, paste('Output/centbiodepth_', Sys.Date(), '.csv', sep=''))
	write.csv(sddepthstrat, paste('Output/sddepthstrat_', Sys.Date(), '.csv', sep=''))
	write.csv(biomass, paste('Output/biomass_', Sys.Date(), '.csv', sep=''))
	write.csv(btmeanstrat, paste('Output/btmeanstrat_', Sys.Date(), '.csv', sep=''))
	write.csv(btsdstrat, paste('Output/btsdstrat_', Sys.Date(), '.csv', sep=''))
	write.csv(sstmeanstrat, paste('Output/sstmeanstrat_', Sys.Date(), '.csv', sep=''))
	write.csv(sstsdstrat, paste('Output/sstsdstrat_', Sys.Date(), '.csv', sep=''))


################################
## Basic plots of annual data ##
################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
require(RColorBrewer)
minlat = read.csv('Output/minlat_2012-10-16.csv', row.names=1)
maxlat = read.csv('Output/maxlat_2012-10-16.csv', row.names=1)
centbiolatindiv = read.csv('Output/centbiolatindiv_2012-10-16.csv', row.names=1)
centbiolatindiv2 = read.csv('Output/centbiolatindiv2_2012-10-16.csv', row.names=1)
centbiolatindiv3 = read.csv('Output/centbiolatindiv3_2012-10-16.csv', row.names=1)
centbiolatindiv4 = read.csv('Output/centbiolatindiv4_2012-10-16.csv', row.names=1)
centbiolat = read.csv('Output/centbiolat_2012-10-16.csv', row.names=1)
sdlat = read.csv('Output/sdlat_2012-10-16.csv', row.names=1)
skewlat = read.csv('Output/skewlat_2012-10-16.csv', row.names=1)
kurtlat = read.csv('Output/kurtlat_2012-10-16.csv', row.names=1)
minlon = read.csv('Output/minlon_2012-10-16.csv', row.names=1)
maxlon = read.csv('Output/maxlon_2012-10-16.csv', row.names=1)
centbiolonindiv = read.csv('Output/centbiolonindiv_2012-10-16.csv', row.names=1)
centbiolon = read.csv('Output/centbiolon_2012-10-16.csv', row.names=1)
mindepth = read.csv('Output/mindepth_2012-11-15.csv', row.names=1)
maxdepth = read.csv('Output/maxdepth_2012-11-15.csv', row.names=1)
centbiodepthindiv = read.csv('Output/centbiodepthindiv_2012-11-15.csv', row.names=1)
centbiodepth = read.csv('Output/centbiodepth_2012-11-15.csv', row.names=1)
surveyminlat = read.csv('Output/surveyminlat_2012-10-16.csv', row.names=1)
surveymeanlat = read.csv('Output/surveymeanlat_2012-10-16.csv', row.names=1)
surveymaxlat = read.csv('Output/surveymaxlat_2012-10-16.csv', row.names=1)
surveyminlon = read.csv('Output/surveyminlon_2012-10-16.csv', row.names=1)
surveymeanlon = read.csv('Output/surveymeanlon_2012-10-16.csv', row.names=1)
surveymaxlon = read.csv('Output/surveymaxlon_2012-10-16.csv', row.names=1)
surveymindepth = read.csv('Output/surveymindepth_2012-11-15.csv', row.names=1)
surveymeandepth = read.csv('Output/surveymeandepth_2012-11-15.csv', row.names=1)
surveymaxdepth = read.csv('Output/surveymaxdepth_2012-11-15.csv', row.names=1)
surveyminjulian = read.csv('Output/surveyminjulian_2012-10-16.csv', row.names=1)
surveymeanjulian = read.csv('Output/surveymeanjulian_2012-10-16.csv', row.names=1)
surveymaxjulian = read.csv('Output/surveymaxjulian_2012-10-16.csv', row.names=1)
surveymintime = read.csv('Output/surveymintime_2012-10-16.csv', row.names=1)
surveymeantime = read.csv('Output/surveymeantime_2012-10-16.csv', row.names=1)
surveymaxtime = read.csv('Output/surveymaxtime_2012-10-16.csv', row.names=1)
biomass = read.csv('Output/biomass_2012-10-16.csv', row.names=1)
btmean = read.csv('Output/btmean_2012-10-16.csv', row.names=1)
btmeanstrat = read.csv('Output/btmeanstrat_2012-10-16.csv', row.names=1)
#btmeanmuet = read.csv('Output/btmeanmuet_2012-????.csv', row.names=1)
sstmean = read.csv('Output/sstmean_2012-10-16.csv', row.names=1)
sstmeanstrat = read.csv('Output/sstmeanstrat_2012-10-16.csv', row.names=1)
#sstmeanmuet = read.csv('Output/sstmeanmuet_2012-08-04.csv', row.names=1)
spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1) # matches taxon name to taxonomy
spppres1 = read.csv('Output/sppres1_2012-11-21.csv', row.names=1)


	# Make sure all dataframes in same order by region and spp
	all(minlat$regspp == maxlat$regspp)
	all(minlat$regspp == centbiolat$regspp)
	all(minlat$regspp == centbiolatindiv$regspp)
	all(minlat$regspp == sdlat$regspp)

	all(minlat$regspp == minlon$regspp)
	all(minlat$regspp == maxlon$regspp)
	all(minlat$regspp == centbiolon$regspp)
	all(minlat$regspp == centbiolonindiv$regspp)

	all(minlat$regspp == mindepth$regspp)
	all(minlat$regspp == maxdepth$regspp)
	all(minlat$regspp == centbiodepth$regspp)
	all(minlat$regspp == centbiodepthindiv$regspp)

	# Make sure all dataframes in same order by region
	all(btmean$region == btmeanstrat$region)
	all(btmean$region == sstmean$region)
	all(btmean$region == sstmeanstrat$region)

# How many species fill their study region?
	ltol = 0 # tolerance for latitude: how close to the edge can a species get? 0.2° is about 22km
	dtol = 0 # tolerance for depth in m

	# TRUE or FALSE for each species: fill to edge of survey in any year?
	fillsurvey = data.frame(spp = minlat$spp, region = minlat$region, regspp = minlat$regspp, minlat = rep(NA, nrow(minlat)),  maxlat = rep(NA, nrow(minlat)),  mindepth = rep(NA, nrow(minlat)),  maxdepth = rep(NA, nrow(minlat)))

	lcols = grep('X', names(minlat))
	scols = grep('X', names(surveyminlat))
	for(i in 1:nrow(minlat)){
		j = which(surveyminlat$region == fillsurvey$region[i])
		fillsurvey$minlat[i] = any(minlat[i,lcols] <= surveyminlat[j,scols] + ltol) # true if spp is within ltol of survey edge in any year, NA otherwise
		fillsurvey$maxlat[i] = any(maxlat[i,lcols] >= surveymaxlat[j,scols] - ltol) # true if spp is within ltol of survey edge in any year
		fillsurvey$mindepth[i] = any(mindepth[i,lcols] <= surveymindepth[j,scols] + dtol) # true if spp is within ltol of survey edge in any year
		fillsurvey$maxdepth[i] = any(maxdepth[i,lcols] >= surveymaxdepth[j,scols] - dtol) # true if spp is within ltol of survey edge in any year
	}
	fillsurvey[is.na(fillsurvey)] = FALSE

	# Trim to species and regions I'm using (resolve to genus)
	dim(fillsurvey) # 721
	regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')
	fillsurvey = fillsurvey[as.character(fillsurvey$spp) %in% as.character(spppres1$taxon[spppres1$use]) & fillsurvey$region %in% regs,]
	dim(fillsurvey) # 580
	
	table(fillsurvey$minlat, fillsurvey$maxlat)
		table(fillsurvey$minlat, fillsurvey$maxlat, fillsurvey$region)
	table(fillsurvey$mindepth, fillsurvey$maxdepth)
		table(fillsurvey$mindepth, fillsurvey$maxdepth, fillsurvey$region)

# Plot mean lat of sampling through time for all regions
	require(RColorBrewer)
	cols = brewer.pal(n=nrow(surveymeanlat), name='Set3'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	#cols = rep('black', 6) # for black
	yrs = as.numeric(gsub("X", "", names(surveymeanlat))[2:ncol(surveymeanlat)])
	tcols = grep('[[:digit:]]', names(surveymeanlat))
	ltype = 'o'
	
	quartz(width=7, height=7)
	nm = paste('Figures/surveymeanlat_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(width=7, height=7, file=nm)
	plot(0,0, xlim=range(yrs), ylim=c(20,60), col='white', xlab='Year', ylab='Mean latitude (°N)', main='Mean Latitude of Survey', cex.lab = 1.5, cex.axis=1.3)
	for(i in 1:nrow(surveymeanlat)){
		y <- as.numeric(surveymeanlat[i,tcols])
		x = yrs[!is.na(y)]; y = y[!is.na(y)]
		if(length(x)>0){
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=2, type=ltype)
			mod = lm(y ~ x)
			print(paste(surveymeanlat$region[i], ' p=', signif(summary(mod)$coef[2,4], 2), ' b=', signif(summary(mod)$coef[2,1], 2), sep=''))
			lines(x, predict(mod), col=cols[i], lty=2)
		}
	}
	legend('bottomright', legend=as.character(surveymeanlat$region), col=cols, pch=16, lwd=rep(2, nrow(surveymeanlat)), lty = rep(1,nrow(surveymeanlat)), cex=0.7, bty='n', ncol=2)
	
	dev.off()

# Plot mean depth of sampling through time for all regions
	require(RColorBrewer)
	cols = brewer.pal(n=nrow(surveymeandepth), name='Set3'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	#cols = rep('black', 6) # for black
	yrs = as.numeric(gsub("X", "", names(surveymeandepth))[2:ncol(surveymeandepth)])
	tcols = grep('[[:digit:]]', names(surveymeandepth))
	ltype = 'o'
	
	quartz(width=7, height=7)
	nm = paste('Figures/surveymeandepth_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(width=7, height=7, file=nm)
	plot(0,0, xlim=range(yrs), ylim=c(-30,300), col='white', xlab='Year', ylab='Mean depth (m)', main='Mean Depth of Survey', cex.lab = 1.5, cex.axis=1.3)
	for(i in 1:nrow(surveymeandepth)){
		y <- as.numeric(surveymeandepth[i,tcols])
		x = yrs[!is.na(y)]; y = y[!is.na(y)]
		if(length(x)>0){
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=2, type=ltype)
			mod = lm(y ~ x)
			print(paste(surveymeandepth$region[i], ' p=', signif(summary(mod)$coef[2,4], 2), ' b=', signif(summary(mod)$coef[2,1], 2), sep=''))
			lines(x, predict(mod), col=cols[i], lty=2)
		}
	}
	legend('bottomright', legend=as.character(surveymeandepth$region), col=cols, pch=16, lwd=rep(2, nrow(surveymeandepth)), lty = rep(1,nrow(surveymeandepth)), cex=0.7, bty='n', ncol=2)
	
	dev.off()

# Plot mean julian day of sampling through time for all regions
	require(RColorBrewer)
	cols = brewer.pal(n=nrow(surveymeanjulian), name='Set3'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	#cols = rep('black', 6) # for black
	yrs = as.numeric(gsub("X", "", names(surveymeanjulian))[2:ncol(surveymeanjulian)])
	tcols = grep('[[:digit:]]', names(surveymeanjulian))
	ltype = 'o'
	
	quartz(width=7, height=7)
	nm = paste('Figures/surveymeanjulian_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(width=7, height=7, file=nm)
	plot(0,0, xlim=range(yrs), ylim=c(0,350), col='white', xlab='Year', ylab='Mean day', main='Mean Julian Day of Survey (adjusted)', cex.lab = 1.5, cex.axis=1.3)
	for(i in 1:nrow(surveymeanjulian)){
		y <- as.numeric(surveymeanjulian[i,tcols])
		x = yrs[!is.na(y)]; y = y[!is.na(y)]
		if(length(x)>0){
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=2, type=ltype)
			mod = lm(y ~ x)
			print(paste(surveymeanjulian$region[i], ' p=', signif(summary(mod)$coef[2,4], 2), ' b=', signif(summary(mod)$coef[2,1], 2), sep=''))
			lines(x, predict(mod), col=cols[i], lty=2)
		}
	}
	legend('bottomright', legend=as.character(surveymeanjulian$region), col=cols, pch=16, lwd=rep(2, nrow(surveymeanjulian)), lty = rep(1,nrow(surveymeanjulian)), cex=0.7, bty='n', ncol=2)
	
	dev.off()

# Plot mean time of day (julian minutes) of sampling through the years for all regions
	require(RColorBrewer)
	cols = brewer.pal(n=nrow(surveymeantime), name='Set3'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	#cols = rep('black', 6) # for black
	yrs = as.numeric(gsub("X", "", names(surveymeantime))[2:ncol(surveymeantime)])
	tcols = grep('[[:digit:]]', names(surveymeantime))
	ltype = 'o' # for mean
	ltype2 = 'l' # for min and max
	
	quartz(width=7, height=7)
	nm = paste('Figures/surveymeantime_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(width=7, height=7, file=nm)
	plot(0,0, xlim=range(yrs), ylim=c(0,1440), col='white', xlab='Year', ylab='Minutes after midnight', main='Mean Time of Day of Survey', cex.lab = 1.5, cex.axis=1.3)
	for(i in 1:nrow(surveymeantime)){
		y <- as.numeric(surveymeantime[i,tcols])
		y1 <- as.numeric(surveymintime[i,tcols])
		y2 <- as.numeric(surveymaxtime[i,tcols])
		x = yrs[!is.na(y)]; y = y[!is.na(y)]; y1 = y1[!is.na(y1)]; y2 = y2[!is.na(y2)]
		if(length(x)>0){
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=2, type=ltype)
			points(x, y1, col=cols[i], pch=16, cex=0.5, lwd=0.5, type=ltype2)
			points(x, y2, col=cols[i], pch=16, cex=0.5, lwd=0.5, type=ltype2)
			mod = lm(y ~ x)
			print(paste(surveymeantime$region[i], ' p=', signif(summary(mod)$coef[2,4], 2), ' b=', signif(summary(mod)$coef[2,1], 2), sep=''))
			lines(x, predict(mod), col=cols[i], lty=2)
		}
	}
	legend('bottomright', legend=as.character(surveymeantime$region), col=cols, pch=16, lwd=rep(2, nrow(surveymeantime)), lty = rep(1,nrow(surveymeantime)), cex=0.7, bty='n', ncol=2)
	
	dev.off()



# Plot all temperatures through time on same plot
	cols = brewer.pal(n=nrow(btmean), name='Set3'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	#cols = rep('black', 6) # for black
	yrs = as.numeric(gsub("X", "", names(btmean))[2:ncol(btmean)])
	tcols = grep('[[:digit:]]', names(btmean))
	ltype = 'o'
	
	quartz(width=12, height=7)
	nm = paste('Figures/temperature_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(width=12, height=7, file=nm)
	par(mfrow=c(1,2), mgp=c(2.5,1,0))
	plot(0,0, xlim=range(yrs), ylim=c(-7,25), col='white', xlab='Year', ylab='Bottom temperature (°C)', main='Bottom temperature', cex.lab = 1.5, cex.axis=1.3)
	for(i in 1:nrow(btmean)){
		y <- as.numeric(btmean[i,tcols])
		x = yrs[!is.na(y)]; y = y[!is.na(y)]
		if(length(x)>0){
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=2, type=ltype)
			mod = lm(y ~ x)
			lines(x, predict(mod), col=cols[i], lty=2)

			y <- as.numeric(btmeanstrat[i,tcols]) # stratified mean temperature
			x = yrs[!is.na(y)]; y = y[!is.na(y)]
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=1, lty=3, type=ltype)
	
#			y <- as.numeric(btmeanmuet[i,tcols]) # GAM-corrected temperature
#			x = yrs[!is.na(y)]; y = y[!is.na(y)]
#			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=0.5, lty = 1, type=ltype)
		}
	}
	legend('bottomright', legend=c(as.character(btmean$region), 'Mean', 'Stratified mean', 'GAM-corrected mean'), col=c(cols, rep('black', 3)), pch=16, lwd=c(rep(2, nrow(btmean)), 2,1,0.5), lty = c(rep(1,nrow(btmean)), 1,3,1), cex=0.7, bty='n', ncol=2)

	plot(0,0, xlim=range(yrs), ylim=c(3,30), col='white', xlab='Year', ylab='Surface temperature (°C)', main='Surface temperature', cex.lab = 1.5, cex.axis=1.3)
	for(i in 1:nrow(sstmean)){
		y <- as.numeric(sstmean[i,tcols])
		x = yrs[!is.na(y)]
		y = y[!is.na(y)]
		if(length(x)>0){
			points(x, y, col=cols[i], pch=16, cex=0.5, lwd=2, type=ltype)
			mod = lm(y ~ x)
			lines(x, predict(mod), col=cols[i], lty=2)
	
		y <- as.numeric(sstmeanstrat[i,tcols]) # stratified mean temperature
		x = yrs[!is.na(y)]; y = y[!is.na(y)]
		points(x, y, col=cols[i], pch=16, cex=0.5, lwd=1, lty=3, type=ltype)

#		y <- as.numeric(sstmeanmuet[i,tcols]) # GAM-corrected temperature
#		x = yrs[!is.na(y)]; y = y[!is.na(y)]
#		points(x, y, col=cols[i], pch=16, cex=0.5, lwd=0.5, lty = 1, type=ltype)
		}
	}
	
	dev.off()

# Latitude through time for each species in each region in many graphs
	# Add taxonomy
	setdiff(centbiolatindiv$spp, spptax$taxon) # should be 0
	centbiolatindiv = merge(centbiolatindiv, spptax, by.x = 'spp', by.y = 'taxon', all.xy=TRUE)
		dim(centbiolatindiv) # 721 x 61
		unique(centbiolatindiv$kingdom) # Animalia, not NA
	
	# Add nicer region
	sort(unique(centbiolatindiv$region))
	centbiolatindiv$region2 = as.character(centbiolatindiv$region)
	centbiolatindiv$region2[centbiolatindiv$region=='AFSC_Aleutians'] = 'Aleutian Islands'
	centbiolatindiv$region2[centbiolatindiv$region=='AFSC_EBS'] = 'Eastern Bering Sea'
	centbiolatindiv$region2[centbiolatindiv$region=='AFSC_GOA'] = 'Gulf of Alaska'
	centbiolatindiv$region2[centbiolatindiv$region=='DFO_Newfoundland_Fall'] = 'Newfoundland'
	centbiolatindiv$region2[centbiolatindiv$region=='DFO_ScotianShelf'] = 'Scotian Shelf'
	centbiolatindiv$region2[centbiolatindiv$region=='DFO_SoGulf'] = 'So. Gulf of St. Lawrence'
	centbiolatindiv$region2[centbiolatindiv$region=='NEFSC_Spring'] = 'Northeast U.S.'
	centbiolatindiv$region2[centbiolatindiv$region=='SEFSC_GOMex'] = 'Gulf of Mexico'
	centbiolatindiv$region2[centbiolatindiv$region=='WestCoast_Tri'] = 'West Coast U.S.'
	centbiolatindiv = centbiolatindiv[order(centbiolatindiv$regspp),]

	all(minlat$regspp == centbiolatindiv$regspp)

	quartz(width = 10, height = 8)
	nm = paste('Figures/min,max,mean lat by spp and year_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(file=nm, width=10, height=8)
	lcol = grep('X', names(maxlat))
	#regs = sort(unique(maxlat$region)) # for all regions
	regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')
	yrs = as.numeric(gsub('X', '', names(maxlat)[lcol]))
	for(k in 1:length(regs)){ # for each region
		indreg = maxlat$region == regs[k] & (as.character(maxlat$spp) %in% as.character(spppres1$taxon[spppres1$use])) # show only species resolved at least to genus (spppres1$use)
		ylims = c(min(surveyminlat[surveyminlat$region==regs[k], grep('X', names(surveyminlat))], na.rm=T), max(surveymaxlat[surveymaxlat$region==regs[k], grep('X', names(surveymaxlat))], na.rm=T))
		tcol = which(!is.na(colSums(maxlat[indreg,lcol]))) # columns for this region
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8, omi=c(0,0.2,0.1,0), mgp=c(2.8, 0.7, 0), font.main=3)
		for(i in 1:sum(indreg)){ # for each spp in this region
			plot(yrs[tcol], maxlat[indreg,lcol][i,tcol], col='blue', type='o', xlab='', ylab='', main=centbiolatindiv$name[indreg][i], ylim = ylims, pch=16, cex=0.5, las=1)
			points(yrs[tcol], minlat[indreg,lcol][i,tcol], col='red', pch=16, cex=0.5, type='o')
			#points(yrs[tcol], centbiolat[indreg,lcol][i,tcol], col='black', pch=16, cex=0.5, type='o') # black is stratum averaged lat
			points(yrs[tcol], centbiolatindiv[indreg,lcol][i,tcol], col='black', pch=16, cex=0.5, type='o') # grey is individual-based lat
			#points(yrs[tcol], centbiolatindiv2[indreg,lcol][i,tcol], col='bisque', pch=16, cex=0.5, type='o') # bisque is pres/abs lat
			#points(yrs[tcol], centbiolatindiv3[indreg,lcol][i,tcol], col='cadetblue', pch=16, cex=0.5, type='o') # blue is weighted median lat
			#points(yrs[tcol], centbiolatindiv4[indreg,lcol][i,tcol], col='darkkhaki', pch=16, cex=0.5, type='o') # khaki is weighted mean of 4th root
			points(yrs[tcol], surveyminlat[surveyminlat$region==regs[k], grep('X', names(surveyminlat))][tcol], col='grey', lty=2, type='l')
			points(yrs[tcol], surveymaxlat[surveymaxlat$region==regs[k], grep('X', names(surveymaxlat))][tcol], col='grey', lty=2, type='l')
			#readline(prompt = "Pause. Press <Enter> to continue...") 
			if((i-1) %% 6 == 0) mtext(text='Latitude (°N)', side=2, line=2.3, cex=0.6)
			if(i %% 36 < 7) mtext(text=centbiolatindiv$region2[indreg][i], side=3, line=1.3, cex=0.6)
		}
	}
	
	dev.off()

	# SD of Latitude through time in many graphs
		quartz(width = 10, height = 8)
		# pdf(paste('Figures/sd lat by spp and year_', Sys.Date(), '.pdf', sep=''), width=10, height=8)
		lcol = grep('X', names(sdlat))
		regs = sort(unique(sdlat$region))
		yrs = as.numeric(gsub('X', '', names(sdlat)[lcol]))
		for(k in 1:length(regs)){ # for each region
			indreg = sdlat$region == regs[k]
			ylims = c(0, max(sdlat[indreg, lcol], na.rm=T))
			tcol = which(!is.na(colSums(sdlat[indreg,lcol]))) # columns for this region
			par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
			for(i in 1:sum(indreg)){ # for each spp in this region
				plot(yrs[tcol], sdlat[indreg,lcol][i,tcol], col='blue', type='o', xlab='', ylab='', main=paste(sdlat$region[indreg][i], '\n', sdlat$spp[indreg][i], sep=''), ylim = ylims, pch=16, cex=0.5)
			}
		}
		
		dev.off()

	# Max, Mean, SD, skew, kurtosis of Latitude through time in many graphs for Simon
		require(RColorBrewer)
		quartz(width = 10, height = 8)
		# pdf(paste('Figures/lat moments by spp and year_', Sys.Date(), '.pdf', sep=''), width=9, height=7)
		inds = grep('NEFSC', maxlat$region)
		lcol = grep('X', names(sdlat)) # columns that are for years
		scol = grep('X', names(surveyminlat))
		yrs = as.numeric(gsub('X', '', names(sdlat)[lcol]))
		ylimssd = c(0, max(sdlat[inds, lcol], na.rm=T))
		ylimsskew = c(0, max(skewlat[inds, lcol], na.rm=T))
		ylimskurt = c(0, max(kurtlat[inds, lcol], na.rm=T))
		ylimsmax = c(min(surveyminlat[surveyminlat$region=='NEFSC_Spring', scol], na.rm=T), max(surveymaxlat[surveymaxlat$region=='NEFSC_Spring', scol], na.rm=T))
		tcol = which(!is.na(colSums(sdlat[sdlat$region=='NEFSC_Spring',lcol]))) # columns for this region

		a1 = max(sdlat[inds,lcol][,tcol], na.rm=T)/diff(ylimsmax) # scaling factor for sd
		a2 = min(skewlat[inds,lcol][,tcol], na.rm=T) # offset factor for skew
		a22 = diff(range(skewlat[inds,lcol][,tcol], na.rm=T))/diff(ylimsmax) # scaling factor for skew
		a3 = min(kurtlat[inds,lcol][,tcol], na.rm=T) # offset factor for skew
		a32 = diff(range(kurtlat[inds,lcol][,tcol], na.rm=T))/diff(ylimsmax) # scaling factor for skew

		cols = c('black', 'red', 'blue', brewer.pal(3, 'Accent'))
		par(mfrow = c(1,1), mai=c(1, 2, 0.5, 2), cex.main=0.7, cex.axis=0.8, mgp=c(2,1,0))
		for(i in 1:length(inds)){ # for each spp in this region
			plot(yrs[tcol],centbiolat[inds,lcol][i,tcol], col=cols[1], type='l', xlab='Year', ylab='', yaxt='n', main=paste(centbiolat$region[inds][i], '\n', centbiolat$spp[inds][i], sep=''), ylim = ylimsmax, lwd=0.5)
				loess = loess.smooth(yrs[tcol],centbiolat[inds,lcol][i,tcol])
				lines(loess$x, loess$y, col=cols[1], lwd=2)
				axis(2, col=cols[1], col.ticks = cols[1], col.axis=cols[1], line=0)
				mtext(side=2, 'Latitude (°N)', col=cols[1], line=1.75)
			lines(yrs[tcol], maxlat[inds,lcol][i,tcol], col=cols[2], lwd=0.5)
				loess = loess.smooth(yrs[tcol],maxlat[inds,lcol][i,tcol])
				lines(loess$x, loess$y, col=cols[2], lwd=2)
			lines(yrs[tcol], minlat[inds,lcol][i,tcol], col=cols[3], lwd=0.5)
				loess = loess.smooth(yrs[tcol],minlat[inds,lcol][i,tcol])
				lines(loess$x, loess$y, col=cols[3], lwd=2)
			y1 = sdlat[inds,lcol][i,tcol]
			y1 = y1/a1 + ylimsmax[1] # rescale to plot with y1
			lines(yrs[tcol], y1, col=cols[4], lwd=0.5)
				loess = loess.smooth(yrs[tcol], y1)
				lines(loess$x, loess$y, col=cols[4], lwd=2)
				at = seq(ylimsmax[1], ylimsmax[2], length.out=4)
				axis(4, at=at, labels = signif((at-ylimsmax[1])*a1,2), col=cols[4], col.ticks = cols[4], col.axis=cols[4], line=0)
				mtext(side=4, line=1.75, 'Standard deviation', col=cols[4])
			y2 = skewlat[inds,lcol][i,tcol]-a2
			y2 = y2/a22 + ylimsmax[1] # rescale to plot with y1
			lines(yrs[tcol], y2, col=cols[5], lwd=0.5)
				loess = loess.smooth(yrs[tcol], y2)
				lines(loess$x, loess$y, col=cols[5], lwd=2)
				at = seq(ylimsmax[1], ylimsmax[2], length.out=4)
				axis(4, at=at, labels = signif((at-ylimsmax[1])*a22,2), col=cols[5], col.ticks = cols[5], col.axis=cols[5], line=3)
				mtext(side=4, line=4, 'Skewness', col=cols[5])
			y3 = kurtlat[inds,lcol][i,tcol]-a3
			y3 = y3/a32 + ylimsmax[1]
			lines(yrs[tcol], y3, col=cols[5], lwd=0.5)
				loess = loess.smooth(yrs[tcol], y3)
				lines(loess$x, loess$y, col=cols[6], lwd=2)
				at = seq(ylimsmax[1], ylimsmax[2], length.out=4)
				axis(2, at=at, labels = signif((at-ylimsmax[1])*a32,2), col=cols[6], col.ticks = cols[6], col.axis=cols[6], line=3)
				mtext(side=2, line=4, 'Kurtosis', col=cols[6])
			legend('topleft', legend=c('Mean', 'Max', 'Min'), text.col=cols[1:3], bty='n')
		}
		
		dev.off()
		
# Longitude through time in many graphs
	quartz(width = 10, height = 8)
	nm = paste('Figures/min,max,mean lon by spp and year_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(nm, width=10, height=8)
	lcol = grep('X', names(maxlon))
	regs = sort(unique(maxlon$region))
	yrs = as.numeric(gsub('X', '', names(maxlon)[lcol]))
	for(k in 1:length(regs)){ # for each region
		indreg = maxlon$region == regs[k]
		ylims = c(min(surveyminlon[surveyminlon$region==regs[k], grep('X', names(surveyminlon))], na.rm=T), max(surveymaxlon[surveymaxlon$region==regs[k], grep('X', names(surveymaxlon))], na.rm=T))
		tcol = which(!is.na(colSums(maxlon[indreg,lcol]))) # columns for this region
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
		for(i in 1:sum(indreg)){ # for each spp in this region
			plot(yrs[tcol], maxlon[indreg,lcol][i,tcol], col='blue', type='o', xlab='', ylab='', main=paste(maxlon$region[indreg][i], '\n', maxlon$spp[indreg][i], sep=''), ylim = ylims, pch=16, cex=0.5)
			points(yrs[tcol], minlon[indreg,lcol][i,tcol], col='red', pch=16, cex=0.5, type='o')
			points(yrs[tcol], centbiolon[indreg,lcol][i,tcol], col='black', pch=16, cex=0.5, type='o')
			points(yrs[tcol], centbiolonindiv[indreg,lcol][i,tcol], col='grey', pch=16, cex=0.5, type='o')
			points(yrs[tcol], surveyminlon[surveyminlon$region==regs[k], grep('X', names(surveyminlon))][tcol], col='grey', lty=2, type='l')
			points(yrs[tcol], surveymaxlon[surveymaxlon$region==regs[k], grep('X', names(surveymaxlon))][tcol], col='grey', lty=2, type='l')
			#readline(prompt = "Pause. Press <Enter> to continue...") 
		}
	}
	
	dev.off()

# Depth through time in many graphs
	quartz(width = 10, height = 8)
	nm = paste('Figures/min,max,mean depth by spp and year_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(nm, width=10, height=8)
	lcol = grep('X', names(maxdepth))
	regs = sort(unique(maxdepth$region))
	yrs = as.numeric(gsub('X', '', names(maxdepth)[lcol]))
	for(k in 1:length(regs)){ # for each region
		indreg = maxdepth$region == regs[k]
		ylims = c(min(surveymindepth[surveymindepth$region==regs[k], grep('X', names(surveymindepth))], na.rm=T), max(surveymaxdepth[surveymaxdepth$region==regs[k], grep('X', names(surveymaxdepth))], na.rm=T))
		tcol = which(!is.na(colSums(maxdepth[indreg,lcol]))) # columns for this region
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
		for(i in 1:sum(indreg)){ # for each spp in this region
			plot(yrs[tcol], maxdepth[indreg,lcol][i,tcol], col='blue', type='o', xlab='', ylab='', main=paste(maxdepth$region[indreg][i], '\n', maxdepth$spp[indreg][i], sep=''), ylim = ylims, pch=16, cex=0.5)
			points(yrs[tcol], mindepth[indreg,lcol][i,tcol], col='red', pch=16, cex=0.5, type='o')
			points(yrs[tcol], centbiodepth[indreg,lcol][i,tcol], col='black', pch=16, cex=0.5, type='o') # stratum averages
			points(yrs[tcol], centbiodepthindiv[indreg,lcol][i,tcol], col='grey', pch=16, cex=0.5, type='o') # individual-based
			points(yrs[tcol], surveymindepth[surveymindepth$region==regs[k], grep('X', names(surveymindepth))][tcol], col='grey', lty=2, type='l')
			points(yrs[tcol], surveymaxdepth[surveymaxdepth$region==regs[k], grep('X', names(surveymaxdepth))][tcol], col='grey', lty=2, type='l')
			#readline(prompt = "Pause. Press <Enter> to continue...") 
		}
	}
	
	dev.off()

# Biomass through time in many graphs
	quartz(width = 10, height = 8)
	nm = paste('Figures/biomass by spp and year_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(nm, width=10, height=8)
	lcol = grep('X', names(biomass))
	regs = sort(unique(biomass$region))
	yrs = as.numeric(gsub('X', '', names(biomass)[lcol]))
	for(k in 1:length(regs)){ # for each region
		indreg = biomass$region == regs[k]
		tcol = which(!is.na(colSums(biomass[indreg,lcol]))) # columns for this region
		par(mfrow = c(6,6), mai=c(0.3, 0.3, 0.2, 0.05), cex.main=0.7, cex.axis=0.8)
		for(i in 1:sum(indreg)){ # for each spp in this region
			plot(yrs[tcol], biomass[indreg,lcol][i,tcol], type='o', xlab='', ylab='', main=paste(biomass$region[indreg][i], '\n', biomass$spp[indreg][i], sep=''), pch=16, cex=0.5)
		}
	}
	
	dev.off()



###########################
## Basic trends analysis ##
###########################
require(car)
# p-values from lm object
lmp <- function(object){
	i = summary(object)$coef
	if(nrow(i)>0){
		return(i[,ncol(i)])
	} else {
		return(numeric(0))
	}
}

# Rate of lat/lon/biomass shift calculations (by regspp)
	# Already trimmed within regions to only those spp with data in all years
	# Also calculat Durbin-Watson test for first-order temporal autocorrelation

	# Set up dataframe
	shift = data.frame(region = centbiolatindiv$region, spp = centbiolatindiv$spp)
	tcols = grep('X', names(centbiolatindiv))
	tcols = grep('199[456789]|2[[:digit:]]{3}', names(centbiolatindiv)) # for only 1994 and later
	yrs = as.numeric(gsub('X', '', names(centbiolatindiv)[tcols]))
	vars = list(minlat=minlat, maxlat=maxlat, 
	centbiolat=centbiolat, 
	centbiolatindiv = centbiolatindiv, minlon=minlon, maxlon=maxlon, 
	centbiolon=centbiolon, 
	centbiolonindiv=centbiolonindiv, mindepth=mindepth, maxdepth=maxdepth, 
	centbiodepth=centbiodepth, 
	centbiodepthindiv=centbiodepthindiv, biomass=biomass)
	mods = c('.a.lm', '.b.lm', '.p.lm', '.r.dwt', '.p.dwt')
	for(i in 1:length(vars)){
		for(j in 1:length(mods)){
			shift[[paste(names(vars)[i], mods[j], sep='')]] = NA
		}
	}
	dim(shift)
	
	# Set some parameter values
	options(warn=1) # print as they occur (0 is default)
	#options(warn=2) # stop if they occur (0 is default)
	lattol = 0.1 # how close to the edge of sampling range can a spp range limit be before I don't trust it
	lontol = 0.1 # how close to the edge of sampling range can a spp range limit be before I don't trust it
	depthtol = 10 # how close to the edge of sampling range can a spp range limit be before I don't trust it

	rcols = grep('X', names(surveyminlat))
	# Fill dataframe with model fits
	for(i in 1:nrow(centbiolatindiv)){
		if(i %% 50 == 0) print(i)
		k = surveyminlat$region == centbiolatindiv$region[i]
		
		for(j in 1:length(vars)){
			nm = names(vars)[j]
			lcols = grepl('X', names(vars[[j]])) & grepl(paste(yrs, collapse='|'), names(vars[[j]]))# columns with latitude or other data
			# Set up independent variable for regression
			y = as.numeric(vars[[j]][i,lcols])
					
			# don't calc trend if any pts close to edge of sampling frame
			if(length(grep('lat', nm))>0) fail = sum(abs(y-surveyminlat[k,rcols])<lattol, na.rm=T)>0  | sum(abs(y-surveymaxlat[k,rcols])<lattol, na.rm=T)>0

			if(length(grep('lon', nm))>0) fail = sum(abs(y-surveyminlon[k,rcols])<lontol, na.rm=T)>0  | sum(abs(y-surveymaxlon[k,rcols])<lontol, na.rm=T)>0

			if(length(grep('depth', nm))>0) fail = sum(abs(y-surveymindepth[k,rcols])<depthtol, na.rm=T)>0  | sum(abs(y-surveymaxdepth[k,rcols])<depthtol, na.rm=T)>0

			if(!fail | nm=='biomass' | grepl('centbio', nm)){ # calc biomass or center of biomass trend no matter what
				x = yrs[!is.na(y)]; y = y[!is.na(y)]
				mod = lm(y ~ x)
				d = dwt(mod)
				shift[[paste(nm, '.a.lm', sep='')]][i] = coef(mod)[1]
				shift[[paste(nm, '.b.lm', sep='')]][i] = coef(mod)[2]
				shift[[paste(nm, '.p.lm', sep='')]][i] = lmp(mod)[2]
				shift[[paste(nm, '.r.dwt', sep='')]][i] = d$r
				shift[[paste(nm, '.p.dwt', sep='')]][i] = d$p
			}
		}	
	}

	# Sample size?
	sum(!is.na(shift$maxlat.b.lm)) # 158
	sum(!is.na(shift$minlat.b.lm)) # 219
	sum(!is.na(shift$centbiolat.b.lm)) # 721
	sum(!is.na(shift$centbiolatindiv.b.lm)) # 721
	sum(!is.na(shift$centbiolon.b.lm)) # 721
	sum(!is.na(shift$centbiolonindiv.b.lm)) # 721
	sum(!is.na(shift$biomass.b.lm)) # 721

	# How many rates show temporal autocorrelation?
	sum(i <- shift$maxlat.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$maxlat.p.dwt)) # 9%
		shift[which(i), c('region', 'spp', 'maxlat.r.dwt', 'maxlat.p.dwt')]
	sum(i <- shift$minlat.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$minlat.p.dwt)) # 12%
		shift[which(i), c('region', 'spp', 'minlat.r.dwt', 'minlat.p.dwt')]
	sum(i <- shift$centbiolatindiv.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$centbiolatindiv.p.dwt)) # 14%
	sum(i <- shift$centbiolat.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$centbiolat.p.dwt)) # 14%
		shift[which(i), c('region', 'spp', 'centbiolat.r.dwt', 'centbiolat.p.dwt')]
	sum(i <- shift$centbiodepthindiv.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$centbiodepthindiv.p.dwt)) # 15%
	sum(i <- shift$centbiodepth.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$centbiodepth.p.dwt)) # 13%
	sum(i <- shift$biomass.p.dwt<0.05, na.rm=T)/sum(!is.na(shift$biomass.p.dwt)) # 31%
		shift[which(i), c('region', 'spp', 'biomass.r.dwt', 'biomass.p.dwt')]


	# Add overall average rate of shift (lat and long)
	# For lm and gls models, center of biomass and center of abundance
	shift$kmyr.bio = NA
	
	# Calculate min and max year for each row
	minyrnotna = function(x, yr) return(min(yr[!is.na(x)]))
	maxyrnotna = function(x, yr) return(max(yr[!is.na(x)]))
	regyears = data.frame(region = surveyminlat$region)
	rownames(regyears) = regyears$region
	regyears$minyr = apply(surveyminlat[,grep('X', names(surveyminlat))], MARGIN=1, FUN=minyrnotna, yr = yrs)
	regyears$maxyr = apply(surveyminlat[,grep('X', names(surveyminlat))], MARGIN=1, FUN=maxyrnotna, yr = yrs)
	shift$minyr = regyears[as.character(shift$region), 'minyr']
	shift$maxyr = regyears[as.character(shift$region), 'maxyr']

	# Mean starting and ending locations, from the trends
	lat1 = shift$centbiolatindiv.a.lm + shift$centbiolatindiv.b.lm*shift$minyr
	lat2 = shift$centbiolatindiv.a.lm + shift$centbiolatindiv.b.lm*shift$maxyr
	lon1 = shift$centbiolonindiv.a.lm + shift$centbiolonindiv.b.lm*shift$minyr
	lon2 = shift$centbiolonindiv.a.lm + shift$centbiolonindiv.b.lm*shift$maxyr
	
	# To positive radians
	lat1 = lat1/180*pi
	lat2 = lat2/180*pi
	lon1 = lon1/180*pi + 2*pi
	lon2 = lon2/180*pi + 2*pi
	
	# Spherical law of cosines
	# 6371 is radius of earth in km
	dist = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))*6371

	# Haversine formula: equivalent answer to at least 7 decimal places
	#a = sin((lat2-lat1)/2)^2 + cos(lat1)*cos(lat2)*sin((lon2-lon1)/2)^2
	#dist2 = 6371*(2*atan2(sqrt(a), sqrt(1-a)))
	
	shift$kmyr.bio = dist/(shift$maxyr - shift$minyr)

# Write out
	nm = paste('Output/shift_', Sys.Date(), '.csv', sep=''); nm
	# nm = paste('Output/shift1994_', Sys.Date(), '.csv', sep=''); nm
	write.csv(shift, file = nm)

## Examine shifts within regions (histogram and summary)
		# centbiolatindiv
		reg = sort(unique(shift$region))
		quartz(width=10, height=7)
		# pdf(width=10, height=7, file=paste('Figures/centbiolatindiv.b.lm_hist_', Sys.Date(), '.pdf', sep=''))
		xlims = range(shift$centbiolatindiv.b.lm, na.rm=T)
		bks = seq(xlims[1], xlims[2], length.out=50)
		par(mfrow=c(3,4))
		for(i in 1:length(reg)){
			print(as.character(reg[i]))
			print(summary(shift$centbiolatindiv.b.lm[shift$region==reg[i]]))
			hist(shift$centbiolatindiv.b.lm[shift$region==reg[i]], col='grey', breaks=bks, xlab='Rate of shift (center of biomass)', main=reg[i], xlim=xlims)
		}
	
		dev.off()

		# centbiolat (stratum)
		quartz(width=10, height=7)
		# pdf(width=10, height=7, file=paste('Figures/centbiolat.b.lm_hist_', Sys.Date(), '.pdf', sep=''))
		xlims = range(shift$centbiolat.b.lm, na.rm=T)
		bks = seq(xlims[1], xlims[2], length.out=50)
		par(mfrow=c(3,4))
		for(i in 1:length(reg)){
			print(as.character(reg[i]))
			print(summary(shift$centbiolat.b.lm[shift$region==reg[i]]))
			hist(shift$centbiolat.b.lm[shift$region==reg[i]], col='grey', breaks=bks, xlab='Rate of shift (center of biomass by stratum)', main=reg[i], xlim=xlims)
		}
	
		dev.off()

	# compare spp trends between the two seasons (for Newfoundland and East Coast)
		nf = shift[shift$region=='DFO_Newfoundland_Spring', c('spp', 'centbiolatindiv.b.lm')]
			names(nf) = c('spp', 'spring')
			dim(nf)
			nf = merge(nf, shift[shift$region=='DFO_Newfoundland_Fall', c('spp', 'centbiolatindiv.b.lm')])
			dim(nf)
			names(nf) = c('spp', 'spring', 'fall')
		ec = shift[shift$region=='NEFSC_Spring', c('spp', 'centbiolatindiv.b.lm')]
			names(ec) = c('spp', 'spring')
			dim(ec)
			ec = merge(ec, shift[shift$region=='NEFSC_Fall', c('spp', 'centbiolatindiv.b.lm')])
			dim(ec)
			names(ec) = c('spp', 'spring', 'fall')
	
		par(mfrow=c(1,2))
		xlims = range(c(nf$spring, nf$fall), na.rm=T)
		plot(nf$spring, nf$fall, main='Newfoundland', xlab='Spring rate of shift', ylab='Fall rate of shift', xlim=xlims, ylim=xlims)
			cor.test(nf$spring, nf$fall) # p = 0.057, cor=0.27
		xlims = range(c(ec$spring, ec$fall), na.rm=T)
		plot(ec$spring, ec$fall, main='East Coast', xlab='Spring rate of shift', ylab='Fall rate of shift', xlim=xlims, ylim=xlims)	
			cor.test(ec$spring, ec$fall) # p = 4e-6, cor=0.73



	
##################################	
## Mean rate of shift by region ##
##################################	

# Autocorrelation calculation assumes measurements were taken are regular intervals, which isn't always true
	require(car)

setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
shift = read.csv('Output/shift_2012-11-15.csv', row.names=1)
centbiolatindiv = read.csv('Output/centbiolatindiv_2012-10-16.csv', row.names=1)
centbiolat = read.csv('Output/centbiolat_2012-10-16.csv', row.names=1)
centbiolonindiv = read.csv('Output/centbiolonindiv_2012-10-16.csv', row.names=1)
centbiolon = read.csv('Output/centbiolon_2012-10-16.csv', row.names=1)
centbiodepthindiv = read.csv('Output/centbiodepthindiv_2012-11-15.csv', row.names=1)
centbiodepth = read.csv('Output/centbiodepth_2012-11-15.csv', row.names=1)
surveyminlat = read.csv('Output/surveyminlat_2012-10-16.csv', row.names=1)
surveymeanlat = read.csv('Output/surveymeanlat_2012-10-16.csv', row.names=1)
surveymaxlat = read.csv('Output/surveymaxlat_2012-10-16.csv', row.names=1)
surveyminlon = read.csv('Output/surveyminlon_2012-10-16.csv', row.names=1)
surveymeanlon = read.csv('Output/surveymeanlon_2012-10-16.csv', row.names=1)
surveymaxlon = read.csv('Output/surveymaxlon_2012-10-16.csv', row.names=1)
surveymindepth = read.csv('Output/surveymindepth_2012-11-15.csv', row.names=1)
surveymeandepth = read.csv('Output/surveymeandepth_2012-11-15.csv', row.names=1)
surveymaxdepth = read.csv('Output/surveymaxdepth_2012-11-15.csv', row.names=1)
surveyminjulian = read.csv('Output/surveyminjulian_2012-10-16.csv', row.names=1)
surveymeanjulian = read.csv('Output/surveymeanjulian_2012-10-16.csv', row.names=1)
surveymaxjulian = read.csv('Output/surveymaxjulian_2012-10-16.csv', row.names=1)
npresences = read.csv('Output/npresences_2012-10-16.csv', row.names=1)
btmean = read.csv('Output/btmean_2012-10-16.csv', row.names=1)
btmeanstrat = read.csv('Output/btmeanstrat_2012-10-16.csv', row.names=1)
sstmean = read.csv('Output/sstmean_2012-10-16.csv', row.names=1)
sstmeanstrat = read.csv('Output/sstmeanstrat_2012-10-16.csv', row.names=1)
spppres1 = read.csv('Output/sppres1_2012-11-21.csv', row.names=1)

# p-values from lm object
lmp <- function(object){
	i = summary(object)$coef
	if(nrow(i)>0){
		return(i[,ncol(i)])
	} else {
		return(numeric(0))
	}
}


	# Set up dataframe to hold mean regional shifts
	shiftreg = data.frame(region = sort(unique(shift$region)))
		shiftreg$season = NA
		shiftreg$season[shiftreg$region %in% c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'DFO_ScotianShelf')] = 'summer'
		shiftreg$season[shiftreg$region %in% c('NEFSC_Spring', 'DFO_Newfoundland_Spring')] = 'spring'
		shiftreg$season[shiftreg$region %in% c('NEFSC_Fall', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')] = 'fall'
	# minthresh = 0; avethresh = 0; suff = '' # use all taxa
	minthresh = 0; avethresh = 0; suff = '_onlygenera' # use only taxa defined to genus
	# minthresh = 5; avethresh = 0; suff = '_threshmin5ave0' # only use spp with >=5 presences/yr (and >= an average of 0, i.e. no threshold on the average)
	tcols = grep('X', names(btmean))
	yrs = as.numeric(gsub('X', '', names(btmean)[tcols]))
	vars = list(btmean=btmean, sstmean=sstmean, julianmean=surveymeanjulian) 
	mods = c('.a.lm', '.b.lm', '.a.lm.se', '.b.lm.se', '.p.lm', '.r.dwt', '.p.dwt')
	for(i in 1:length(vars)){
		for(j in 1:length(mods)) shiftreg[[paste(names(vars)[i], mods[j], sep='')]] = NA
	}
	dim(shiftreg) # 11 x 22
	
	# Calculate min and max year for each region
	minyrnotna = function(x, yr) return(min(yr[!is.na(x)]))
	maxyrnotna = function(x, yr) return(max(yr[!is.na(x)]))
	regyears = data.frame(region = surveyminlat$region)
	rownames(regyears) = regyears$region
	regyears$minyr = apply(surveyminlat[,grep('X', names(surveyminlat))], MARGIN=1, FUN=minyrnotna, yr = yrs)
	regyears$maxyr = apply(surveyminlat[,grep('X', names(surveyminlat))], MARGIN=1, FUN=maxyrnotna, yr = yrs)
	shiftreg = merge(shiftreg, regyears)

	# Set some parameter values
	options(warn=1) # print as they occur (0 is default)
	#options(warn=2) # stop if they occur (0 is default)

	# Fill dataframe with model fits of temperature trends
	for(i in 1:nrow(btmean)){ # for each region		
		for(j in 1:length(vars)){ # for each variable
			nm = names(vars)[j]
			lcols = grep('X', names(vars[[j]])) # columns with data
			# Set up independent variable for regression
			y = as.numeric(vars[[j]][i,lcols])					
			x = yrs[!is.na(y)]; y = y[!is.na(y)]
			if(length(x)>1){
				mod = lm(y ~ x)
				d = dwt(mod)
				shiftreg[[paste(nm, '.a.lm', sep='')]][i] = coef(mod)[1]
				shiftreg[[paste(nm, '.b.lm', sep='')]][i] = coef(mod)[2]
				shiftreg[[paste(nm, '.a.lm.se', sep='')]][i] = summary(mod)$coefficients[1,2]
				shiftreg[[paste(nm, '.b.lm.se', sep='')]][i] = summary(mod)$coefficients[2,2]
				shiftreg[[paste(nm, '.p.lm', sep='')]][i] = lmp(mod)[2]
				shiftreg[[paste(nm, '.r.dwt', sep='')]][i] = d$r
				shiftreg[[paste(nm, '.p.dwt', sep='')]][i] = d$p
			}
		}	
	}
		

	# How many temperature rates show temporal autocorrelation?
	sum(i <- shiftreg$btmean.p.dwt<0.05, na.rm=T)/sum(!is.na(shiftreg$btmean.p.dwt)) # 10%
		shiftreg[which(i), c('region', 'btmean.r.dwt', 'btmean.p.dwt')]
	sum(i <- shiftreg$btmeanstrat.p.dwt<0.05, na.rm=T)/sum(!is.na(shiftreg$btmeanstrat.p.dwt)) # 10%
		shiftreg[which(i), c('region', 'btmeanstrat.r.dwt', 'btmeanstrat.p.dwt')]
	sum(i <- shiftreg$btmeanmuet.p.dwt<0.05, na.rm=T)/sum(!is.na(shiftreg$btmeanmuet.p.dwt)) # 20%
		shiftreg[which(i), c('region', 'btmeanmuet.r.dwt', 'btmeanmuet.p.dwt')]
	sum(i <- shiftreg$sstmean.p.dwt<0.05, na.rm=T)/sum(!is.na(shiftreg$sstmean.p.dwt)) # 0%
		shiftreg[which(i), c('region', 'sstmean.r.dwt', 'sstmean.p.dwt')]
	sum(i <- shiftreg$sstmeanstrat.p.dwt<0.05, na.rm=T)/sum(!is.na(shiftreg$sstmeanstrat.p.dwt)) # 13%
		shiftreg[which(i), c('region', 'sstmeanstrat.r.dwt', 'sstmeanstrat.p.dwt')]
	sum(i <- shiftreg$sstmeanmuet.p.dwt<0.05, na.rm=T)/sum(!is.na(shiftreg$sstmeanmuet.p.dwt)) # 13%
		shiftreg[which(i), c('region', 'sstmeanmuet.r.dwt', 'sstmeanmuet.p.dwt')]

	## Determine which species to keep (depends on # presences)
	i = apply(npresences[,grep('X', names(npresences))], MARGIN=1, FUN=min, na.rm=T)>=minthresh
	i = i & apply(npresences[,grep('X', names(npresences))], MARGIN=1, FUN=mean, na.rm=T)>=avethresh
	if(suff == '_onlygenera'){
		shift$regspp = paste(shift$region, shift$spp, sep='_')
		row.names(spppres1) = as.character(spppres1$regspp)
		spppres1 = spppres1[as.character(shift$regspp),] # put spppres1 in correct order
		if(all(spppres1$regspp == shift$regspp)){
			i = i & spppres1$use
		} else {
			warning("spppres1 and shift not in same order!")
		}
	}
	sum(i) # 659 w/ _onlygenera
	
	# Sample size by region
	table(shift$region[i])

	## Add average rate of community shift to shiftreg (and SD and SE)
	lreal = function(x) return(sum(!is.na(x)))
	mean = aggregate(list(centbiolat.a.lm = shift$centbiolat.a.lm[i], centbiolat.b.lm=shift$centbiolat.b.lm[i], centbiolatindiv.a.lm = shift$centbiolatindiv.a.lm[i], centbiolatindiv.b.lm=shift$centbiolatindiv.b.lm[i], centbiolon.a.lm = shift$centbiolon.a.lm[i], centbiolon.b.lm=shift$centbiolon.b.lm[i], centbiolonindiv.a.lm = shift$centbiolonindiv.a.lm[i], centbiolonindiv.b.lm=shift$centbiolonindiv.b.lm[i], minlat.a.lm=shift$minlat.a.lm[i], minlat.b.lm=shift$minlat.b.lm[i], maxlat.a.lm=shift$maxlat.a.lm[i], maxlat.b.lm=shift$maxlat.b.lm[i], centbiodepth.a.lm = shift$centbiodepth.a.lm[i], centbiodepth.b.lm = shift$centbiodepth.b.lm[i], centbiodepthindiv.a.lm = shift$centbiodepthindiv.a.lm[i], centbiodepthindiv.b.lm = shift$centbiodepthindiv.b.lm[i]), by=list(region=shift$region[i]), FUN=mean, na.rm=T)
	sd = aggregate(list(centbiolat.a.lm.sd = shift$centbiolat.a.lm[i], centbiolat.b.lm.sd =shift$centbiolat.b.lm[i], centbiolatindiv.a.lm.sd = shift$centbiolatindiv.a.lm[i], centbiolatindiv.b.lm.sd =shift$centbiolatindiv.b.lm[i], centbiolon.a.lm.sd = shift$centbiolon.a.lm[i], centbiolon.b.lm.sd =shift$centbiolon.b.lm[i], centbiolonindiv.a.lm.sd = shift$centbiolonindiv.a.lm[i], centbiolonindiv.b.lm.sd =shift$centbiolonindiv.b.lm[i], minlat.a.lm.sd =shift$minlat.a.lm[i], minlat.b.lm.sd =shift$minlat.b.lm[i], maxlat.a.lm.sd =shift$maxlat.a.lm[i], maxlat.b.lm.sd =shift$maxlat.b.lm[i], centbiodepth.a.lm.sd = shift$centbiodepth.a.lm[i], centbiodepth.b.lm.sd =shift$centbiodepth.b.lm[i], centbiodepthindiv.a.lm.sd = shift$centbiodepthindiv.a.lm[i], centbiodepthindiv.b.lm.sd =shift$centbiodepthindiv.b.lm[i]), by=list(region=shift$region[i]), FUN=sd, na.rm=T)
	n = aggregate(list(centbiolat.lm.n = shift$centbiolat.a.lm[i], centbiolatindiv.lm.n = shift$centbiolatindiv.a.lm[i], centbiolon.lm.n = shift$centbiolon.a.lm[i], centbiolonindiv.lm.n = shift$centbiolonindiv.a.lm[i], minlat.lm.n =shift$minlat.a.lm[i], maxlat.lm.n =shift$maxlat.a.lm[i], centbiodepth.lm.n = shift$centbiodepth.a.lm[i], centbiodepthindiv.lm.n = shift$centbiodepthindiv.a.lm[i]), by=list(region=shift$region[i]), FUN=lreal)
	shiftreg = merge(shiftreg, mean)
	shiftreg = merge(shiftreg, sd)
	shiftreg = merge(shiftreg, n)
	shiftreg$centbiolat.b.lm.se = shiftreg$centbiolat.b.lm.sd/ sqrt(shiftreg$centbiolat.lm.n)
	shiftreg$centbiolatindiv.b.lm.se = shiftreg$centbiolatindiv.b.lm.sd/ sqrt(shiftreg$centbiolatindiv.lm.n)
	shiftreg$centbiolon.b.lm.se = shiftreg$centbiolon.b.lm.sd/ sqrt(shiftreg$centbiolon.lm.n)
	shiftreg$centbiolonindiv.b.lm.se = shiftreg$centbiolonindiv.b.lm.sd/ sqrt(shiftreg$centbiolonindiv.lm.n)
	shiftreg$centbiodepth.b.lm.se = shiftreg$centbiodepth.b.lm.sd/ sqrt(shiftreg$centbiodepth.lm.n)
	shiftreg$centbiodepthindiv.b.lm.se = shiftreg$centbiodepthindiv.b.lm.sd/ sqrt(shiftreg$centbiodepthindiv.lm.n)

	## Add average velocity by region (km/yr)
		# average starting and ending lat and lon
		shiftreg$lat1 = shiftreg$centbiolatindiv.a.lm + shiftreg$centbiolatindiv.b.lm*shiftreg$minyr
		shiftreg$lat2 = shiftreg$centbiolatindiv.a.lm + shiftreg$centbiolatindiv.b.lm*shiftreg$maxyr
		shiftreg$lon1 = shiftreg$centbiolonindiv.a.lm + shiftreg$centbiolonindiv.b.lm*shiftreg$minyr
		shiftreg$lon2 = shiftreg$centbiolonindiv.a.lm + shiftreg$centbiolonindiv.b.lm*shiftreg$maxyr

		# To positive radians
		l1 = shiftreg$lat1/180*pi
		l2 = shiftreg$lat2/180*pi
		lg1 = shiftreg$lon1/180*pi + 2*pi
		lg2 = shiftreg$lon2/180*pi + 2*pi
		
		# Spherical law of cosines
		# 6371 is radius of earth in km
		shiftreg$dist = acos(sin(l1)*sin(l2)+cos(l1)*cos(l2)*cos(lg2-lg1))*6371
			
		
		shiftreg$veldec = shiftreg$dist/(shiftreg$maxyr - shiftreg$minyr) * 10 # in km/decade by region
		shiftreg[,c('region', 'veldec')]

	## Add latitudinal and longitudinal range for each region
	all(surveyminlat$region == surveymaxlat$region) # should be TRUE
	all(shiftreg$region == surveyminlat$region)
	xcols = grep('X', names(surveyminlat))
	shiftreg$rangelat = apply(surveymaxlat[,xcols], MARGIN=1, FUN=max, na.rm=TRUE)- apply(surveyminlat[,xcols], MARGIN=1, FUN=min, na.rm=TRUE)
	shiftreg$rangelon = apply(surveymaxlon[,xcols], MARGIN=1, FUN=max, na.rm=TRUE)- apply(surveyminlon[,xcols], MARGIN=1, FUN=min, na.rm=TRUE)
	shiftreg$rangedepth = apply(surveymaxdepth[,xcols], MARGIN=1, FUN=max, na.rm=TRUE)- apply(surveymindepth[,xcols], MARGIN=1, FUN=min, na.rm=TRUE)


	# Write out
	nm = paste('Output/shiftreg', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(shiftreg, file = nm)


## Calculate a region-wide timeseries of lat, lon, and depth changes
	# find first lat,lon, depth for each species
	flat1 = numeric(nrow(centbiolat)); flon1 = flat1; fdep1 = flat1; flat2 = flat1; flon2 = flat1; fdep2 = flat1
	ycols = grep('X', names(centbiolatindiv))
	for(j in 1:length(flat1)){
		temp = centbiolatindiv[j, ycols]; flat1[j] = temp[!is.na(temp)][1]
		temp = centbiolonindiv[j, ycols]; flon1[j] = temp[!is.na(temp)][1]
		temp = centbiodepthindiv[j, ycols]; fdep1[j] = temp[!is.na(temp)][1]
		temp = centbiolat[j, ycols]; flat2[j] = temp[!is.na(temp)][1]
		temp = centbiolon[j, ycols]; flon2[j] = temp[!is.na(temp)][1]
		temp = centbiodepth[j, ycols]; fdep2[j] = temp[!is.na(temp)][1]
	}
	flat1 = data.frame(X1963=flat1)
	flon1 = data.frame(X1963=flon1)
	fdep1 = data.frame(X1963=fdep1)
	flat2 = data.frame(X1963=flat2)
	flon2 = data.frame(X1963=flon2)
	fdep2 = data.frame(X1963=fdep2)
	
	flatd1 = flat1[,rep(1,length(ycols))]
	flond1 = flon1[,rep(1,length(ycols))]
	fdepd1 = fdep1[,rep(1,length(ycols))]
	flatd2 = flat2[,rep(1,length(ycols))]
	flond2 = flon2[,rep(1,length(ycols))]
	fdepd2 = fdep2[,rep(1,length(ycols))]
		
	centbiolatindiv2 = centbiolatindiv; centbiolatindiv2[,ycols] = centbiolatindiv[,ycols] - flatd1
	centbiolonindiv2 = centbiolonindiv; centbiolonindiv2[,ycols] = centbiolonindiv[,ycols] - flond1
	centbiodepthindiv2 = centbiodepthindiv; centbiodepthindiv2[,ycols] = centbiodepthindiv[,ycols] - fdepd1
	centbiolat2 = centbiolat; centbiolat2[,ycols] = centbiolat[,ycols] - flatd2
	centbiolon2 = centbiolon; centbiolon2[,ycols] = centbiolon[,ycols] - flond2
	centbiodepth2 = centbiodepth; centbiodepth2[,ycols] = centbiodepth[,ycols] - fdepd2

	# make dataframe to hold mean offsets
	regcentbiolatindiv = data.frame(region = sort(unique(centbiolatindiv$region)))
	regcentbiolonindiv = data.frame(region = sort(unique(centbiolatindiv$region)))
	regcentbiodepthindiv = data.frame(region = sort(unique(centbiolatindiv$region)))
	regcentbiolat = data.frame(region = sort(unique(centbiolatindiv$region)))
	regcentbiolon = data.frame(region = sort(unique(centbiolatindiv$region)))
	regcentbiodepth = data.frame(region = sort(unique(centbiolatindiv$region)))
	
	# check that dataframes are in same order
	all(spppres1$regspp == shift$regspp)
	all(regcentbiolatindiv$regspp == shift$regspp)
	all(regcentbiolonindiv$regspp == shift$regspp)
	all(regcentbiodepthindiv$regspp == shift$regspp)
	all(regcentbiolat$regspp == shift$regspp)
	all(regcentbiolon$regspp == shift$regspp)
	all(regcentbiodepth$regspp == shift$regspp)
	
	# calc mean offset in each year for each region
	for(j in 1:length(ycols)){
		# centbiolatindiv
		temp = aggregate(centbiolatindiv2[i,ycols[j]], by = list(region = centbiolatindiv$region[i]), FUN = mean)
			names(temp)[2] = names(centbiolatindiv)[ycols[j]]
		regcentbiolatindiv = merge(regcentbiolatindiv, temp, all.x=TRUE)

		# centbiolonindiv
		temp = aggregate(centbiolonindiv2[i,ycols[j]], by = list(region = centbiolonindiv$region[i]), FUN = mean)
			names(temp)[2] = names(centbiolonindiv)[ycols[j]]
		regcentbiolonindiv = merge(regcentbiolonindiv, temp, all.x=TRUE)

		# centbiodepthindiv
		temp = aggregate(centbiodepthindiv2[i,ycols[j]], by = list(region = centbiodepthindiv$region[i]), FUN = mean)
			names(temp)[2] = names(centbiodepthindiv)[ycols[j]]
		regcentbiodepthindiv = merge(regcentbiodepthindiv, temp, all.x=TRUE)

		# centbiolat
		temp = aggregate(centbiolat2[i,ycols[j]], by = list(region = centbiolat$region[i]), FUN = mean)
			names(temp)[2] = names(centbiolat)[ycols[j]]
		regcentbiolat = merge(regcentbiolat, temp, all.x=TRUE)

		# centbiolon
		temp = aggregate(centbiolon2[i,ycols[j]], by = list(region = centbiolon$region[i]), FUN = mean)
			names(temp)[2] = names(centbiolon)[ycols[j]]
		regcentbiolon = merge(regcentbiolon, temp, all.x=TRUE)

		# centbiodepth
		temp = aggregate(centbiodepth2[i,ycols[j]], by = list(region = centbiodepth$region[i]), FUN = mean)
			names(temp)[2] = names(centbiodepth)[ycols[j]]
		regcentbiodepth = merge(regcentbiodepth, temp, all.x=TRUE)
	}
	
	# write out
	nm = paste('Output/regcentbiolatindiv', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(regcentbiolatindiv, nm)
	nm = paste('Output/regcentbiolonindiv', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(regcentbiolonindiv, nm)
	nm = paste('Output/regcentbiodepthindiv', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(regcentbiodepthindiv, nm)
	nm = paste('Output/regcentbiolat', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(regcentbiolat, nm)
	nm = paste('Output/regcentbiolon', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(regcentbiolon, nm)
	nm = paste('Output/regcentbiodepth', suff, '_', Sys.Date(), '.csv', sep=''); nm
	write.csv(regcentbiodepth, nm)

#######################################################
## Running mean rate of shift by taxon and by region ##
#######################################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
require(RColorBrewer)
centbiolatindiv = read.csv('Output/centbiolatindiv_2012-10-16.csv', row.names=1)
centbiolonindiv = read.csv('Output/centbiolonindiv_2012-10-16.csv', row.names=1)
centbiodepthindiv = read.csv('Output/centbiodepthindiv_2012-11-15.csv', row.names=1)
surveyminlat = read.csv('Output/surveyminlat_2012-10-16.csv', row.names=1)
surveymaxlat = read.csv('Output/surveymaxlat_2012-10-16.csv', row.names=1)
surveyminlon = read.csv('Output/surveyminlon_2012-10-16.csv', row.names=1)
surveymaxlon = read.csv('Output/surveymaxlon_2012-10-16.csv', row.names=1)
surveymindepth = read.csv('Output/surveymindepth_2012-11-15.csv', row.names=1)
surveymaxdepth = read.csv('Output/surveymaxdepth_2012-11-15.csv', row.names=1)
biomass = read.csv('Output/biomass_2012-10-16.csv', row.names=1)
btmean = read.csv('Output/btmean_2012-10-16.csv', row.names=1)
sstmean = read.csv('Output/sstmean_2012-10-16.csv', row.names=1)
spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1) # matches taxon name to taxonomy
spppres1 = read.csv('Output/sppres1_2012-10-16.csv', row.names=1) # spp present at least once. use column indicates if resolved at least to genus


	# Make sure all dataframes in same order by region and spp
	all(centbiolatindiv$regspp == centbiolonindiv$regspp)
	all(centbiolatindiv$regspp == centbiodepthindiv$regspp)
	all(centbiolatindiv$regspp == centbiodepthindiv$regspp)
	all(centbiolatindiv$regspp == biomass$regspp)

	# Make sure all dataframes in same order by region
	all(btmean$region == sstmean$region)
	all(btmean$region == surveyminlat$region)
	all(btmean$region == surveymaxlat$region)
	all(btmean$region == surveyminlon$region)
	all(btmean$region == surveymaxlon$region)
	all(btmean$region == surveymindepth$region)
	all(btmean$region == surveymaxdepth$region)

# Rate of lat/lon/depth/biomass shift calculations (by regspp), 10 year running mean
	# Set up dataframes
	shiftlat10 = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # 10 year running mean of lat shift
	shiftlat10mean = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # the center of the regression
	shiftlon10 = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # 10 year running mean of lon shift
	shiftlon10mean = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # center
	shiftdepth10mean = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # center
	shiftdist10 = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # 10 year running mean of distance shifted
	shifthead10 = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # 10 year running mean of heading shifted
	shiftdepth10 = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # 10 year running mean of depth shift
	shiftbio10 = data.frame(regspp = centbiolatindiv$regspp, region = centbiolatindiv$region, spp = centbiolatindiv$spp) # 10 year running mean of biomass shift
	tcols = grep('X', names(centbiolatindiv))
	yrs = as.numeric(gsub('X', '', names(centbiolatindiv)[tcols]))
	for(i in 1:length(yrs)){
		nm = paste('X', yrs[i], sep='')
		shiftlat10[nm] = NA
		shiftlon10[nm] = NA
		shiftlat10mean[nm] = NA
		shiftlon10mean[nm] = NA
		shiftdepth10mean[nm] = NA
		shiftdist10[nm] = NA
		shifthead10[nm] = NA
		shiftdepth10[nm] = NA
		shiftbio10[nm] = NA
	}
	
	# Set some parameter values
	options(warn=1) # print as they occur (0 is default)
	#options(warn=2) # stop if they occur (0 is default)

	# Fill dataframes with shift rates
	for(i in 1:nrow(centbiolatindiv)){
		if(i %% 50 == 0) print(i)
		theseyrs = yrs[!is.na(centbiolatindiv[i,tcols])] # years with data for this taxon
		
		# calc the running means
		for(j in (min(theseyrs)+5):(max(theseyrs)-4)){ # for each base year
			runyrs = theseyrs[theseyrs >= (j-5) & theseyrs <= (j+4)] # yrs with data in this 10 yr mean
			nms = paste('X', runyrs, sep='') # column names for the 10 year range
			centnm = paste('X', j, sep='') # name of the center year

			# Latitude
			mod1 = lm(as.numeric(centbiolatindiv[i,nms])~runyrs)
			shiftlat10[i,centnm] = coef(mod1)[2]
			shiftlat10mean[i,centnm] = coef(mod1)[1] + coef(mod1)[2]*j # the average lat

			# Longitude
			mod2 = lm(as.numeric(centbiolonindiv[i,nms])~runyrs)
			shiftlon10[i,centnm] = coef(mod2)[2]
			shiftlon10mean[i,centnm] = coef(mod2)[1] + coef(mod2)[2]*j

			# Distance along rhumb line (constant bearing) from http://www.movable-type.co.uk/scripts/latlong.html
			lat1 = coef(mod1)[1] + coef(mod1)[2]*min(runyrs)
			lat2 = coef(mod1)[1] + coef(mod1)[2]*max(runyrs)
			lon1 = coef(mod2)[1] + coef(mod2)[2]*min(runyrs)
			lon2 = coef(mod2)[1] + coef(mod2)[2]*max(runyrs)
	
				# To positive radians
			lat1 = lat1/180*pi
			lat2 = lat2/180*pi
			lon1 = lon1/180*pi + 2*pi
			lon2 = lon2/180*pi + 2*pi
	
			# 6371 is radius of earth in km
			dPhi = log(tan(pi/4 + lat2/2)/tan(pi/4 + lat1/2))
			q = (lat2-lat1)/dPhi
			if(is.infinite(q)) q = 0
			dLon = lon2 - lon1
			if(dLon>pi){
				if(dLon>0) dLon = -(2*pi-dLon)
				if(dLon <= 0) dLon = 2*pi+dLon
			}
			dist = sqrt((lat2-lat1)^2 + q^2 * dLon^2) * 6371 
			# dist = acos(sin(lat1)*sin(lat2)+cos(lat1)*cos(lat2)*cos(lon2-lon1))*6371			 # great circle distance
			shiftdist10[i,centnm] = dist/(diff(range(runyrs))+1)
			
			# Heading (0° is north) from http://www.movable-type.co.uk/scripts/latlong.html
			# Rhumb line (constant bearing, not great circle)
			shifthead10[i,centnm] = (atan2(dLon, dPhi)*180/pi + 360) %% 360

			# Depth
			y = as.numeric(centbiodepthindiv[i,nms])
			if(all(is.na(y))){
				warning(paste('no depth for', paste(range(runyrs), collapse=', '), centbiodepthindiv$region[i], centbiodepthindiv$spp[i]))
			} else {
				mod = lm(y~runyrs)
				shiftdepth10[i,centnm] = coef(mod)[2]
				shiftdepth10mean[i,centnm] = coef(mod)[1] + coef(mod)[2]*j
			}
			# Biomass
			mod = lm(as.numeric(biomass[i,nms])~runyrs)
			shiftbio10[i,centnm] = coef(mod)[2]
		}	
	}

	# Calc 10 year running mean temperature trends
	btmean10 = data.frame(region = btmean$region) # 10 year running mean of bt trend
	sstmean10 = data.frame(region = btmean$region) # 10 year running mean of sst trend

	tcols = grep('X', names(btmean))
	yrs = as.numeric(gsub('X', '', names(btmean)[tcols]))
	for(i in 1:length(yrs)){
		nm = paste('X', yrs[i], sep='')
		btmean10[nm] = NA
		sstmean10[nm] = NA
	}
	
	# Set some parameter values
	options(warn=1) # print as they occur (0 is default)

	# Fill dataframes with shift rates for temperature
	for(i in 1:nrow(btmean)){
		theseyrs = yrs[!is.na(btmean[i,tcols])] # years with data for this taxon
		
		# calc the running means (but no data in NEFSC Fall
		if(!grepl('NEFSC_Fall', btmean$region[i])){
			for(j in (min(theseyrs)+5):(max(theseyrs)-4)){ # for each base year
				runyrs = theseyrs[theseyrs >= (j-5) & theseyrs <= (j+4)] # yrs with data in this 10 yr mean
				nms = paste('X', runyrs, sep='') # column names for the 10 year range
				centnm = paste('X', j, sep='') # name of the center year
	
				# BT
				mod1 = lm(as.numeric(btmean[i,nms])~runyrs)
				btmean10[i,centnm] = coef(mod1)[2]
				
				# SST (except in Newfoundland)
				if(!grepl('Newfoundland', sstmean$region[i])){
					mod2 = lm(as.numeric(sstmean[i,nms])~runyrs)
					sstmean10[i,centnm] = coef(mod2)[2]
				}
			}	
		}
	}

## Output taxon shifts
	write.csv(shiftlat10, file=paste('Output/shiftlat10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftlon10, file=paste('Output/shiftlon10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftlat10mean, file=paste('Output/shiftlat10mean_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftlon10mean, file=paste('Output/shiftlon10mean_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftdepth10mean, file=paste('Output/shiftdepth10mean_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftdist10, file=paste('Output/shiftdist10_', Sys.Date(), '.csv', sep=''))
	write.csv(shifthead10, file=paste('Output/shifthead10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftdepth10, file=paste('Output/shiftdepth10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftbio10, file=paste('Output/shiftbio10_', Sys.Date(), '.csv', sep=''))
	write.csv(btmean10, file=paste('Output/btmean10_', Sys.Date(), '.csv', sep=''))
	write.csv(sstmean10, file=paste('Output/sstmean10_', Sys.Date(), '.csv', sep=''))

## Read in taxon shifts
	shiftlat10 = read.csv('Output/shiftlat10_2012-11-15.csv', row.names=1)
	shiftlon10 = read.csv('Output/shiftlon10_2012-11-15.csv', row.names=1)
	shiftlat10mean = read.csv('Output/shiftlat10mean_2012-11-15.csv', row.names=1)
	shiftlon10mean = read.csv('Output/shiftlon10mean_2012-11-15.csv', row.names=1)
	shiftdepth10mean = read.csv('Output/shiftdepth10mean_2012-11-20.csv', row.names=1)
	shiftdist10 = read.csv('Output/shiftdist10_2012-11-15.csv', row.names=1)
	shifthead10 = read.csv('Output/shifthead10_2012-11-15.csv', row.names=1)
	shiftdepth10 = read.csv('Output/shiftdepth10_2012-11-15.csv', row.names=1)
	shiftbio10 = read.csv('Output/shiftbio10_2012-11-15.csv', row.names=1)
	spppres1 = read.csv('Output/sppres1_2012-10-16.csv', row.names=1) # spp present at least once. use column indicates if resolved at least to genus

## Regional average shifts and means
	shiftreglat10 = data.frame(region = btmean$region)
	shiftreglon10 = data.frame(region = btmean$region)
	shiftregdepth10 = data.frame(region = btmean$region)
	shiftregbio10 = data.frame(region = btmean$region)

	shiftreglat10mean = data.frame(region = btmean$region)
	shiftreglon10mean = data.frame(region = btmean$region)
	shiftregdepth10mean = data.frame(region = btmean$region)
	
	# trim to taxa to use
	suff = '_onlygenera'
	inds = shiftlat10$regspp %in% spppres1$regspp[spppres1$use]
		sum(inds) # 659 for onlygenera
	
	tcols = grep('X', names(shiftlat10))
	yrs = as.numeric(gsub('X', '', names(shiftlat10)[tcols]))
	for(i in 1:length(yrs)){
		nm = paste('X', yrs[i], sep='')
		# lat shift
		temp = aggregate(list(nm = shiftlat10[[nm]][inds]), by=list(region = shiftlat10$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftreglat10 = merge(shiftreglat10, temp)

		# lon shift
		temp = aggregate(list(nm = shiftlon10[[nm]][inds]), by=list(region = shiftlon10$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftreglon10 = merge(shiftreglon10, temp)

		# depth shift
		temp = aggregate(list(nm = shiftdepth10[[nm]][inds]), by=list(region = shiftdepth10$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftregdepth10 = merge(shiftregdepth10, temp)

		# bio shift
		temp = aggregate(list(nm = shiftbio10[[nm]][inds]), by=list(region = shiftbio10$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftregbio10 = merge(shiftregbio10, temp)

		# lat mean
		temp = aggregate(list(nm = shiftlat10mean[[nm]][inds]), by=list(region = shiftlat10mean$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftreglat10mean = merge(shiftreglat10mean, temp)

		# lon mean
		temp = aggregate(list(nm = shiftlon10mean[[nm]][inds]), by=list(region = shiftlon10mean$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftreglon10mean = merge(shiftreglon10mean, temp)

		# depth mean
		temp = aggregate(list(nm = shiftdepth10mean[[nm]][inds]), by=list(region = shiftdepth10mean$region[inds]), FUN=mean, na.rm=TRUE)
			names(temp)[2] = nm
			temp[[nm]][is.nan(temp[[nm]])] = NA
		shiftregdepth10mean = merge(shiftregdepth10mean, temp)
	}
	
	# Distance and heading from mean lat and lon shifts
	shiftregdist10 = data.frame(region = btmean$region)
	shiftreghead10 = data.frame(region = btmean$region)

	tcols = grep('X', names(shiftreglat10))
	yrs = as.numeric(gsub('X', '', names(shiftreglat10)[tcols]))
	for(i in 1:nrow(shiftreglat10)){
		for(j in 1:length(yrs)){
			# Distance along rhumb line (constant bearing) from http://www.movable-type.co.uk/scripts/latlong.html
			nm = paste('X', yrs[j], sep='')
			lat1 = shiftreglat10mean[i,nm] + shiftreglat10[i,nm]*-5 # 5 yrs earlier
			lat2 = shiftreglat10mean[i,nm] + shiftreglat10[i,nm]*4 # 4 yrs later
			lon1 = shiftreglon10mean[i,nm] + shiftreglon10[i,nm]*-5 # 5 yrs earlier
			lon2 = shiftreglon10mean[i,nm] + shiftreglon10[i,nm]*4 # 4 yrs later
	
			if(!any(is.na(c(lat1, lat2, lon1, lon2)))){ # only run if have all values
					# To positive radians
				lat1 = lat1/180*pi
				lat2 = lat2/180*pi
				lon1 = lon1/180*pi + 2*pi
				lon2 = lon2/180*pi + 2*pi
		
				# 6371 is radius of earth in km
				dPhi = log(tan(pi/4 + lat2/2)/tan(pi/4 + lat1/2))
				q = (lat2-lat1)/dPhi
				if(is.infinite(q)) q = 0
				dLon = lon2 - lon1
				if(dLon>pi){
					if(dLon>0) dLon = -(2*pi-dLon)
					if(dLon <= 0) dLon = 2*pi+dLon
				}
				dist = sqrt((lat2-lat1)^2 + q^2 * dLon^2) * 6371 
				shiftregdist10[i,nm] = dist/10 # convert to km/year
				
				# Heading (0° is north) from http://www.movable-type.co.uk/scripts/latlong.html
				# Rhumb line (constant bearing, not great circle)
				shiftreghead10[i,nm] = (atan2(dLon, dPhi)*180/pi + 360) %% 360
			} else {
				shiftregdist10[i,nm] = NA
				shiftreghead10[i,nm] = NA
			}
		}
	}
	
## Output regional shifts
	write.csv(shiftreglat10, file=paste('Output/shiftreglat10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftreglon10, file=paste('Output/shiftreglon10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftreglat10mean, file=paste('Output/shiftreglat10mean_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftreglon10mean, file=paste('Output/shiftreglon10mean_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftregdepth10mean, file=paste('Output/shiftregdepth10mean_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftregdist10, file=paste('Output/shiftregdist10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftreghead10, file=paste('Output/shiftreghead10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftregdepth10, file=paste('Output/shiftregdepth10_', Sys.Date(), '.csv', sep=''))
	write.csv(shiftregbio10, file=paste('Output/shiftregbio10_', Sys.Date(), '.csv', sep=''))

## Read in regional shifts
	setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
	shiftreglat10 = read.csv('Output/shiftreglat10_2012-11-17.csv', row.names=1)
	shiftreglon10 = read.csv('Output/shiftreglon10_2012-11-17.csv', row.names=1)
	shiftreglat10mean = read.csv('Output/shiftreglat10mean_2012-11-17.csv', row.names=1)
	shiftreglon10mean = read.csv('Output/shiftreglon10mean_2012-11-17.csv', row.names=1)
	shiftregdepth10mean = read.csv('Output/shiftregdepth10mean_2012-11-20.csv', row.names=1)
	shiftregdist10 = read.csv('Output/shiftregdist10_2012-11-17.csv', row.names=1)
	shiftreghead10 = read.csv('Output/shiftreghead10_2012-11-17.csv', row.names=1)
	shiftregdepth10 = read.csv('Output/shiftregdepth10_2012-11-17.csv', row.names=1)
	shiftregbio10 = read.csv('Output/shiftregbio10_2012-11-17.csv', row.names=1)
	goodhauls = read.csv('Output/goodhauls_allregions_2012-11-19.csv', row.names=1)


## Figures
	# Speed, heading, and temperature trend as 10-year running means
	quartz(height=6, width=7.5)
	# pdf(height=8, width=7.5, file=paste('Figures/shiftregdist10,head10,bt10,sst10_', Sys.Date(), '.pdf', sep=''))
	par(mfrow=c(9,3), mai=c(0.1, 0.25, 0.05, 0.05), omi = c(0.2, 0.1, 0, 0), mgp = c(1.1,0.3, 0), tcl=-0.3)
	regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')
	regsnice = c('Bering', 'Aleutians', 'Gulf of Alaska', 'West Coast', 'Gulf of Mexico', 'Northeast', 'Scotian Shelf', 'S. Gulf of St. Lawrence', 'Newfoundland')
	
	inds = match(regs, shiftregdist10$region)
	yrs = as.numeric(gsub('X', '', names(shiftreglat10)[tcols]))
	tcols = grep('X', names(shiftregdist10))
	for(i in 1:9){
		plot(yrs, shiftregdist10[inds[i],tcols], type='l', xlab='', ylab='', cex.lab=0.8, cex.axis=0.7, xaxt='n')
		mtext('km/year', side=2, outer=FALSE, cex=0.5, line=1.1)
		legend('topleft', title=regsnice[i], legend='', bty='n', cex=0.7)
		if(i==9) axis(1, cex.axis=0.7)

		plot(yrs, shiftreghead10[inds[i],tcols], type='l', xlab='', ylab='Heading (°)', cex.lab=0.8, cex.axis=0.7, xaxt='n', ylim=c(0,360))
		abline(h=90, col='grey', lwd=0.5)
		abline(h=180, col='grey', lwd=0.5)
		abline(h=270, col='grey', lwd=0.5)
		if(i==9) axis(1, cex.axis=0.7)

		plot(yrs, btmean10[inds[i],tcols], type='l', xlab='', ylab='°C/year', cex.lab=0.8, cex.axis=0.7, xaxt='n', col='blue', ylim=range(c(btmean10[inds[i],tcols], sstmean10[inds[i],tcols]), na.rm=TRUE))
		lines(yrs, sstmean10[inds[i],tcols], col='red')
		abline(h=0, col='grey', lty=2)
		if(i==9) axis(1, cex.axis=0.7)
	}	
	mtext('Year', side=1, outer=TRUE, cex=0.5, adj= 0.17, padj=0.5)
	mtext('Year', side=1, outer=TRUE, cex=0.5, adj= 0.52, padj=0.5)
	mtext('Year', side=1, outer=TRUE, cex=0.5, adj= 0.87, padj=0.5)

	dev.off()

	# Lat and lon as 10-year running means on maps by region
	regs = c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall')
	regsnice = c('Eastern Bering Sea', 'Aleutian Islands', 'Gulf of Alaska', 'West Coast U.S.', 'Gulf of Mexico', 'Northeast U.S.', 'Scotian Shelf', 'So. Gulf of St. Lawrence', 'Newfoundland')
	ylabs = c('Latitude (°N)', '', '', '', 'Latitude (°N)', '', '', 'Latitude (°N)', '')
	xlabs = c('', '', '', 'Longitude (°E)', '', '', 'Longitude (°E)', 'Longitude (°E)', 'Longitude (°E)')
	ylims = list(ebs = c(57.95,58.1), al = c(52.47, 52.60), goa = c(56.6, 57.02), wc = c(43.45, 44.3), gom = c(28.25,28.6), ne = c(40.5, 41.1), ss = c(43.96, 44.25), sl = c(47.07, 47.5), nf = c(48.5, 48.74))
	xlims = list(ebs = c(-168.73,-168.37), al = c(-177.65, -176.48), goa = c(-152.4, -150), wc = c(-124.55, -124.38), gom = c(-93.6,-92.2), ne = c(-70.5, -69.8), ss = c(-63.1, -62.4), sl = c(-63.4, -62), nf = c(-51.72, -51.67))
	loc = c('bottomleft', 'bottomright', 'bottomright', 'bottomleft', 'bottomright', 'bottomright', 'bottomright', 'bottomleft', 'bottomleft') # legend location
	#bcol = 'grey50'
	bcol = 'light green'
	cex = 1.2; col = rgb(0.2, 0.2, 0.2, 0.5) # for paper
	dcols = grep('X', names(shiftreglat10mean)) # columns with data
	#ramp = colorRampPalette(c('blue', 'red'), space='rgb') # custom color ramp (give it a number of colors that you want)
	ramp = colorRamp(c('red', 'blue'), space='rgb') # custom color ramp (give it values 0-1 to represent the locations on the ramp that you want)
	pos1 = c(3,1,3,1,1,1,1,3,1) # position for first year label
	pos2 = c(3,3,3,1,3,3,3,3,3)
	off = 0.5
	
	quartz(width=10, height=6.5) # btemp
	# pdf(width=10, height=6.5, file=paste('Figures/Assemblage_pos_bydecade_', Sys.Date(), '.pdf', sep=''))
	par(mai=c(0.3, 0.35, 0.2, 0.1), mgp = c(2.5, 0.8, 0), omi=c(0.5, 0.5, 0, 0))
	layout(mat=matrix(c(1,2,3,4,5,6,7,4,8,9,10,11), byrow=TRUE, nrow=3))
	for(r in 1:length(regs)){
		i = which(shiftreglat10mean$region == regs[r])
		tcols = dcols[!is.na(shiftreglat10mean[i,dcols])] # columns with data
		x=as.numeric(shiftreglon10mean[i,tcols])
		y=as.numeric(shiftreglat10mean[i,tcols])
		z = as.numeric(shiftregdepth10mean[i,tcols])
		#cols = ramp(length(tcols)) # for use with colorRampPalette
		c = z-min(z); c = c/max(c); c2 = ramp(c); cols = rgb(c2[,1], c2[,2], c2[,3], maxColorValue=255) # for use with colorRamp
		plot(0,0, pch=16, cex=cex, main=regsnice[r], xlab='', ylab='', col='white', ylim=ylims[[r]], xlim=xlims[[r]], type='p', las=1) # set up plot
		lines(x,y, col='grey')
		points(x,y, pch=16, cex=cex, col=cols)		
		mtext(xlabs[r], 1, line=2.6)
		mtext(ylabs[r], 2, line=3.6)
		theseyrs = as.numeric(gsub('X', '', names(shiftreglat10mean)[tcols]))
		text(x[1], y[1], labels=theseyrs[1], pos=pos1[r], cex=1, offset=off)
		text(x[length(x)], y[length(y)], labels=theseyrs[length(x)], pos=pos2[r], cex=1, offset = off)
		#try(map2('world', xlim=xlims[[r]], ylim=ylims[[r]], fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, add=TRUE))
		legend(loc[r], col=c('red', 'blue'), legend=paste(round(range(z)), 'm'), pch=16, bty='o', title='Depth')
	}
	
		dev.off()


###########################################
## Plots of trends by taxa and by region ##
###########################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
spppres1 = read.csv('Output/sppres1_2012-11-21.csv', row.names=1)
shift = read.csv('Output/shift_2012-11-15.csv', row.names=1)
#shiftreg = read.csv('Output/shiftreg_2012-08-05.csv', row.names=1); suff='' # shifts by region
#shiftreg = read.csv('Output/shiftreg_threshmin5ave0_2012-07-27.csv', row.names=1); suff = '_threshmin5ave0' # shifts by region, only using spp with >=5 presences in every year
shiftreg = read.csv('Output/shiftreg_onlygenera_2013-02-19.csv', row.names=1); suff = '_onlygenera' # shifts by region, only using spp with >=5 presences in every year
minlat = read.csv('Output/minlat_2012-10-16.csv', row.names=1)
maxlat = read.csv('Output/maxlat_2012-10-16.csv', row.names=1)
centbiolatindiv = read.csv('Output/centbiolatindiv_2012-10-16.csv', row.names=1)
centbiolat = read.csv('Output/centbiolat_2012-10-16.csv', row.names=1)
surveyminlat = read.csv('Output/surveyminlat_2012-10-16.csv', row.names=1)
surveymaxlat = read.csv('Output/surveymaxlat_2012-10-16.csv', row.names=1)
spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1) # matches taxon name to taxonomy
source('map2.R') # make maps with a specified polygon border color

# Trim
regs = c('AFSC_Aleutians', 'AFSC_EBS', 'AFSC_GOA', 'DFO_Newfoundland_Fall', 'DFO_ScotianShelf', 'DFO_SoGulf', 'NEFSC_Spring', 'SEFSC_GOMex', 'WestCoast_Tri')
i = rep(TRUE, nrow(shift))
if(suff == '_onlygenera'){
	shift$regspp = paste(shift$region, shift$spp, sep='_')
	row.names(spppres1) = as.character(spppres1$regspp)
	spppres1 = spppres1[as.character(shift$regspp),] # put spppres1 in correct order
	if(all(spppres1$regspp == shift$regspp)){ # if in the right order
		i = i & spppres1$use
	} else { print("WRONG ORDER!") }
}
i = i & shift$region %in% regs # trim to 9 regions
sum(i) # 580 w/ _onlygenera and trimmed to 9 regions
shift = shift[i,]
	dim(shift)

	# write.csv(shift, file=paste('Output/shift_trim_', Sys.Date(), '.csv', sep=''))

inds2 = shiftreg$region %in% regs
shiftreg = shiftreg[inds2,]
	dim(shiftreg)

# Add a nice name for the taxa in centbiolatindiv
setdiff(centbiolatindiv$spp, spptax$taxon) # should be 0
centbiolatindiv = merge(centbiolatindiv, spptax, by.x = 'spp', by.y = 'taxon', all.xy=TRUE)
	dim(centbiolatindiv) # 721 x 61
	unique(centbiolatindiv$kingdom) # Animalia, not NA
centbiolatindiv = centbiolatindiv[order(centbiolatindiv$regspp),]

# Histogram of shifts
	quartz(width=7, height=3)
	# pdf(paste('Figures/shift hist', suff, '_', Sys.Date(), '.pdf', sep=''), width=7, height=5)
	par(mfrow=c(1,3))
	hist(shift$centbiolatindiv.b.lm, breaks=30, col='grey', main='Center of biomass (individual)', xlab='Rate of shift (° latitude/year)')
	hist(shift$minlat.b.lm, breaks=30, col='grey', main='Southern boundary', xlab='Rate of shift (° latitude/year)')
	hist(shift$maxlat.b.lm, breaks=30, col='grey', main='Northern boundary', xlab='Rate of shift (° latitude/year)')
	
		t.test(shift$centbiolat.b.lm)
			sum(!is.na(shift$centbiolat.b.lm))
		t.test(shift$minlat.b.lm)
			sum(!is.na(shift$minlat.b.lm))
		t.test(shift$maxlat.b.lm)
			sum(!is.na(shift$maxlat.b.lm))

	dev.off()


## Plot species velocity vectors on a map (use centbioindiv and lm) of North America
	source('map2.R') # make maps with a specified polygon border color
	require(maps)
	require(mapdata)
	require(mapproj)
	require(RColorBrewer)
	require(grImport)

	# read in illustrations
		# convert to XML (only need to do this 1x)
#	PostScriptTrace('Figure_illustrations/cod_black_vector.eps', outfilename='Figure_illustrations/cod_black_vector.eps.xml') # fail if outfilename has a space
#	PostScriptTrace('Figure_illustrations/lobster_black_vector.eps', outfilename='Figure_illustrations/lobster_black_vector.eps.xml')
#	PostScriptTrace('Figure_illustrations/skate_black_vector.eps', outfilename='Figure_illustrations/skate_black_vector.eps.xml')

		# read in .ps
	cod = readPicture('Figure_illustrations/cod_black_vector.eps.xml')
	lobster = readPicture('Figure_illustrations/lobster_black_vector.eps.xml')
	skate = readPicture('Figure_illustrations/skate_black_vector.eps.xml')

	# set up mean starting and ending lats and lons
	lat1 = shift$centbiolatindiv.a.lm + shift$centbiolatindiv.b.lm*shift$minyr
	lat2 = shift$centbiolatindiv.a.lm + shift$centbiolatindiv.b.lm*shift$maxyr
	lon1 = shift$centbiolonindiv.a.lm + shift$centbiolonindiv.b.lm*shift$minyr
	lon2 = shift$centbiolonindiv.a.lm + shift$centbiolonindiv.b.lm*shift$maxyr
	
	# Arrows on a map
	#bcol = brewer.pal(n=9, name='BrBG')[4]
	bcol = 'grey50'
	#display.brewer.all()
	#set = 'Set3'
	set = 'Paired'
	cols = brewer.pal(n=length(regs), name=set); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	shiftcols = cols[match(shift$region, shiftreg$region[shiftreg$region %in% regs])]

	quartz(width=7, height=5)
	# pdf(width=7, height=5, file=paste('Figures/Sppvelocity_', Sys.Date(), '.pdf', sep=''))
	par(omi = c(0,0.1,0,0), mai=c(0.3, 0.4, 0.05, 0.05), ps=9, cex=1, cex.lab=1, mgp=c(2, 0.5, 0), fig = c(0,1,0.4,0.98), tcl=-0.3)
	plot(0,0, xlim=c(-185,-50), ylim=c(27,63), xaxt='n', yaxt='n', xlab='', ylab='')
	one = map2('worldHires', regions=c('USA', 'Canada', 'Mexico', 'Greenland'), fill=TRUE, wrap=FALSE, plot=FALSE)
		polygon(one, col=bcol, border=bcol)
		#oneb = map2('worldHires', regions=c('USA', 'Canada', 'Mexico', 'Greenland'), fill=FALSE, wrap=FALSE, plot=FALSE) # for only lines (faster)
		#lines(oneb, col=bcol) # for only lines (faster)
	two = map2('worldHires', regions='USSR', xlim=c(170, 180), ylim=c(25,68), fill=TRUE, wrap=FALSE, plot=FALSE) # for adding land west of -180° lon
		two$x = two$x - 360
		polygon(two, col=bcol, border=NA)
		axis(1, mgp=c(2, 0.4, 0)); axis(2, mgp=c(2, 0.7, 0), las=1); box()
		mtext("Longitude (°)", side=1, line=1.3)
		mtext("Latitude (°)", side=2, line=1.7, las=0)
		mtext('A', side=3, adj=0, font=2)
	points(lon1, lat1, pch=16, cex=0.4, col=shiftcols)
	arrows(x0 = lon1, y0 = lat1, x1 = lon2, y1 = lat2, length = 0.06, lwd=0.25, col=shiftcols) # taxon shifts
	arrows(x0 = shiftreg$lon1, y0 = shiftreg$lat1, x1 = shiftreg$lon2, y1 = shiftreg$lat2, length = 0.15, col='black', lwd=2) # mean shifts in each region

		# Inset charts
		lcol = grep('X', names(maxlat))
		yrs = as.numeric(gsub('X', '', names(maxlat)[lcol]))
		inregs = c('AFSC_GOA', 'WestCoast_Tri', 'NEFSC_Spring')
		spp = c('Gadus macrocephalus', 'Raja binoculata', 'Homarus americanus')
		flims = list(one = c(0.205, 0.325, 0.67, 0.82), two=c(0.34, 0.46, 0.51, 0.66), three = c(0.85, 0.97, 0.50, 0.65)) # figure inset limits, in fractional coords
		ylims = list(one = c(52,61), two = c(36,49), three=c(36,44.5))
		for(p in 1:length(inregs)){
			opar = par(fig = flims[[p]], new=TRUE, mai=c(0,0,0.2,0), cex=0.5, mgp = c(1, 0.1, 0), tcl = -0.3, cex.axis=1)
			par(fig=flims[[p]]) # not clear why I need this, but axes don't print correctly without it
			k = centbiolatindiv$region == inregs[p] & centbiolatindiv$name == spp[p]
			tcol = which(!is.na(colSums(maxlat[k,lcol]))) # columns for this region
			plot(yrs[tcol], maxlat[k,lcol][,tcol], col='blue', type='o', xlab='', ylab='', main='', ylim = ylims[[p]], pch=16, cex=0.5, las=1, xaxt='n', yaxt='n')
				axis(1)
				axis(2, las=1, mgp=c(2,0.8,0))
			points(yrs[tcol], minlat[k,lcol][,tcol], col='red', pch=16, cex=0.5, type='o')
			points(yrs[tcol], centbiolatindiv[k,lcol][,tcol], col='black', pch=16, cex=0.5, type='o') # grey is individual-based lat
			points(yrs[tcol], surveyminlat[surveyminlat$region==inregs[p], grep('X', names(surveyminlat))][tcol], col='grey', lty=2, type='l')
			points(yrs[tcol], surveymaxlat[surveymaxlat$region==inregs[p], grep('X', names(surveymaxlat))][tcol], col='grey', lty=2, type='l')
			mtext(text='Latitude (°)', side=2, line=1.8, cex=0.5)
			mtext(text='Year', side=1, line=0.7, cex=0.5)
			mtext(text=centbiolatindiv$name[k], side=3, line=0.1, cex=0.5, font=3)
			par(opar)
		}

		# Inset illustrations
		grid.picture(cod, x=0.13, y=0.72, width=0.07)
		grid.picture(skate, x=0.27, y=0.55, width=0.07)
		grid.picture(lobster, x=0.92, y=0.645, width=0.05)

	# Subplot B
	par(fig=c(0,0.33,0.04,0.35), new=TRUE, tcl=-0.3, mai=c(0.25, 0.4, 0.05, 0.05))
	plot(0, 0, xlim=c(-177,-160), ylim=c(55,61), xaxt='n', yaxt='n')
	map2('worldHires', regions='USA:Alaska', fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, add=TRUE)
		ind = shift$region == 'AFSC_EBS'
	points(lon1[ind], lat1[ind], pch=16, cex=0.4, col=shiftcols[ind], xlim=c(-173,-130), ylim=c(52,60), new=FALSE)
	arrows(x0 = lon1[ind], y0 = lat1[ind], x1 = lon2[ind], y1 = lat2[ind], length = 0.06, lwd=0.25, col=shiftcols[ind])
		axis(1, mgp=c(2, 0.3, 0)); axis(2, mgp=c(2, 0.6, 0), las=1)
		mtext("Longitude (°)", side=1, line=1.2)
		mtext("Latitude (°)", side=2, line=1.7, las=0)
		mtext('B', side=3, adj=0, font=2)
	
	# Subplot C
	par(fig=c(0.33,0.66,0.04,0.35), new=TRUE)
	plot(0, 0, xlim=c(-98,-88), ylim=c(26.3,30.5), xaxt='n', yaxt='n')
	map2('usa', regions=c('main'), fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, add=TRUE)
		ind = shift$region == 'SEFSC_GOMex'
	points(lon1[ind], lat1[ind], pch=16, cex=0.4, col=shiftcols[ind], xlim=c(-173,-130), ylim=c(52,60), new=FALSE)
	arrows(x0 = lon1[ind], y0 = lat1[ind], x1 = lon2[ind], y1 = lat2[ind], length = 0.06, lwd=0.25, col=shiftcols[ind])
		axis(1, mgp=c(2, 0.3, 0)); axis(2, mgp=c(2, 0.6, 0), las=1)
		mtext("Longitude (°)", side=1, line=1.2)
		mtext('C', side=3, adj=0, font=2)

	# Subplot D
	par(fig=c(0.66,1,0.04,0.35), new=TRUE)
	plot(0, 0, xlim=c(-58,-48), ylim=c(43,53), xaxt='n', yaxt='n')
	map2('worldHires', regions=c('Canada', 'Canada:Newfoundland', "Canada:New World Island", "Canada:Fogo Island", "Canada:Bell Island", "Canada:Merasheen Island", "Canada:Random Island"), exact=TRUE, fill=TRUE, col=bcol, bcol=bcol, wrap=FALSE, myborder = 0, add=TRUE)
		ind = shift$region == 'DFO_Newfoundland_Fall'
	points(lon1[ind], lat1[ind], pch=16, cex=0.4, col=shiftcols[ind], xlim=c(-173,-130), ylim=c(52,60), new=FALSE)
	arrows(x0 = lon1[ind], y0 = lat1[ind], x1 = lon2[ind], y1 = lat2[ind], length = 0.06, lwd=0.25, col=shiftcols[ind])
		axis(1, mgp=c(2, 0.3, 0)); axis(2, mgp=c(2, 0.6, 0), las=1)
		mtext("Longitude (°)", side=1, line=1.2)
		mtext('D', side=3, adj=0, font=2)



	dev.off()

## Spp velocity for Northeast US
	source('map2.R') # make maps with a specified polygon border color
	require(maps)
	require(mapdata)
	require(mapproj)
	require(RColorBrewer)
	#require(grImport)

	regs = c('NEFSC_Spring')
		inds = shift$region %in% regs
		# inds = shift$region %in% regs & shift$spp == 'Urophycis chuss'
		# inds = shift$region %in% regs & shift$spp == 'Homarus americanus'
		# inds = shift$region %in% regs & shift$spp == 'Doryteuthis (Amerigo) pealeii'
		# inds = shift$region %in% regs & shift$spp %in% c('Urophycis chuss', 'Homarus americanus')
		inds = shift$region %in% regs & shift$spp %in% c('Paralichthys dentatus')
		# inds = shift$region %in% regs & shift$spp %in% c('Paralichthys dentatus', 'Homarus americanus')
	sum(inds) 
	
	lat1 = shift$centbiolatindiv.a.lm[inds] + shift$centbiolatindiv.b.lm[inds]*shift$minyr[inds]
	lat2 = shift$centbiolatindiv.a.lm[inds] + shift$centbiolatindiv.b.lm[inds]*shift$maxyr[inds]
	lon1 = shift$centbiolonindiv.a.lm[inds] + shift$centbiolonindiv.b.lm[inds]*shift$minyr[inds]
	lon2 = shift$centbiolonindiv.a.lm[inds] + shift$centbiolonindiv.b.lm[inds]*shift$maxyr[inds]
	
	# Add a nice name for the taxa
	setdiff(centbiolatindiv$spp, spptax$taxon) # should be 0
	centbiolatindiv = merge(centbiolatindiv, spptax, by.x = 'spp', by.y = 'taxon', all.xy=TRUE)
		dim(centbiolatindiv) # 721 x 61
		unique(centbiolatindiv$kingdom) # Animalia, not NA
	centbiolatindiv = centbiolatindiv[order(centbiolatindiv$regspp),]

	# Arrows on a map
	#bcol = brewer.pal(n=9, name='BrBG')[4]
	bcol = 'grey50'; bordercol = 'white'
	#display.brewer.all()
	#set = 'Set3'
	set = 'Paired'
#	cols = brewer.pal(n=9, name=set); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
#	shiftcols = cols[7]
	shiftcols = 'black'
	#bcol = rgb(44/255,162/255,95/255); bordercol = 'black'; shiftcols = 'black'

	quartz(width=7, height=7)
	# pdf(width=7, height=7, file=paste('Figures/Sppvelocity_NEUS_', Sys.Date(), '.pdf', sep=''))
	par(omi = c(0.5,0.5,0,0))
	map2('worldHires', xlim=c(-79,-63), ylim=c(35,46), fill=TRUE, col=bcol, bcol=NA, wrap=FALSE, myborder = 0)
	map2(database='state', add=TRUE, fill=TRUE, col=bcol, bcol=bordercol)
		axis(1); axis(2, las=1); box()
		mtext("Longitude", side=1, line=2.5)
		mtext("Latitude", side=2, line=2.5)
	points(lon1, lat1, pch=16, cex=1, col=shiftcols)
	arrows(x0 = lon1, y0 = lat1, x1 = lon2, y1 = lat2, length = 0.06, lwd=2, col=shiftcols)

		inds2 = shiftreg$region %in% regs
	arrows(x0 = shiftreg$lon1[inds2], y0 = shiftreg$lat1[inds2], x1 = shiftreg$lon2[inds2], y1 = shiftreg$lat2[inds2], length = 0.25, col='black', lwd=8)

		dev.off()


## Compare depth and lat shifts
	regs = c('AFSC_Aleutians', 'AFSC_EBS', 'AFSC_GOA', 'DFO_Newfoundland_Fall', 'DFO_ScotianShelf', 'DFO_SoGulf', 'NEFSC_Spring', 'SEFSC_GOMex', 'WestCoast_Tri')
	regsnice = c('Aleutians', 'Bering', 'Gulf of Alaska', 'Newfoundland', 'Scotian Shelf', 'S. Gulf of St. Lawrence', 'East Coast', 'Gulf of Mexico', 'West Coast')
	inds = shiftreg$region %in% regs
	cols = brewer.pal(n=length(regs), name='Paired'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region

	quartz(width=5, height=5)
	# pdf(width=5, height=5, file=paste('Figures/shiftreg centbiodepthindiv centbiolatindiv_', Sys.Date(), '.pdf', sep=''))
	plot(shiftreg$centbiolatindiv.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], ylab='Depth shift (m/year)', xlab= 'Latitudinal shift (°N/year)', pch=16, col=cols)
	legend('bottomleft', legend=regsnice, col=cols, pch=16, cex=0.5)

	dev.off()

### Compare spp shifts to max possible shift in a region
	sc = data.frame(region = shiftreg$region, maxlatshift = shiftreg$rangelat/(shiftreg$maxyr - shiftreg$minyr), maxdepthshift = shiftreg$rangedepth/(shiftreg$maxyr - shiftreg$minyr)) # scale rate of shift by max rate of shift given year and lat ranges
	scaledshift = shift[i,c('region', 'spp', 'centbiolatindiv.b.lm', 'centbiodepthindiv.b.lm')] # i trims this to onlygenera and regs
	scaledshift$scaledlat = NA
	scaledshift$scaleddepth = NA
	scaledshift$maxlatshift = NA
	scaledshift$maxdepthshift = NA
	for(k in 1:nrow(scaledshift)){
		j = which(sc$region == scaledshift$region[k])
		scaledshift$maxlatshift[k] = sc$maxlatshift[j]
		scaledshift$maxdepthshift[k] = sc$maxdepthshift[j]
		scaledshift$scaledlat[k] = abs(scaledshift$centbiolatindiv.b.lm[k]/sc$maxlatshift[j])
		scaledshift$scaleddepth[k] = abs(scaledshift$centbiodepthindiv.b.lm[k]/sc$maxdepthshift[j])
	}
	dim(scaledshift)

	summary(scaledshift$scaledlat)
		head(scaledshift[order(scaledshift$scaledlat, decreasing=TRUE), c('region', 'spp', 'centbiolatindiv.b.lm', 'scaledlat')])
		hist(scaledshift$scaledlat)
	summary(scaledshift$scaleddepth)
		head(scaledshift[order(scaledshift$scaleddepth, decreasing=TRUE), c('region', 'spp', 'centbiodepthindiv.b.lm', 'scaleddepth')])
		hist(scaledshift$scaleddepth)

	quartz(width=8, height=4)
	par(mfrow=c(1,2))
	plot(scaledshift$maxlatshift, abs(scaledshift$centbiolatindiv.b.lm))
		summary(mod1 <- lm(I(abs(centbiolatindiv.b.lm)) ~ maxlatshift, data=scaledshift))
		abline(mod1, col='red')
	plot(scaledshift$maxdepthshift, scaledshift$centbiodepthindiv.b.lm)
		summary(mod2 <- lm(centbiodepthindiv.b.lm ~ maxdepthshift, data=scaledshift))
		abline(mod2, col='red')


######################################
## Output shifts for William Cheung ##
######################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
shift = read.csv('Output/shift_2012-08-05.csv', row.names=1)
spptax = read.csv('Life history/data/spptaxonomy_2012-08-12 manual.csv', row.names=1) # matches taxon name to taxonomy
cheung = read.csv('Cheung/spplist_Cheung_2012-06-13_WLC.csv', stringsAsFactors = TRUE) # list of spp for which William has data

## Merge in taxonomy
names(shift)[names(shift)=='spp'] = 'taxon'
setdiff(shift$taxon, spptax$taxon) # should be 0

shiftlh = merge(shift, spptax, by='taxon', all.x=TRUE)
	dim(shift)
	dim(shiftlh)

	# Examine rows that didn't match
		i = is.na(shiftlh$kingdom)
		shiftlh[i, c('taxon', 'kingdom')] # 0, good

shiftlh$sppname = paste(shiftlh$genus, shiftlh$species)


# Output all spp names
	out = shiftlh[!is.na(shiftlh$species),c('sppname', 'region')]
	out = out[order(out$sppname, out$region),]
	
	write.csv(out, paste('Cheung/spplist_Cheung_', Sys.Date(), '.csv', sep=''), row.names=FALSE)
	

# Choose which taxa in which regions to output with shift data
	inds = !is.na(shiftlh$species) & shiftlh$sppname %in% cheung$spp[cheung$DBEM_list == 'TRUE']
		sum(inds) # 151
	
	out = shiftlh[inds,c('sppname', 'region', 'centbiolatindiv.b.lm')]
	out = out[order(out$sppname, out$region),]
	names(out)[3] = 'deglatperyear'
	
	write.csv(out, paste('Cheung/shift_Cheung_', Sys.Date(), '.csv', sep=''), row.names=FALSE)


############################################
## Compare assemblage shift to temp shift ##
############################################
setwd('/Users/mpinsky/Documents/Princeton/Trawl Data/')
source('error.bar 2012-04-06.R')
require(RColorBrewer)
#shiftreg = read.csv('Output/shiftreg_2012-08-05.csv', row.names=1); suff=''
shiftreg = read.csv('Output/shiftreg_onlygenera_2013-07-01.csv', row.names=1); suff = '_onlygenera' # shifts by region, only using spp with >=5 presences in every year
shift = read.csv('Output/shift_2012-11-15.csv', row.names=1)
	shift2 = merge(shift, shiftreg[,c('region', 'btmean.b.lm', 'sstmean.b.lm')], by='region') # add rate of temperature change
	dim(shift)
	dim(shift2)
regcentbiolatindiv = read.csv('Output/regcentbiolatindiv_onlygenera_2012-10-16.csv', row.names=1)
regcentbiodepthindiv = read.csv('Output/regcentbiodepthindiv_onlygenera_2012-11-15.csv', row.names=1)
btmean = read.csv('Output/btmean_2012-10-16.csv', row.names=1)
sstmean = read.csv('Output/sstmean_2012-10-16.csv', row.names=1)


# BT and SST and lat and depth by haul (Fig. 2 for paper)
	regs = c('AFSC_Aleutians', 'AFSC_EBS', 'AFSC_GOA', 'DFO_Newfoundland_Fall', 'DFO_ScotianShelf', 'DFO_SoGulf', 'NEFSC_Spring', 'SEFSC_GOMex', 'WestCoast_Tri')
	inds = shiftreg$region %in% regs
	cols = brewer.pal(n=length(regs), name='Paired'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	cexlab = 1; cexpt = 1.5

	quartz(width=4.6, height=4.6)
	# pdf(paste('Figures/shiftreg centbiolat&depthindivlm btemplm&stemplm', suff, '_', Sys.Date(), '.pdf', sep=''), width=4.6, height=4.6)
	par(mfrow=c(2,2), mgp = c(3.5,0.6, 0), mai=c(0.15,0.15,0.1, 0.05), omi=c(0.5,0.4,0,0), ps=9, tcl=-0.3)
	plot(shiftreg$btmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], ylim=c(-0.023, 0.018), xlim=c(-0.063, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, las=1, xaxt='n')
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$centbiolatindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
		mex = which(shiftreg$region == 'SEFSC_GOMex')
	points(shiftreg$btmean.b.lm[c(mex, mex)], shiftreg$centbiolatindiv.b.lm[c(mex,mex)], pch=1, cex=c(1.7,2)) # mark GoMex with a black circle
	mtext('Latitudinal shift (°N/yr)', side=2, line=2.3, cex=cexlab)
	mtext('A', side=3, line=-1, adj=0.03, cex=1, font=2)

		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))
		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm+julianmean.b.lm*season, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))

	plot(shiftreg$sstmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], ylim=c(-0.023, 0.018), xlim=c(-0.102, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, yaxt='n', xaxt='n')
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$centbiolatindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
	mtext('B', side=3, line=-1, adj=0.03, cex=1, font=2)
		summary(lm(centbiolatindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiolatindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))
				
	plot(shiftreg$btmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], ylim=c(-2, 1), xlim=c(-0.063, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, las=1)
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$centbiodepthindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
	mtext('Bottom temperature\nchange (°C/yr)', 1, 2.6, cex=cexlab)
	mtext('Depth shift (m/yr)', 2, 2.3, cex=cexlab)
	mtext('C', side=3, line=-1, adj=0.03, cex=1, font=2)

		summary(lm(centbiodepthindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiodepthindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds & !(shiftreg$region %in% c('SEFSC_GOMex')),]))

	plot(shiftreg$sstmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], ylim=c(-1.5, 1), xlim=c(-0.11, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, yaxt='n')
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$centbiodepthindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
	mtext('Surface temperature\nchange (°C/yr)', 1, 2.6, cex=cexlab)
	mtext('D', side=3, line=-1, adj=0.03, cex=1, font=2)
		summary(lm(centbiodepthindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiodepthindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))


	dev.off()
	

# BT and SST (not julian-day corrected) and lat and depth by haul (not strat) with ALL TAXA
	regs = c('AFSC_Aleutians', 'AFSC_EBS', 'AFSC_GOA', 'DFO_Newfoundland_Fall', 'DFO_ScotianShelf', 'DFO_SoGulf', 'NEFSC_Spring', 'SEFSC_GOMex', 'WestCoast_Tri')
	inds = shiftreg$region %in% regs
	cols = brewer.pal(n=length(regs), name='Paired'); cols2 = col2rgb(cols); cols = rgb(cols2[1,], cols2[2,], cols2[3,], alpha=255, maxColorValue=255) # for coloring by region
	cexlab = 0.7; cexpt = 1.5
	## Determine which species to keep (depends on # presences)
	if(suff == '_onlygenera'){
		shift2$regspp = paste(shift2$region, shift2$spp, sep='_')
		row.names(spppres1) = as.character(spppres1$regspp)
		spppres1 = spppres1[as.character(shift2$regspp),] # put spppres1 in correct order
		if(all(spppres1$regspp == shift2$regspp)){ # make sure they're in the same order
			inds2 = spppres1$use & shift2$region %in% regs
		} else {
			warning("spppres1 and shift not in same order!")
		}
	}
	sum(inds2) # 659 w/ _onlygenera (in all 11 regions)
				# 580 in 9 regions I analyze

	quartz(width=5, height=5)
	# pdf(paste('Figures/shiftreg centbiolat&depthindivlm btemplm&stemplm', suff, '_', Sys.Date(), '.pdf', sep=''), width=5, height=5)
	par(mfrow=c(2,2), mgp = c(2.5,0.7, 0), mai=c(0.2,0.2,0.1, 0.05), omi=c(0.5,0.5,0,0))
	plot(shiftreg$btmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], ylim=c(-0.023, 0.018), xlim=c(-0.063, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, las=1, xaxt='n')
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$centbiolatindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
		mex = which(shiftreg$region == 'SEFSC_GOMex')
	points(shiftreg$btmean.b.lm[c(mex, mex)], shiftreg$centbiolatindiv.b.lm[c(mex,mex)], pch=1, cex=c(1.7,2)) # mark GoMex with a black circle
	mtext('Assemblage latitudinal shift (°N/yr)', 2, 2.7, cex=cexlab)
	mtext('a)', side=3, line=-1.7, adj=0.03, cex=1.5)

		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))

		# Add all taxa points
		points(shift2$btmean.b.lm[inds2], shift2$centbiolatindiv.b.lm[inds2], col='grey', cex=0.3, pch=16)

		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm, data=shift2[inds2,]))
			sum(complete.cases(shift2[inds2, c('centbiolatindiv.b.lm', 'btmean.b.lm')]))
		summary(lm(centbiolatindiv.b.lm ~ btmean.b.lm, data=shift2[inds2 & shift2$region != 'SEFSC_GOMex',]))
			sum(complete.cases(shift2[inds2 & shift2$region != 'SEFSC_GOMex', c('centbiolatindiv.b.lm', 'btmean.b.lm')]))
		

	plot(shiftreg$sstmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], ylim=c(-0.023, 0.018), xlim=c(-0.102, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, yaxt='n', xaxt='n')
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$centbiolatindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiolatindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
	mtext('b)', side=3, line=-1.7, adj=0.03, cex=1.5)

		summary(lm(centbiolatindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiolatindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))

		# Add all taxa points
		points(shift2$sstmean.b.lm[inds2], shift2$centbiolatindiv.b.lm[inds2], col='grey', cex=0.3, pch=16)

		summary(lm(centbiolatindiv.b.lm ~ sstmean.b.lm, data=shift2[inds2,]))
			sum(complete.cases(shift2[inds2, c('centbiolatindiv.b.lm', 'sstmean.b.lm')]))
		summary(lm(centbiolatindiv.b.lm ~ sstmean.b.lm, data=shift2[inds2 & shift2$region != 'SEFSC_GOMex',]))
			sum(complete.cases(shift2[inds2 & shift2$region != 'SEFSC_GOMex', c('centbiolatindiv.b.lm', 'sstmean.b.lm')]))

				
	plot(shiftreg$btmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], ylim=c(-2, 1), xlim=c(-0.063, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, las=1)
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$centbiodepthindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$btmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
	mtext('Change in bottom temperature (°C/yr)', 1, 2.5, cex=cexlab)
	mtext('Assemblage depth shift (m/yr)', 2, 2.7, cex=cexlab)
	mtext('c)', side=3, line=-1.7, adj=0.03, cex=1.5)

		summary(lm(centbiodepthindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiodepthindiv.b.lm ~ btmean.b.lm, data=shiftreg[inds & !(shiftreg$region %in% c('SEFSC_GOMex')),]))

		# Add all taxa points
		points(shift2$btmean.b.lm[inds2], shift2$centbiodepthindiv.b.lm[inds2], col='grey', cex=0.3, pch=16)

		summary(lm(centbiodepthindiv.b.lm ~ btmean.b.lm, data=shift2[inds2,]))
			sum(complete.cases(shift2[inds2, c('centbiodepthindiv.b.lm', 'btmean.b.lm')]))
		summary(lm(centbiodepthindiv.b.lm ~ btmean.b.lm, data=shift2[inds2 & shift2$region != 'SEFSC_GOMex',]))
			sum(complete.cases(shift2[inds2 & shift2$region != 'SEFSC_GOMex', c('centbiodepthindiv.b.lm', 'btmean.b.lm')]))

	plot(shiftreg$sstmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], ylim=c(-1.5, 1), xlim=c(-0.11, 0.055), xlab='', ylab='', pch=16, main='', col=cols, cex=cexpt, cex.axis = cexlab, cex.lab=cexlab, yaxt='n')
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$centbiodepthindiv.b.lm.se[inds], length=0.03, col=cols)
		error.bar(shiftreg$sstmean.b.lm[inds], shiftreg$centbiodepthindiv.b.lm[inds], upper = shiftreg$btmean.b.lm.se[inds], length=0.03, dir='x', col=cols)
	mtext('Change in surface temperature (°C/yr)', 1, 2.5, cex=cexlab)
	mtext('d)', side=3, line=-1.7, adj=0.03, cex=1.5)

		summary(lm(centbiodepthindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds,]))
		summary(lm(centbiodepthindiv.b.lm ~ sstmean.b.lm, data=shiftreg[inds & shiftreg$region != 'SEFSC_GOMex',]))

		# Add all taxa points
		points(shift2$sstmean.b.lm[inds2], shift2$centbiodepthindiv.b.lm[inds2], col='grey', cex=0.3, pch=16)

		summary(lm(centbiodepthindiv.b.lm ~ sstmean.b.lm, data=shift2[inds2,]))
			sum(complete.cases(shift2[inds2, c('centbiodepthindiv.b.lm', 'sstmean.b.lm')]))
		summary(lm(centbiodepthindiv.b.lm ~ sstmean.b.lm, data=shift2[inds2 & shift2$region != 'SEFSC_GOMex',]))
			sum(complete.cases(shift2[inds2 & shift2$region != 'SEFSC_GOMex', c('centbiodepthindiv.b.lm', 'sstmean.b.lm')]))

	dev.off()


	
##### Assemblage time-series vs. temperature time-series #####
	all(regcentbiolatindiv$region == btmean$region) # TRUE
	all(regcentbiodepthindiv$region == sstmean$region) # TRUE
	#cols=brewer.pal(3, 'Dark2') # lat, depth, temp

	# Lat and btemp (Fig. S3 for paper)
	cols = c('black', 'red', 'blue')
	inds = match(c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf', 'DFO_Newfoundland_Fall'), regcentbiolatindiv$region)
	ylabs = c('Latitude offset (°)', '', '', 'Latitude offset (°)', '', '', 'Latitude offset (°)', '', '')
	ylabs2 = c('', '', 'Temperature (°C)', '', '', 'Temperature (°C)', '', '', 'Temperature (°C)')
	xlabs = c('', '', '', '', '', '', 'Year', 'Year', 'Year')
	labs = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')
	offset = c(0.6, 0.6, 0.6, -0.7, 0.6, -0.6, 0.6, 0.6, 0.6) # how much to elevate temp above lat offset in graph
	labcex = 1 # for axis labels
	axiscex = 1.5 # for numbers on the axes
	ylims = c(-0.5, 1.3) # for first y-axis (offsets)
	axis2by = c(2, 1, 1, 0.5, 1, 1, 2, 1, 1) # interval for second y-axis

	quartz(width=10, height=7)
	nm = paste('Figures/regcentlatindiv vs. btmean_', Sys.Date(), '.pdf', sep=''); nm
	# pdf(width=10, height=7, file=nm)
	par(mfrow=c(3,3), mai=c(0.4, 0.4, 0.1, 0.4), mgp = c(3, 0.9,0), omi = c(0.4, 0.3, 0, 0.2))
	ycols = grep('X', names(regcentbiolatindiv))
	tcols = grep('X', names(btmean))
	for(i in 1:length(inds)){
		x = as.numeric(gsub('X', '', names(regcentbiolatindiv)[ycols]))
		y = regcentbiolatindiv[inds[i],ycols]
		x = x[!is.na(y)]
		y = y[!is.na(y)]
		x3 = as.numeric(gsub('X', '', names(btmean)[tcols]))
		y3 = btmean[inds[i],tcols]
		x3 = x3[!is.na(y3)]
		y3 = y3[!is.na(y3)]
		b = mean(y3)
		m = sd(y)/sd(y3)
		y3p = m*(y3-b) + mean(y) + offset[i]
		#ylim = range(c(y, y3p)) + c(0, 0.15)
		plot(x, y, type='l', ylim=ylims, main='', xlab='', ylab='', col=cols[1], lwd=2, las=1, cex.axis=axiscex)
			mtext(side=2, text=ylabs[i], line=3.8, col=cols[1], cex=labcex)
			mtext(side=1, text=xlabs[i], line=2.7, cex=labcex)
		lines(x3, y3p, col=cols[3], lwd=2)
	
		# trend lines
		mod = lm(y ~ x)
		lines(x, predict(mod), lwd=1, col=cols[1], lty=2)	
		mod = lm(y3p ~ x3)
		lines(x3, predict(mod), lwd=1, col=cols[3], lty=2)	
		if(regcentbiolatindiv$region[inds[i]] == 'AFSC_EBS'){ # add partial line as well in Eastern Bering Sea
			xa = x[x<=2006]; ya = y[x<=2006]; mod = lm(ya ~ xa)
			lines(xa, predict(mod), lwd=1, col=cols[1], lty=3)
			xb = x[x<=2006]; yb = y3p[x<=2006]; mod = lm(yb ~ xb)
			lines(xb, predict(mod), lwd=1, col=cols[3], lty=3)
		}

		tks = seq(floor(min(y3)), ceiling(max(y3)), by=axis2by[i])
		axis(4, at=m*(tks-b)+mean(y)+offset[i], labels = tks, col=cols[3], col.axis=cols[3], las=1, cex.axis=axiscex) # 2nd y axis
		mtext(side=4, text=ylabs2[i], line=3, col=cols[3], cex=labcex) # label for 2nd axis
		mtext(text = labs[i], side=3, line=-1.5, adj=0.03, font=2) # subfigure labels
	}

	dev.off()

		# Just for AFSC_EBS
		quartz(width=4, height=3)
		# pdf(width=4, height=3, file=paste('Figures/regcentlatindiv vs. btmean_AFSC_EBS_', Sys.Date(), '.pdf', sep=''))
		par(mai=c(0.8,0.8,0.3,0.8), mgp=c(2.5,1,0))
		cols = c('black', 'blue')
		ycols = grep('X', names(regcentbiolatindiv))
		tcols = grep('X', names(btmean))
		i = 2 # for EBS
		x = as.numeric(gsub('X', '', names(regcentbiolatindiv)[ycols]))
		y = regcentbiolatindiv[i,ycols]
		x = x[!is.na(y)]
		y = y[!is.na(y)]
			mod = lm(y ~ x)
			x1 = x[x<=2006]; y1 = y[x<=2006]; mod1 = lm(y1 ~ x1)
		x3 = as.numeric(gsub('X', '', names(btmean)[tcols]))
		y3 = btmean[i,tcols]
		x3 = x3[!is.na(y3)]
		y3 = y3[!is.na(y3)]
			mod3 = lm(y3 ~ x3)
			x4 = x3[x3<=2006]; y4 = y3[x3<=2006]; mod4 = lm(y4 ~ x4)
		b = mean(y3)+2
		m = sd(y)/sd(y3)
		plot(x, y, type='l', main='', xlab='Year', ylab='Change in latitude (°)', col=cols[1], ylim=c(-0.5, 0.25), lwd=2)
			lines(x1, predict(mod1), lwd=1, col=cols[1], lty=3)
			lines(x, predict(mod), lwd=1, col=cols[1], lty=2)
		lines(x3, m*(y3-b), col=cols[2], lwd=2)
			lines(x4, m*(predict(mod4)-b), lwd=1, col=cols[2], lty=3)
			lines(x3, m*(predict(mod3)-b), lwd=1, col=cols[2], lty=2)
		tks = seq(0, 4, by=1)
		axis(4, at=m*(tks-b), labels = tks, col=cols[2], col.axis=cols[2])
		mtext(side=4, text='Temperature (°C)', line=2.5, col=cols[2])
	
		dev.off()
	
		summary(lm(y ~ y3))
		ccf(y,y3) # negative correlation at lag 3
		summary(lm(diff(y) ~ diff(y3))) # differences are correlated
		plot(diff(y), diff(y3))
	
	# Depth and sstemp (Fig. S4 for paper)
	cols = c('black', 'red', 'blue')
	inds = match(c('AFSC_EBS', 'AFSC_Aleutians', 'AFSC_GOA', 'WestCoast_Tri', 'SEFSC_GOMex', 'NEFSC_Spring', 'DFO_ScotianShelf', 'DFO_SoGulf'), regcentbiolatindiv$region)
	ylabs = c('Depth offset (m)', '', '', 'Depth offset (m)', '', '', 'Depth offset (m)', '', '')
	ylabs2 = c('', '', 'Temperature (°C)', '', '', 'Temperature (°C)', '', '', 'Temperature (°C)')
	xlabs = c('', '', '', '', '', '', 'Year', 'Year', 'Year')
	labs = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')
	offset = c(15, -15, 15, 25, 15, -22, 17, -20) # how much to elevate temp above depth offset in graph
	labcex = 1 # for axis labels
	axiscex = 1.5 # for numbers on the axes
	ylims = c(-35,27)
	axis2by = c(4, 1, 2, 1, 2, 1, 2, 1) # interval for second y-axis

	quartz(width=10, height=7)
	# pdf(width=10, height=7, file=paste('Figures/regcentdepthindiv vs. sstmean_', Sys.Date(), '.pdf', sep=''))
	par(mfrow=c(3,3), mai=c(0.3, 0.3, 0.1, 0.4), mgp = c(2.3, 0.9,0), omi = c(0.5, 0.4, 0, 0.3))
	ycols = grep('X', names(regcentbiodepthindiv))
	tcols = grep('X', names(sstmean))
	for(i in 1:length(inds)){
		x = as.numeric(gsub('X', '', names(regcentbiodepthindiv)[ycols]))
		y = regcentbiodepthindiv[inds[i],ycols]
		x = x[!is.na(y)]
		y = y[!is.na(y)]
		x3 = as.numeric(gsub('X', '', names(sstmean)[tcols]))
		y3 = sstmean[inds[i],tcols]
		x3 = x3[!is.na(y3)]
		y3 = y3[!is.na(y3)]
		y3 = y3
		b = mean(y3)
		m = sd(y)/sd(y3)
		y3p = m*(y3-b) + mean(y) + offset[i]
		#ylims = range(c(y, y3p)) + c(0, 2)
		plot(x, y, type='l', ylim=ylims, main='', xlab='', ylab='', col=cols[1], lwd=2, las=1, cex.axis = axiscex)
			mtext(side=2, text=ylabs[i], line=3.5, col=cols[1], cex=labcex)
			mtext(side=1, text=xlabs[i], line=2.4, cex=labcex)
		lines(x3, y3p, col=cols[3], lwd=2)
	
		# trend lines
		mod = lm(y ~ x)
		lines(x, predict(mod), lwd=1, col=cols[1], lty=2)	
		mod = lm(y3p ~ x3)
		lines(x3, predict(mod), lwd=1, col=cols[3], lty=2)	

		tks = seq(floor(min(y3))-axis2by[i], ceiling(max(y3))+axis2by[i], by=axis2by[i])
		axis(4, at=m*(tks-b)+mean(y)+offset[i], labels = tks, col=cols[3], col.axis=cols[3], las=1, cex.axis=axiscex)
		mtext(side=4, text=ylabs2[i], line=3.1, col=cols[3], cex=labcex)
		mtext(text = labs[i], side=3, line=-1.5, adj=0.03, col='black', font=2)
	}

	dev.off()	
	
##########################################################################
## Compare community shift to temperature change at multiple timescales ##
##########################################################################

## trial
t = 1:200
x = sin(t/10) + sin(t/4) + rnorm(length(t), 0, 0.2)
y = sin(t/10) + sin(t/4) + rnorm(length(t), 0, 0.2)

w= wc(x, y, plot=FALSE, start=1, dt=1)

quartz(width=8,height=5)
layout(matrix(c(1,1,2,3), nrow=2, byrow=TRUE), widths=c(6,2))
opar = par(mai=c(0.5, 0.5, 0.1, 0.1))
plot(t,x, 'l', col='black'); lines(t,y, col='blue')
wc.image(w)
	axis(1, at=w$axis.1[seq(1,length(w$axis.1), length.out=6)])
	at = w$axis.2[seq(1,length(w$axis.2), length.out=6)]; axis(2, at=at, labels=2^at)
m = par('mai'); m[c(2,4)] = 0.1
par(mai=m)
wc.power(w)
par(opar)


## analysis
regcentbiolatindiv = read.csv('Output/regcentbiolatindiv_2012-07-27.csv', row.names=1)




i = centbiolatindiv$regspp == 'NEFSC_Spring_Gadus morhua'
cs = intersect(which(!is.na(centbiolatindiv[i,])), grep('X', names(centbiolatindiv)))
j = btmean$region=='NEFSC_Spring'
bs = intersect(which(!is.na(btmean[j,])), grep('X', names(btmean)))
w= wc(x <- as.numeric(centbiolatindiv[i,cs]), y <- as.numeric(btmean[j, bs]), plot=FALSE, start=1968, dt=1)

summary(w)

quartz(width=8,height=5)
layout(matrix(c(1,2), nrow=1), widths=c(5,3))
wc.image(w)
	axis(1, at=w$axis.1[seq(1,length(w$axis.1), length.out=6)])
	axis(2, at=w$axis.2[seq(1,length(w$axis.2), length.out=6)])
m = par('mai'); m[c(2,4)] = 0.1
opar = par(mai=m)
wc.power(w)
par(opar)