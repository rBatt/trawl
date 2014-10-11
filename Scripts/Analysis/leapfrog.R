# RDB 10-Oct-2014
# Leapfrog!

# Where do communities go over time? 

#The location of the community next year is the stratum whose community is most similar to this stratum's community this year. Just keep finding the nearest one!
# Can be done iteratively (leapfrogging, allows for evolution of community), or in reference to baseline
# Permits tracking of step sizes of a community over time
# Permits tracking of change in a community over time (does most similar become more different?)
# Can calculate community specific climate velocity, maybe (how far did the community go [step size] in 1 year, or over x years?)

# ===================
# = Thoughts/ ideas =
# ===================
# Maybe I should look at shortest (community) distance from the first 10 years. So you have a stratum for the first ten years. Then you have all the strata in year 11 to which you compare the focal stratum. What is the distance of all year 11 strata to the focal stratum in year 1, year 2, year 3, ...? So if you have N strata, you now have N*10 distances. For each of the N strata, take the average of those 10 distances, and now you have N mean distances from the first decade of the focal strata. Whichever stratum in year 11 is on average closest to the first decade of the focal stratum is the winner. That is where the community went.

# Or you could maybe do a moving average on each species. So what's the average biomasses of each species in a stratum for the first 10 years? OK, that's the basline community. Problem is that if biomasses oscilate between high and low, and there are strata that have the exact same species that are either high or low in year 11, then there's a stratum at medium in year 11, the medium stratum would win even though the high or low strata would be more similar. So probably bad idea to average biomasses first.

# I could look at all the distances from the dat.t community, and weight their lat-lons by the inverse of the squared distance.
# or plot the distances in lat-lon space, where color is distance.


# ==================
# = Load Libraries =
# ==================
# library(maps)
library(data.table)
library(vegan)
library(reshape2)
library(PBSmapping)


# =========================
# = Load Data and Scripts =
# =========================
# Load trawl data
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/divData.RData")

# Load Legendre beta diversity functions
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions/beta.div.R")

# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))
lu <- function(x) length(unique(x))

# convert lon-lat to km
ll2km <- function(x,y){
	# x should be longitude
	# y should be latitude
	blah <- data.frame(X=x, Y=y) 
	attr(blah, "projection")="LL"
	blah2 <- convUL(blah)
	list(lat.km=blah2[,"Y"], lon.km=blah2[,"X"]) # returning a named list format is handy for use with data.table :=
}


# Distances between communities x & y
dist2 <- function(x,y, ...){
	
	nullDim.x <- is.null(dim(x))
	nullDim.y <- is.null(dim(y))
	stopifnot(nullDim.x==nullDim.y)
	nullDim <- nullDim.y & nullDim.x

	# Distance from each sampling unit in community x to each sampling unit in community y
	
	# x and y *must* have the same number of columns, and must be in same order (or have column names)
	
	# Assumes that x and y have same number of sampling units (although, see below)
	# That number of sampling units is n.str
	# if you had n.str.x and n.str.y,
	# change: [1:n.str,(n.str+1):(n.str*2)]
	# to: [1:n.str.x, (n.str.x+1):(n.str.x + n.str.y)]
	
	# add following lines to be general use, I think
	n.str.x <- ifelse(!nullDim.x, nrow(x), length(x))
	n.str.y <- ifelse(!nullDim.y, nrow(y), length(y))
	# then make the aforementioned change to [] subsetting
	
	# as.matrix(beta.div(rbind(x, y), nperm=0, save.D=TRUE)$D)[1:n.str,(n.str+1):(n.str*2)]
	if(nullDim){
		as.matrix(beta.div(c(x, y), nperm=0, save.D=TRUE, ...)$D)[1:n.str.x, (n.str.x+1):(n.str.x + n.str.y)] # untested		
	}else{
		as.matrix(beta.div(rbind(x, y), nperm=0, save.D=TRUE, ...)$D)[1:n.str.x, (n.str.x+1):(n.str.x + n.str.y)]
	}
}


# ========================
# = Fill in 0's and NA's =
# ========================
# For every species ever observed anywhere in a region, each year and each stratum must have an observation for that species
allFrog <- divData[,CJ(spp=unique(spp), year=unique(year), stratum=unique(stratum)), by=c("s.reg")]
setkey(allFrog)
frogData0 <- merge(allFrog, divData, all=TRUE)
frogData0[is.na(wtcpue), wtcpue:=0]
frogData0[, c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg","stratum")]
frogData0[, c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]
frogData0[,c("lat.km", "lon.km"):=ll2km(lon,lat)]
frogData <- frogData0

test <- frogData[s.reg=="neus",]
i=1


# ===================
# = Inside the frog =
# ===================
shifts <- frogData[,
	j={
		
		
		# =======================
		# = Strat and Year info =
		# =======================
		n.yrs <- .SD[,lu(year)] # number of years
		# n.yrs <- test[,lu(year)]
		
		years <- .SD[,as.integer(unique(year))] # unique years
		# years <- test[,as.integer(unique(year))]
		
		n.str <- .SD[,lu(stratum)] # number of strata
		# n.str <- test[,lu(stratum)]
		
		strata <- .SD[,unique(stratum)] # unique strata
		# strata <- test[,unique(stratum)]
		
		
		# =====================================
		# = Initials and record Origin Values =
		# =====================================
		# Miscillaneous info, not locations
		
		# origin dat.0
		dat.0 <- .SD[year==years[1],] # data for the initial year
		# dat.0 <- test[year==years[1],]
		
		# Set dat.0 location values to stratum-year-s.reg means, like for Origin Info
		locName <- c("lat","lon","lat.km","lon.km","depth")
		dat.0[,(locName):=list(lat=mean(lat), lon=mean(lon), lat.km=mean(lat.km), lon.km=mean(lon.km), depth=mean(depth)), by=c("year","stratum")]
		
		# Initialize current year data
		dat.t <- dat.0 # initialize the current year
		
		# Set up origin community matrix/ temperature vector
		castExp.0 <- acast(melt(dat.0, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
		setkey(dat.0, stratum)
		stemp.castExp.0 <- unique(dat.0)[,stemp]
		btemp.castExp.0 <- unique(dat.0)[,btemp]
		
		# ===============
		# = Origin Info =
		# ===============
		# Info about locations
		info.00 <- dat.0 # get lat lon info for unique strata (which are the initial locations for each trajectory)
		info.0 <- info.00[,list(lat=mean(lat), lon=mean(lon), lat.km=mean(lat.km), lon.km=mean(lon.km), depth=mean(depth)), by=c("year","stratum")]
		
		#Subset Info to unique strata
		setkey(info.0, stratum, year, lat, lon, depth)
		info.0 <- unique(info.0)
		
		# Community Info
		comInfo <- c("comStrat.t","com.lat.t","com.lon.t", "com.lat.km.t", "com.lon.km.t")
		info.0 <- info.0[,(comInfo):=list(stratum, lat, lon, lat.km, lon.km)] # add in info for community
		
		# # Stemp Info
		# stempInfo <- c("stempStrat.t","stemp.lat.t","stemp.lon.t", "stemp.lat.km.t", "stemp.lon.km.t")
		# info.0 <- info.0[,(stempInfo):=list(stratum, lat, lon, lat.km, lon.km)] # add in info for Stemp
		#
		# # Btemp Info
		# btempInfo <- c("btempStrat.t","btemp.lat.t","btemp.lon.t", "btemp.lat.km.t", "btemp.lon.km.t")
		# info.0 <- info.0[,(btempInfo):=list(stratum, lat, lon, lat.km, lon.km)] # add in info for temperature
		
		# Subset Info
		info.0 <- info.0[,list(s.reg, comStrat.t, com.lat.t, com.lon.t, com.lat.km.t, com.lon.km.t)] # subset
		
		# Info as data.frame
		info.0 <- as.data.frame(info.0) # turn to data.frame beacause I got frustrated with data.table
		
		# Store some other original condition information
		t.lat.0 <- info.0[, "com.lat.t"]
		t.lon.0 <- info.0[, "com.lon.t"]
		t.lat.km.0 <- info.0[, "com.lat.km.t"]
		t.lon.km.0 <- info.0[, "com.lon.km.t"]
		
		
		# =====================================================
		# = For() loop to iterate/ leapfrog through each year =
		# =====================================================
		# Initialize data at time t (oops, done above with dat.0). This will evolve and be updated
		# Initially, have a set of unique communities. This is the first dat.t.
		# Then you figure out which communities in the next year (dat.t1, communities for all strata)
		# best match the communities in dat.t.
		# Those communities in dat.t1 that are best matches become the new dat.t
		# This will probably lead to a convergence, and makes it impossible for divergence
		# One option would be to allow the community to split if there were several similar communities in the following year
		
		# Empty vectory to store accumulated distrances traveled by temperature or community trajectories
		com.dll.0.tot <- numeric(n.str)
		stemp.dll.0.tot <- numeric(n.str)
		btemp.dll.0.tot <- numeric(n.str)
		
		# Enter the rabbit hole
		for(i in 1:(n.yrs-1)){ # start at 1st year, end a year early (one step ahead prediction)
			t.yr <- years[i]
			dat.t1 <- .SD[year==years[i+1],] # in year t+1
			# dat.t1 <- test[year==years[i+1],] # in year t+1
			
			print(paste(s.reg,i))
			# =============================================
			# = Find distances between current and future =
			# =============================================
			# 1) Community Distances
			castExp.t <- acast(melt(dat.t, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
			castExp.t1 <- acast(melt(dat.t1, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
			
			# dComp.t <- as.matrix(beta.div(rbind(castExp.t, castExp.t1), nperm=0, save.D=TRUE)$D)[1:n.str,(n.str+1):(n.str*2)]
			dComp.t <- dist2(castExp.t, castExp.t1)
			comMatch0 <- apply(dComp.t, 1, which.min) # should probably weight the comp distances by geographic distance; if 2 communities are nearly identical, but one is really far away and the other close, should probably assume the close one is the same community.
			comMatch.arr <- matrix(c(1:length(comMatch0), as.integer(comMatch0)), ncol=2)
			
			
			# 2) Stemp distances
			setkey(dat.t1, stratum)
			stemp.castExp.t <- unique(dat.t)[,stemp]
			stemp.castExp.t1 <- unique(dat.t1)[,stemp]
			
			dStemp.t <- dist2(stemp.castExp.t, stemp.castExp.t1, method="euclidean")
			
			# Begin dealing with case where missing temperature
			if(i==1){
				# if in the first iteration, just consider the default movement to be to remain stationary
				last.stempMatch0 <- 1:n.str # length(diag(dStemp.t))
			}
			stempMatch0 <- apply(dStemp.t, 1, function(x){if(all(is.na(x))){0}else{which.min(x)}}) # calculate which.min, but if all na, replace index with a 0
			stempMatch0[stempMatch0==0L] <- last.stempMatch0[stempMatch0==0L] # replace all 0 with the index from last time (the temperature location just doesn't move)
			last.stempMatch0 <- stempMatch0 # then update the last set of temperature matches
			# End dealing with case where missign temperature
			
			stempMatch.arr <- matrix(c(1:length(stempMatch0), as.integer(stempMatch0)), ncol=2)
			
			
			# 3) Btemp distances
			setkey(dat.t1, stratum)
			btemp.castExp.t <- unique(dat.t)[,btemp]
			btemp.castExp.t1 <- unique(dat.t1)[,btemp]
			
			dBtemp.t <- dist2(btemp.castExp.t, btemp.castExp.t1, method="euclidean")
			
			# Begin dealing with case where missing temperature
			if(i==1){
				# if in the first iteration, just consider the default movement to be to remain stationary
				last.btempMatch0 <- 1:n.str # length(diag(dBtemp.t))
			}
			btempMatch0 <- apply(dBtemp.t, 1, function(x){if(all(is.na(x))){0}else{which.min(x)}}) # calculate which.min, but if all na, replace index with a 0
			btempMatch0[btempMatch0==0L] <- last.btempMatch0[btempMatch0==0L] # replace all 0 with the index from last time (the temperature location just doesn't move)
			last.btempMatch0 <- btempMatch0 # then update the last set of temperature matches
			# End dealing with case where missign temperature
			
			btempMatch.arr <- matrix(c(1:length(btempMatch0), as.integer(btempMatch0)), ncol=2)
			
			
			# ===================================
			# = Update community based on match =
			# ===================================
			setkey(dat.t1, stratum)
			dat.t.new <- dat.t1[strata[comMatch0]]
			dat.t.new[,comStrat.t:=stratum]
			dat.t.new[,stratum:=dat.t1[,stratum]]
			
			
			# =====================================
			# = Update temperature based on match =
			# =====================================
			# Stemp
			setkey(dat.t1, stratum)
			stemp.dat.t.new <- dat.t1[strata[stempMatch0]]
			stemp.dat.t.new[,stempStrat.t:=stratum]
			stemp.dat.t.new[,stratum:=dat.t1[,stratum]]
			
			# Btemp
			setkey(dat.t1, stratum)
			btemp.dat.t.new <- dat.t1[strata[btempMatch0]]
			btemp.dat.t.new[,btempStrat.t:=stratum]
			btemp.dat.t.new[,stratum:=dat.t1[,stratum]]

			
			# ============================================
			# = Calculate distances to updated community =
			# ============================================
			# 1) Geographical distances
			# Set up geographical locations
			setkey(dat.t.new, stratum) # set key so can use unique()
			setkey(dat.t, stratum) # set key so can use unique()
			com.ll.new <- as.matrix(unique(dat.t.new)[,list(lon.km, lat.km)]) # lat-lon for the updated location
			com.ll.t <- as.matrix(unique(dat.t)[,list(lon.km, lat.km)]) # lat-lon for the current location
			ll.0 <- matrix(c(t.lon.km.0, t.lat.km.0), ncol=2) # lat-lon for the original location (should move to outside for loop)

			# note: can get com.ll.t from t.com.lat.km.t and t.com.lon.km.t
			
			# Geographical distances – between time steps, and from origin to updated (net and total)
			com.dll.t <- diag(dist2(com.ll.new, com.ll.t, method="euclidean")) # geographical distance traveled between time steps
			com.dll.0.net <- diag(dist2(com.ll.new, ll.0, method="euclidean")) # net geographical distance from origin to updated
			com.dll.0.tot <- com.dll.0.tot + com.dll.t # total distance traveled so far (distance covered by trajectory)
			
			
			# 1.5) Geographical angles
			com.ll.new.del <- com.ll.new-ll.0 # new geographical location relative to origin
			com.ll.t.del <- com.ll.new-com.ll.t # new geographical position relative to last position
			com.dll.0.net.angle <- atan2(com.ll.new.del[,2], com.ll.new.del[,1])*180/pi # angle between new location and origin
			com.dll.t.angle <- atan2(com.ll.t.del[,2], com.ll.t.del[,1])*180/pi # angle between new location and last position
			
			
			# 2) Community distances
			# Set up community matrices
			castExp.t.new <- acast(melt(dat.t.new, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
			
			# Community distances – between time steps, from origin to updated
			t.dComp.0.net <- as.numeric(diag(dist2(castExp.t.new, castExp.0)))
			t.dComp.t <- dComp.t[comMatch.arr]
			
			
			# 3) Temperature distances
			# Set up temperature matrices (should be extremely simple b/c just scalar)
			
			# Temperature distances – between time steps, from origin to updated
			
			
			# =======================================
			# = Calculate distance to updated Stemp =
			# =======================================
			# 1) Geographical distances to new Stemp
			# Set up geographical locations
			setkey(stemp.dat.t.new, stratum) # set key so can use unique()
			setkey(dat.t, stratum) # set key so can use unique()
			stemp.ll.new <- as.matrix(unique(stemp.dat.t.new)[,list(lon.km, lat.km)]) # lat-lon for the updated location
			stemp.ll.t <- as.matrix(unique(dat.t)[,list(lon.km, lat.km)]) # lat-lon for the current location
			# ll.0 <- matrix(c(t.lon.km.0, t.lat.km.0), ncol=2) # lat-lon for the original location (should move to outside for loop)
			
			# Geographical distances to new Stemp – between time steps, and from origin to updated (net and total)
			stemp.dll.t <- diag(dist2(stemp.ll.new, stemp.ll.t, method="euclidean")) # geographical distance traveled between time steps
			stemp.dll.0.net <- diag(dist2(stemp.ll.new, ll.0, method="euclidean")) # net geographical distance from origin to updated
			stemp.dll.0.tot <- stemp.dll.0.tot + stemp.dll.t # total distance traveled so far (distance covered by trajectory)
		
			
			# 1.5) Geographical angles between current and new Stemp
			stemp.ll.new.del <- stemp.ll.new-ll.0 # new geographical location relative to origin
			stemp.ll.t.del <- stemp.ll.new-stemp.ll.t # new geographical position relative to last position
			stemp.dll.0.net.angle <- atan2(stemp.ll.new.del[,2], stemp.ll.new.del[,1])*180/pi # angle between new location and origin
			stemp.dll.t.angle <- atan2(stemp.ll.t.del[,2], stemp.ll.t.del[,1])*180/pi # angle between new location and last position
			
			
			# 2) Stemp distances
			# Set up Stemp matrices
			stemp.castExp.t.new <- unique(stemp.dat.t.new)[,stemp]
			
			# Stemp distances – between time steps, from origin to updated
			t.dStemp.0.net <- as.numeric(diag(dist2(stemp.castExp.t.new, stemp.castExp.0, method="euclidean")))
			t.dStemp.t <- dStemp.t[stempMatch.arr]
			
			
			# =======================================
			# = Calculate distance to updated Btemp =
			# =======================================
			# 1) Geographical distances to new Btemp
			# Set up geographical locations
			setkey(btemp.dat.t.new, stratum) # set key so can use unique()
			setkey(dat.t, stratum) # set key so can use unique()
			btemp.ll.new <- as.matrix(unique(btemp.dat.t.new)[,list(lon.km, lat.km)]) # lat-lon for the updated location
			btemp.ll.t <- as.matrix(unique(dat.t)[,list(lon.km, lat.km)]) # lat-lon for the current location
			# ll.0 <- matrix(c(t.lon.km.0, t.lat.km.0), ncol=2) # lat-lon for the original location (should move to outside for loop)
			
			# Geographical distances to new Btemp – between time steps, and from origin to updated (net and total)
			btemp.dll.t <- diag(dist2(btemp.ll.new, btemp.ll.t, method="euclidean")) # geographical distance traveled between time steps
			btemp.dll.0.net <- diag(dist2(btemp.ll.new, ll.0, method="euclidean")) # net geographical distance from origin to updated
			btemp.dll.0.tot <- btemp.dll.0.tot + btemp.dll.t # total distance traveled so far (distance covered by trajectory)
		
			
			# 1.5) Geographical angles between current and new Btemp
			btemp.ll.new.del <- btemp.ll.new-ll.0 # new geographical location relative to origin
			btemp.ll.t.del <- btemp.ll.new-btemp.ll.t # new geographical position relative to last position
			btemp.dll.0.net.angle <- atan2(btemp.ll.new.del[,2], btemp.ll.new.del[,1])*180/pi # angle between new location and origin
			btemp.dll.t.angle <- atan2(btemp.ll.t.del[,2], btemp.ll.t.del[,1])*180/pi # angle between new location and last position
			
			
			# 2) Btemp distances
			# Set up Btemp matrices
			btemp.castExp.t.new <- unique(btemp.dat.t.new)[,btemp]
			
			# Btemp distances – between time steps, from origin to updated
			t.dBtemp.0.net <- as.numeric(diag(dist2(btemp.castExp.t.new, btemp.castExp.0, method="euclidean")))
			t.dBtemp.t <- dBtemp.t[btempMatch.arr]
			
			
			# ============================
			# = Add location information =
			# ============================
			# Community
			t.com.lat.t <- info.0[comMatch0, "com.lat.t"]
			t.com.lon.t <- info.0[comMatch0, "com.lon.t"]
			t.com.lat.km.t <- info.0[comMatch0, "com.lat.km.t"]
			t.com.lon.km.t <- info.0[comMatch0, "com.lon.km.t"]
			
			# Stemp
			t.stemp.lat.t <- info.0[stempMatch0, "com.lat.t"]
			t.stemp.lon.t <- info.0[stempMatch0, "com.lon.t"]
			t.stemp.lat.km.t <- info.0[stempMatch0, "com.lat.km.t"]
			t.stemp.lon.km.t <- info.0[stempMatch0, "com.lon.km.t"]
			
			# Btemp
			t.btemp.lat.t <- info.0[btempMatch0, "com.lat.t"]
			t.btemp.lon.t <- info.0[btempMatch0, "com.lon.t"]
			t.btemp.lat.km.t <- info.0[btempMatch0, "com.lat.km.t"]
			t.btemp.lon.km.t <- info.0[btempMatch0, "com.lon.km.t"]
		   

			# n.stat <- sum((comMatch.arr[,1] - comMatch.arr[,2])==0) # number that didn't move this time
			# stemp.n.stat <- sum((stempMatch.arr[,1] - stempMatch.arr[,2])==0) # number that didn't move this time
			
			comMatch0 <- data.table(
				s.reg=s.reg,
				year=t.yr,
				stratum=strata, 
				strat.lat.0=t.lat.0,
				strat.lon.0=t.lon.0,
				
				comStrat.t=strata[comMatch0],
				com.lat.t=t.com.lat.t,
				com.lon.t=t.com.lon.t,
				
				stempStrat.t=strata[stempMatch0],
				stemp.lat.t=t.stemp.lat.t,
				stemp.lon.t=t.stemp.lon.t,
				btempStrat.t=strata[btempMatch0],
				btemp.lat.t=t.btemp.lat.t,
				btemp.lon.t=t.btemp.lon.t,
				
				com.dll.0.net=com.dll.0.net , # distance from origin
				com.dll.0.tot=com.dll.0.tot,
				com.dll.t=com.dll.t, # distance moved this time step
				stemp.dll.0.net=stemp.dll.0.net , # distance from origin
				stemp.dll.t=stemp.dll.t, # distance moved this time step
				btemp.dll.0.net=btemp.dll.0.net , # distance from origin
				btemp.dll.t=btemp.dll.t, # distance moved this time step
				
				dComp.net=t.dComp.0.net,
				dComp.t=t.dComp.t,
				dStemp.net=t.dStemp.0.net,
				dStemp.t=t.dStemp.t, 
				dBtemp.net=t.dBtemp.0.net,
				dBtemp.t=t.dBtemp.t, 
				
				com.dll.t.angle=stemp.dll.t.angle,
				com.dll.0.net.angle=stemp.dll.0.net.angle,
				stemp.dll.t.angle=stemp.dll.t.angle,
				stemp.dll.0.net.angle=stemp.dll.0.net.angle,
				btemp.dll.t.angle=btemp.dll.t.angle,
				btemp.dll.0.net.angle=btemp.dll.0.net.angle
				
				
			)
			
			# ===============================================
			# = Accumulate matches at end of each iteration =
			# ===============================================
			if(i!=1L){
				comMatch <- rbind(comMatch, comMatch0)
			}else{
				comMatch <- comMatch0
			}
			
			
			# ========================================
			# = Update dat.t to determine froggyness =
			# ========================================
			# Leapfrog Option
			# names.keep <- names(dat.t)
			# dat.t <- dat.t.new[,(names.keep), with=FALSE] # for leapfrog!
			
			# # Consecutive Pairs Options
			dat.t <- dat.t1
			
			# # Origin Only
			# dat.t <- dat.0
			
		}
		
		comMatch # return this data.table for each s.reg
		
	},
	
	by=c("s.reg")
]


# ==============================================
# = Begin plotting everything you can think of =
# ==============================================

# ==============
# = Trajectory =
# ==============
comMatch[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(comMatch, s.reg, stratum)
dev.new()
par(mfrow=c(1,1), mar=c(1.5,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
comMatch[,
	j={

		
		strat.cols1 <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(lu(stratum))
		strat.cols1.5 <- rgb(t(col2rgb(strat.cols1)), alpha=100, maxColorValue=255)
		strat.cols2 <- rgb(t(col2rgb(strat.cols1)), alpha=45, maxColorValue=255)
		strat.chars <- as.character(as.numeric(unique(stratum)))
		names(strat.cols1) <- unique(stratum)
		names(strat.cols2) <- unique(stratum)
		names(strat.chars) <- unique(stratum)
		
		# ==================================
		# = Plot 1: Community Trajectories =
		# ==================================
		
		plot(com.lon.t, com.lat.t, type="n")
		.SD[,
			j={
				lines(com.lon.t, com.lat.t, type="l", col=strat.cols2[stratum])
				
			},
			by=c("stratum")	
		]
		.SD[,
			points(jitter(rev(com.lon.t)[1], factor=0.15), jitter(rev(com.lat.t)[1], factor=0.15), col="black", bg=strat.cols1.5[stratum], pch=21, cex=2),
			by=c("stratum")
		]
		
		points(unique(strat.lon.0), unique(strat.lat.0), col=strat.cols1, bg=strat.cols2, cex=3, pch=21)
		text(unique(strat.lon.0), unique(strat.lat.0), col="black", cex=1, labels=strat.chars, font=1)
		
		
	},
	
	by=c("s.reg")
]





# ===============================
# = Community Lat vs. Stemp Lat =
# ===============================
shifts[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(shifts, s.reg, stratum)
# dev.new(width=4, height=6)
png("~/Desktop/communityShift_vs_surfTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.5,0.75,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
shifts[,
	j={

		
		strat.cols1 <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(lu(stratum))
		strat.cols1.5 <- rgb(t(col2rgb(strat.cols1)), alpha=100, maxColorValue=255)
		strat.cols2 <- rgb(t(col2rgb(strat.cols1)), alpha=45, maxColorValue=255)
		gray2 <- rgb(t(col2rgb("gray")), alpha=85, maxColorValue=255)
		strat.chars <- as.character(as.numeric(unique(stratum)))
		names(strat.cols1) <- unique(stratum)
		names(strat.cols2) <- unique(stratum)
		names(strat.chars) <- unique(stratum)
		
		com.lat.shift <- com.lat.t-strat.lat.0
		stemp.lat.shift <- stemp.lat.t-strat.lat.0
		
		plot(com.lat.shift, stemp.lat.shift, ylab=bquote(Com~(degree*N/yr)), xlab=bquote(Stemp~(degree*N/yr)), pch=21, bg=gray2, col=NA, main=s.reg)
		
		pval <- tryCatch(summary(lm(com.lat.shift~stemp.lat.shift))$coef[2,4], error=function(cond)2)
		
		
		
		if(pval<0.05){
			abline(a=0, b=1, col="red", lwd=3)
		}else{
			abline(a=0, b=1, col="red")
		}
		if(pval<0.005){
			abline(a=0, b=1, col="red", lwd=3)
			abline(a=0, b=1, col="white", lty="dashed", lwd=1.5)
		}
		
		
		
		
		
		# print(summary(lm(com.lat.shift~stemp.lat.shift)))
		# print(summary(lm(com.lat.shift~stemp.lat.shift*strat.lat.0)))
	
	},
	
	by=c("s.reg")
]
dev.off()




# ===============================
# = Community Lat vs. Btemp Lat =
# ===============================
shifts[,stratum:=factor(stratum, levels=unique(stratum)[order(strat.lat.0[!duplicated(stratum)])])]
setkey(shifts, s.reg, stratum)
# dev.new(width=4, height=6)
png("~/Desktop/communityShift_vs_botTempShift.png", width=4, height=6, res=300, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.5,0.75,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
shifts[,
	j={

		
		strat.cols1 <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(lu(stratum))
		strat.cols1.5 <- rgb(t(col2rgb(strat.cols1)), alpha=100, maxColorValue=255)
		strat.cols2 <- rgb(t(col2rgb(strat.cols1)), alpha=45, maxColorValue=255)
		gray2 <- rgb(t(col2rgb("gray")), alpha=85, maxColorValue=255)
		strat.chars <- as.character(as.numeric(unique(stratum)))
		names(strat.cols1) <- unique(stratum)
		names(strat.cols2) <- unique(stratum)
		names(strat.chars) <- unique(stratum)
		
		com.lat.shift <- com.lat.t-strat.lat.0
		btemp.lat.shift <- btemp.lat.t-strat.lat.0
		
		plot(com.lat.shift, btemp.lat.shift, ylab=bquote(Com~(degree*N/yr)), xlab=bquote(Btemp~(degree*N/yr)), pch=21, bg=gray2, col=NA, main=s.reg)
		
		pval <- summary(lm(com.lat.shift~btemp.lat.shift))$coef[2,4]
		
		
		
		if(pval<0.05){
			abline(a=0, b=1, col="red", lwd=3)
		}else{
			abline(a=0, b=1, col="red")
		}
		if(pval<0.005){
			abline(a=0, b=1, col="red", lwd=3)
			abline(a=0, b=1, col="white", lty="dashed", lwd=1.5)
		}
		
		
		
		
		
		print(summary(lm(com.lat.shift~btemp.lat.shift)))
		print(summary(lm(com.lat.shift~btemp.lat.shift*strat.lat.0)))
	
	},
	
	by=c("s.reg")
]
dev.off()



