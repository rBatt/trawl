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

# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))

# Load plotting functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))

# Load statistics functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))

s.reg.key <- c(
	"ai"="Aleutian Islands", 
	"ebs"="Eastern Bering Strait", 
	"gmex"="Gulf of Mexico", 
	"goa"="Gulf of Alaska",
	"neus"="Northeast US",
	"newf"="Newfoundland",
	"ngulf"="Northern Gulf of St. Lawrence",
	"sa"="South Atlantic US",
	"sgulf"="S. Gulf of St. Lawrence",
	"shelf"="Scotian Shelf",
	"wcann"="West Coast US (annual)",
	"wctri"="West Coast US (triennial)",
	"wc"= "West Coast US" #(combined annual & triennial)"
)


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


# test <- frogData[s.reg=="neus",]
# i=1


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
		alphaD.t0 <- diversity(castExp.0) # Inital alpha diversity!
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
		
		# Community Info (because these are initial conditions, same for stemp and btemp)
		comInfo <- c("comStrat.t","com.lat.t","com.lon.t", "com.lat.km.t", "com.lon.km.t")
		info.0 <- info.0[,(comInfo):=list(stratum, lat, lon, lat.km, lon.km)] # add in info for community
		
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
		
		comArrive.net <- numeric(n.str)
		stempArrive.net <- numeric(n.str)
		btempArrive.net <- numeric(n.str)
		
		# Enter the rabbit hole
		for(i in 1:(n.yrs-1)){ # start at 1st year, end a year early (one step ahead prediction)
			t.yr <- years[i]
			dat.t1 <- .SD[year==years[i+1],] # in year t+1
			# dat.t1 <- test[year==years[i+1],] # in year t+1
			
			print(paste(s.reg,i))
			# print(round(sum(is.na(btemp))/length(btemp),2))
			# =============================================
			# = Find distances between current and future =
			# =============================================
			# 1) Community Distances
			castExp.t <- acast(melt(dat.t, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
			castExp.t1 <- acast(melt(dat.t1, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
			
			dComp.t1.t <- dist2(castExp.t, castExp.t1)
			comMatch0 <- apply(dComp.t1.t, 1, which.min)
			comMatch.arr <- matrix(c(1:length(comMatch0), as.integer(comMatch0)), ncol=2)
			
			dComp.t.t0.matched0 <- dist2(castExp.0, castExp.t) # matching initial community to closest current analog
			comMatch.t.t0.0 <- apply(dComp.t.t0.matched0, 1, which.min)
			comMatch.t.t0.arr <- matrix(c(1:length(comMatch.t.t0.0), as.integer(comMatch.t.t0.0)), ncol=2)
			
			
			# 2) Stemp distances
			setkey(dat.t1, stratum)
			stemp.castExp.t <- unique(dat.t)[,stemp]
			stemp.castExp.t1 <- unique(dat.t1)[,stemp]
			
			dStemp.t1.t <- dist2(stemp.castExp.t, stemp.castExp.t1, method="euclidean")
			
			# Begin dealing with case where missing temperature
			if(i==1){
				# if in the first iteration, just consider the default movement to be to remain stationary
				last.stempMatch0 <- 1:n.str # length(diag(dStemp.t1.t))
			}
			stempMatch00 <- apply(dStemp.t1.t, 1, function(x){if(all(is.na(x))){0}else{which.min(x)}}) # calculate which.min, but if all na, replace index with a 0
			stempMatch0 <- stempMatch00
			stempMatch0[stempMatch00==0L] <- last.stempMatch0[stempMatch00==0L] # replace all 0 with the index from last time (the temperature location just doesn't move)
			last.stempMatch0 <- stempMatch0 # then update the last set of temperature matches
			# End dealing with case where missign temperature
			
			stempMatch.arr <- matrix(c(1:length(stempMatch0), as.integer(stempMatch0)), ncol=2)
			
			
			# 3) Btemp distances
			setkey(dat.t1, stratum)
			btemp.castExp.t <- unique(dat.t)[,btemp]
			btemp.castExp.t1 <- unique(dat.t1)[,btemp]
			
			dBtemp.t1.t <- dist2(btemp.castExp.t, btemp.castExp.t1, method="euclidean")
			
			# Begin dealing with case where missing temperature
			if(i==1){
				# if in the first iteration, just consider the default movement to be to remain stationary
				last.btempMatch0 <- 1:n.str # length(diag(dBtemp.t1.t))
			}
			btempMatch00 <- apply(dBtemp.t1.t, 1, function(x){if(all(is.na(x))){0}else{which.min(x)}}) # calculate which.min, but if all na, replace index with a 0
			btempMatch0 <- btempMatch00
			btempMatch0[btempMatch00==0L] <- last.btempMatch0[btempMatch00==0L] # replace all 0 with the index from last time (the temperature location just doesn't move)
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
			
			
			# ===================================================
			# = Update community for closest analog to original =
			# ===================================================
			dat.t0.match <- dat.t[strata[comMatch.t.t0.0]]
			dat.t0.match[,comStrat.t0.match:=stratum]
			dat.t0.match[,stratum:=dat.t[,stratum]]
			
			
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
			com.ll.new <- as.matrix(unique(dat.t.new)[,list(lon, lat)]) #as.matrix(unique(dat.t.new)[,list(lon.km, lat.km)]) # lat-lon for the updated location
			com.ll.t <- as.matrix(unique(dat.t)[,list(lon, lat)]) #as.matrix(unique(dat.t)[,list(lon.km, lat.km)]) # lat-lon for the current location
			ll.0 <- matrix(c(t.lon.0, t.lat.0), ncol=2) #matrix(c(t.lon.km.0, t.lat.km.0), ncol=2) # lat-lon for the original location (should move to outside for loop)
			
			# Geographical locations for current-original match
			setkey(dat.t0.match)
			com.ll.t0.match <- as.matrix(unique(dat.t0.match)[,list(lon, lat)])
			
			# Geographical distances – between time steps, and from origin to updated (net and total)
			com.dll.t1.t <- diag(dist2(com.ll.new, com.ll.t, method="euclidean")) # geographical distance traveled between time steps
			com.dll.t1.t0 <- diag(dist2(com.ll.new, ll.0, method="euclidean")) # net geographical distance from origin to updated
			com.dll.0.tot <- com.dll.0.tot + com.dll.t1.t # total distance traveled so far (distance covered by trajectory)
			
			# Geographical community distance for current-origin match
			# skipping for now b/c I just want the shift mostly
			
			
			
			# 1.5) Geographical angles
			com.ll.new.del <- com.ll.new-ll.0 # new geographical location relative to origin
			com.dll.t1.t <- com.ll.new-com.ll.t # new geographical position relative to last position
			com.dll.angle.t1.t0 <- atan2(com.ll.new.del[,2], com.ll.new.del[,1])*180/pi # angle between new location and origin
			com.dll.angle.t1.t <- atan2(com.dll.t1.t[,2], com.dll.t1.t[,1])*180/pi # angle between new location and last position
			
			
			# 2) Community distances
			# Set up community matrices
			castExp.t.new <- acast(melt(dat.t.new, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
			
			# Community distances – between time steps, from origin to updated, from origin to current, and from origin to current match
			t.dComp.t.t0 <- as.numeric(diag(dist2(castExp.t, castExp.0))) # using diag() so that comparing the same stratum to itself (between t and t0). Note that this isn't the distance between initial communities and best current analog – it's comparing the stationary stratum community
			t.dComp.t1.t0 <- as.numeric(diag(dist2(castExp.t.new, castExp.0)))
			t.dComp.t1.t <- dComp.t1.t[comMatch.arr]
			t.dComp.t.t0.matched <- dComp.t.t0.matched0[comMatch.t.t0.arr]
			
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
			stemp.ll.new <- as.matrix(unique(stemp.dat.t.new)[,list(lon, lat)]) #as.matrix(unique(stemp.dat.t.new)[,list(lon.km, lat.km)]) # lat-lon for the updated location
			stemp.ll.t <- as.matrix(unique(dat.t)[,list(lon, lat)]) #as.matrix(unique(dat.t)[,list(lon.km, lat.km)]) # lat-lon for the current location
			
			# Geographical distances to new Stemp – between time steps, and from origin to updated (net and total)
			# TODO Fix all of these geographical distances, because they are wrong!!! (I think)
			stemp.dll.t1.t <- diag(dist2(stemp.ll.new, stemp.ll.t, method="euclidean")) # geographical distance traveled between time steps
			stemp.dll.t1.t0 <- diag(dist2(stemp.ll.new, ll.0, method="euclidean")) # net geographical distance from origin to updated
			stemp.dll.0.tot <- stemp.dll.0.tot + stemp.dll.t1.t # total distance traveled so far (distance covered by trajectory)
		
			
			# 1.5) Geographical angles between current and new Stemp
			stemp.ll.new.del <- stemp.ll.new-ll.0 # new geographical location relative to origin
			stemp.dll.t1.t <- stemp.ll.new-stemp.ll.t # new geographical position relative to last position
			stemp.dll.angle.t1.t0 <- atan2(stemp.ll.new.del[,2], stemp.ll.new.del[,1])*180/pi # angle between new location and origin
			stemp.dll.angle.t1.t <- atan2(stemp.dll.t1.t[,2], stemp.dll.t1.t[,1])*180/pi # angle between new location and last position
			
			
			# 2) Stemp distances
			# Set up Stemp matrices
			stemp.castExp.t.new <- unique(stemp.dat.t.new)[,stemp]
			
			# Stemp distances – between time steps, from origin to updated, and from current to origin
			t.dStemp.t.t0 <- as.numeric(diag(dist2(stemp.castExp.t, stemp.castExp.0, method="euclidean")))
			t.dStemp.t1.t0 <- as.numeric(diag(dist2(stemp.castExp.t.new, stemp.castExp.0, method="euclidean")))
			t.dStemp.t1.t <- dStemp.t1.t[stempMatch.arr]
			
			
			# =======================================
			# = Calculate distance to updated Btemp =
			# =======================================
			# 1) Geographical distances to new Btemp
			# Set up geographical locations
			setkey(btemp.dat.t.new, stratum) # set key so can use unique()
			setkey(dat.t, stratum) # set key so can use unique()
			btemp.ll.new <- as.matrix(unique(btemp.dat.t.new)[,list(lon, lat)]) #as.matrix(unique(btemp.dat.t.new)[,list(lon.km, lat.km)]) # lat-lon for the updated location
			btemp.ll.t <- as.matrix(unique(dat.t)[,list(lon, lat)]) #as.matrix(unique(dat.t)[,list(lon.km, lat.km)]) # lat-lon for the current location
			
			# Geographical distances to new Btemp – between time steps, and from origin to updated (net and total)
			btemp.dll.t1.t <- diag(dist2(btemp.ll.new, btemp.ll.t, method="euclidean")) # geographical distance traveled between time steps
			btemp.dll.t1.t0 <- diag(dist2(btemp.ll.new, ll.0, method="euclidean")) # net geographical distance from origin to updated
			btemp.dll.0.tot <- btemp.dll.0.tot + btemp.dll.t1.t # total distance traveled so far (distance covered by trajectory)
		
			
			# 1.5) Geographical angles between current and new Btemp
			btemp.ll.new.del <- btemp.ll.new-ll.0 # new geographical location relative to origin
			btemp.dll.t1.t <- btemp.ll.new-btemp.ll.t # new geographical position relative to last position
			btemp.dll.angle.t1.t0 <- atan2(btemp.ll.new.del[,2], btemp.ll.new.del[,1])*180/pi # angle between new location and origin
			btemp.dll.angle.t1.t <- atan2(btemp.dll.t1.t[,2], btemp.dll.t1.t[,1])*180/pi # angle between new location and last position
			
			
			# 2) Btemp distances
			# Set up Btemp matrices
			btemp.castExp.t.new <- unique(btemp.dat.t.new)[,btemp]
			
			# Btemp distances – between time steps, from origin to updated, and from current to origin
			t.dBtemp.t.t0 <- as.numeric(diag(dist2(btemp.castExp.t, btemp.castExp.0, method="euclidean")))
			t.dBtemp.t1.t0 <- as.numeric(diag(dist2(btemp.castExp.t.new, btemp.castExp.0, method="euclidean")))
			t.dBtemp.t1.t <- dBtemp.t1.t[btempMatch.arr]
			
			
			# ============================
			# = Add location information =
			# ============================
			# Community
			t.com.lat.t1 <- info.0[comMatch0, "com.lat.t"]
			t.com.lon.t1 <- info.0[comMatch0, "com.lon.t"]
			t.com.lat.km.t1 <- info.0[comMatch0, "com.lat.km.t"]
			t.com.lon.km.t1 <- info.0[comMatch0, "com.lon.km.t"]
			
			# Community that is current best match to initial
			t.com.lat.t0.match <- info.0[comMatch.t.t0.0, "com.lat.t"]
			t.com.lon.t0.match <- info.0[comMatch.t.t0.0, "com.lon.t"]
			t.com.lat.km.t0.match <- info.0[comMatch.t.t0.0, "com.lat.km.t"]
			t.com.lon.km.t0.match <- info.0[comMatch.t.t0.0, "com.lon.km.t"]
			
			# Stemp
			t.stemp.lat.t1 <- info.0[stempMatch0, "com.lat.t"]
			t.stemp.lon.t1 <- info.0[stempMatch0, "com.lon.t"]
			t.stemp.lat.km.t1 <- info.0[stempMatch0, "com.lat.km.t"]
			t.stemp.lon.km.t1 <- info.0[stempMatch0, "com.lon.km.t"]
			
			# Btemp
			t.btemp.lat.t1 <- info.0[btempMatch0, "com.lat.t"]
			t.btemp.lon.t1 <- info.0[btempMatch0, "com.lon.t"]
			t.btemp.lat.km.t1 <- info.0[btempMatch0, "com.lat.km.t"]
			t.btemp.lon.km.t1 <- info.0[btempMatch0, "com.lon.km.t"]
			
			
			# ==========================
			# = Clean up temps w/ NA's =
			# ==========================
			bs.stemp <- as.integer(stempMatch00)==0L
			st2na <- seq_along(stempMatch00)
			st2na[bs.stemp] <- NA # the trick here is that if you have (1:3)[c(1,NA,3)], you get 1, NA, 3 back.
			
			bs.btemp <- as.integer(btempMatch00)==0L
			bt2na <- seq_along(btempMatch00)
			bt2na[bs.btemp] <- NA
			
			
			# ===================================================
			# = Compute com/stemp/btemp arrivals and departures =
			# ===================================================
			# Community arrivals
			comArrive0 <- table(strata[comMatch0])
			comArrive.tot <- sum(comArrive0, na.rm=TRUE)
			comArrive.t <- numeric(n.str)
			comArrive.t[strata%in%names(comArrive0)] <- as.numeric(comArrive0) #/comArrive.tot
			comArrive.net <- comArrive.net + comArrive.t + -1 #/comArrive.tot
			
			# Stemp arrivals
			stempArrive0 <- table(strata[stempMatch0])
			stempArrive.tot <- sum(stempArrive0, na.rm=TRUE)
			stempArrive.t <- numeric(n.str)
			if(sum(!bs.btemp)>0){
				stempArrive.t[strata%in%names(stempArrive0)] <- as.numeric(stempArrive0) #/stempArrive.tot
			}
			stempArrive.net <- stempArrive.net + stempArrive.t + -1 #/stempArrive.tot
			
			# Btemp arrivals
			btempArrive0 <- table(strata[btempMatch0])
			btempArrive.tot <- sum(btempArrive0, na.rm=TRUE)
			btempArrive.t <- numeric(n.str)
			if(sum(!bs.btemp)>0){
				btempArrive.t[strata%in%names(btempArrive0)] <- as.numeric(btempArrive0) #/btempArrive.tot
			}
			btempArrive.net <- btempArrive.net + btempArrive.t + -1 #/btempArrive.tot
			
			
			# ==========================
			# = Attemp at flow through =
			# ==========================
			# Stemp flow through
			stemp.lon.mat0 <- matrix(c(t.lon.0, t.stemp.lon.t1),ncol=2)
			stemp.lon.mat <- stemp.lon.mat0
			stemp.lon.mat.logic <- stemp.lon.mat0[,1] > stemp.lon.mat0[,2]
			stemp.lon.mat[stemp.lon.mat.logic,] <- stemp.lon.mat0[stemp.lon.mat.logic, 2:1]
			
			stemp.lat.mat0 <- matrix(c(t.lat.0, t.stemp.lat.t1),ncol=2)
			stemp.lat.mat <- stemp.lat.mat0
			stemp.lat.mat.logic <- stemp.lat.mat0[,1] > stemp.lat.mat0[,2]
			stemp.lat.mat[stemp.lat.mat.logic,] <- stemp.lat.mat0[stemp.lat.mat.logic, 2:1]
			
			stemp.ft <- numeric(length(t.lon.0))
			for(j in 1:length(t.lon.0)){
				t.stemp.lon.ft <- t.lon.0[j]>stemp.lon.mat[,1] & t.lon.0[j]<stemp.lon.mat[,2]
				t.stemp.lat.ft <- t.lat.0[j]>stemp.lat.mat[,1] & t.lat.0[j]<stemp.lat.mat[,2]
				stemp.ft[j] <- sum(t.stemp.lon.ft & t.stemp.lat.ft, na.rm=TRUE)
			}
			
			# Btemp flow through
			btemp.lon.mat0 <- matrix(c(t.lon.0, t.btemp.lon.t1),ncol=2)
			btemp.lon.mat <- btemp.lon.mat0
			btemp.lon.mat.logic <- btemp.lon.mat0[,1] > btemp.lon.mat0[,2]
			btemp.lon.mat[btemp.lon.mat.logic,] <- btemp.lon.mat0[btemp.lon.mat.logic, 2:1]
			
			btemp.lat.mat0 <- matrix(c(t.lat.0, t.btemp.lat.t1),ncol=2)
			btemp.lat.mat <- btemp.lat.mat0
			btemp.lat.mat.logic <- btemp.lat.mat0[,1] > btemp.lat.mat0[,2]
			btemp.lat.mat[btemp.lat.mat.logic,] <- btemp.lat.mat0[btemp.lat.mat.logic, 2:1]
			
			btemp.ft <- numeric(length(t.lon.0))
			for(j in 1:length(t.lon.0)){
				t.btemp.lon.ft <- t.lon.0[j]>btemp.lon.mat[,1] & t.lon.0[j]<btemp.lon.mat[,2]
				t.btemp.lat.ft <- t.lat.0[j]>btemp.lat.mat[,1] & t.lat.0[j]<btemp.lat.mat[,2]
				btemp.ft[j] <- sum(t.btemp.lon.ft & t.btemp.lat.ft, na.rm=TRUE)
			}
			
			
			# =========================
			# = Calculate Burrows %'s =
			# =========================
			stemp.N.tot <- stempArrive.t + 1 + stemp.ft
			stemp.N.start <- rep(1, n.str)/stemp.N.tot
			stemp.N.end <- stempArrive.t/stemp.N.tot
			stemp.N.ft <- stemp.ft/stemp.N.tot
			stemp.cat <- burrow.cat(stemp.N.start, stemp.N.end, stemp.N.ft)
			
			btemp.N.tot <- btempArrive.t + 1 + btemp.ft
			btemp.N.start <- rep(1, n.str)/btemp.N.tot
			btemp.N.end <- btempArrive.t/btemp.N.tot
			btemp.N.ft <- btemp.ft/btemp.N.tot
			btemp.cat <- burrow.cat(btemp.N.start, btemp.N.end, btemp.N.ft)
			
			
			# =============================
			# = Calculate Alpha Diversity =
			# =============================
			alphaD.t <- diversity(castExp.t)
			alphaD.t1 <- diversity(castExp.t1)
			
			
			# ============================
			# = The Data Table to Return =
			# ============================
			comMatch0 <- data.table(
				# Basic information
				region=s.reg.key[s.reg], # full region name (handy for making nice graphs)
				year=t.yr, # current year
				dYear.t1.t=years[i+1]-t.yr, # size of time step
				stratum=strata, # this is the "origin" stratum (even if leapfrogging), not the "destination" stratum
				strat.lat.0=t.lat.0, # origin latitude
				strat.lon.0=t.lon.0, # origin longitude
				
				
				# Location information
				comStrat.t=strata[comMatch0], # destination stratum for the community (next year's best match)
				com.lat.t1=t.com.lat.t1, # destination latitude for the community
				com.lon.t1=t.com.lon.t1, # destination longitude for the community
				
				comStrat.t0.match=strata[comMatch.t.t0.0], # stratum in current year that is best match to original community
				com.lat.t0.match=t.com.lat.t0.match, # latitude of current match to original
				com.lon.t0.match=t.com.lon.t0.match, # longitude of current match to original
				
				stempStrat.t1=strata[stempMatch0][st2na], # destination stratum for surface temperature
				stemp.lat.t1=t.stemp.lat.t1[st2na], # destination latitude for surface temperature
				stemp.lon.t1=t.stemp.lon.t1[st2na], # destination longitude for surface temperature
				
				btempStrat.t1=strata[btempMatch0][bt2na], # destination stratum for bottom temperature
				btemp.lat.t1=t.btemp.lat.t1[bt2na], # destination latitude for bottom temperature
				btemp.lon.t1=t.btemp.lon.t1[bt2na], # destination longitude for bottom temperature
				
				
				# Movement information
				com.dLon.t1.t=t.com.lon.t1-t.lon.0, # shift in ºlongitude between current community and its destination
				com.dLat.t1.t=t.com.lat.t1-t.lat.0, # shift in ºlatitude between current community and its destination
				com.dll.t1.t0=com.dll.t1.t0, # distance from community origin (year t0) to destination (year t+1) position in ll
				com.dll.0.tot=com.dll.0.tot, # sum of all community current-to-destination distances up to this point, in lat-lon
				com.dll.t1.t=com.dll.t1.t, # distance (lat-lon) community moved from current (year t) to destination (year t+1)
				com.dll.angle.t1.t=com.dll.angle.t1.t, # angle between destination community and current community
				com.dll.angle.t1.t0=com.dll.angle.t1.t0, # angle between destination community and original community (only matters for leapfrog?)
				
				com.dLon.t0.match=t.com.lon.t0.match-t.lon.0, # shift in ºlongitude between original community and its current best match
				com.dLat.t0.match=t.com.lat.t0.match-t.lat.0, # shift in ºlatitude between original community and its current best match
				
				stemp.dLon.t1.t=t.stemp.lon.t1[st2na]-t.lon.0,
				stemp.dLat.t1.t=t.stemp.lat.t1[st2na]-t.lat.0,
				stemp.dll.t1.t0=stemp.dll.t1.t0[st2na], # surface temperature distance from origin to current (ll)
				stemp.dll.t1.t=stemp.dll.t1.t[st2na], # distance (ll) surface temperature moved this time step
				stemp.dll.angle.t1.t=stemp.dll.angle.t1.t[st2na],
				stemp.dll.angle.t1.t0=stemp.dll.angle.t1.t0[st2na],
				
				btemp.dLon.t1.t=t.btemp.lon.t1[bt2na]-t.lon.0,
				btemp.dLat.t1.t=t.btemp.lat.t1[bt2na]-t.lat.0,
				btemp.dll.t1.t0=btemp.dll.t1.t0[bt2na], # distance (ll) between bottom tempearture origin and destination
				btemp.dll.t1.t=btemp.dll.t1.t[bt2na], # distance (ll) between bottom temperature current position and destination
				btemp.dll.angle.t1.t=btemp.dll.angle.t1.t[bt2na],
				btemp.dll.angle.t1.t0=btemp.dll.angle.t1.t0[bt2na],
				
				
				# These are sort of like the residuals from the match – the "degree of mismatch" in the "best match", if you will
				dComp.t.t0=t.dComp.t.t0, # community distance (betaD) between original (t0) and current (t) communities
				dComp.t1.t0=t.dComp.t1.t0, # community distance (betaD) between original (t0) and destination (t+1) communities
				dComp.t1.t=t.dComp.t1.t, # community distance (betaD) between current (t) and destination (t+1) communities
				dComp.t.t0.matched=t.dComp.t.t0.matched, # community distance (betaD) between original (t0) and current best match (matched to t) communities
				
				dStemp.t.t0=t.dStemp.t.t0,
				dStemp.t1.t0=t.dStemp.t1.t0[st2na], # change in surface temperature between origin (t0) and destination (t+1)
				dStemp.t1.t=t.dStemp.t1.t[st2na], # change in surface temperature between current (t) and destination (t+1)
				
				dBtemp.t.t0=t.dBtemp.t.t0,
				dBtemp.t1.t0=t.dBtemp.t1.t0[bt2na], # change in bottom temperature between origin (t0) and destination (t+1)
				dBtemp.t1.t=t.dBtemp.t1.t[bt2na], # change in bottom temperature between current (t) and destination (t+1)
				
				
				# Burrows Counts and Cats
				stemp.N.start=stemp.N.start,
				stemp.N.end=stemp.N.end,
				stemp.N.ft=stemp.N.ft,
				stemp.N.tot=stemp.N.tot,
				stemp.cat=stemp.cat,
				
				btemp.N.start=btemp.N.start,
				btemp.N.end=btemp.N.end,
				btemp.N.ft=btemp.N.ft,
				btemp.N.tot=btemp.N.tot,
				btemp.cat=btemp.cat,
				
				
				# Alpha Diversity measures
				alphaD.t=alphaD.t,
				alphaD.t1=alphaD.t1,
				dAlphaD.t.t0=alphaD.t-alphaD.t0,
				dAlphaD.t1.t0=alphaD.t1-alphaD.t0,
				dAlphaD.t1.t=alphaD.t1-alphaD.t,
				
				# 
				stemp=stemp.castExp.t, 
				btemp=btemp.castExp.t
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

save(shifts, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/frog_shifts.RData")


