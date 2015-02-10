
# =================
# = Load Packages =
# =================
library(raster)
library(SDMTools)
library(plyr)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# =============================
# = Load statistics functions =
# =============================
stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# =============
# = Load Data =
# =============
load("./trawl/Data/HadISST.RData")
sst.mu0 <- raster.nan2na(sst.mu)
sst.ann0 <- raster.nan2na(sst.ann)
rm(list=c("sst.mu","sst.ann"))

load("./trawl/Results/HadISST/cover.type.RData")


# ================================
# = Define Trajectory Resolution =
# ================================
# Define spatial and temporal resolution of trajectory iteration
n.per.yr <- 10 # number of time steps per year (burrows used 10) Burrows kept the 1º grid size for his calculations, but mine become smaller as I increase the number of trajectories. So I should probably do 10*n.per.ll to keep the same ratio of (length of edge of grid cell) : (number of steps per year)
n.per.ll <- 2 # sqrt(number of cells per 1 degree grid cell) (burrows used 10)

n.yrs <- dim(sst.ann0)[3] # number of years
step.index <- seq(2, n.yrs*n.per.yr, length.out=n.yrs*n.per.yr-1) # time counter for loop
tYrs <- rep(1:n.yrs, each=n.per.yr) # reference to the year # that lines up with step.index


# ==================================================
# = High Res & Correct Projection for Temperatures =
# ==================================================
# Expand sst to higher resolution, while avoiding extra NA's and repeated values in the rook
sst.ann.s0 <- disaggregate(sst.ann0, n.per.ll, method="bilinear") # "small" grid size for annual sea surface temperature
sst.ann.s3 <- reclassify(disaggregate(sst.ann0, n.per.ll), cbind(-Inf, Inf, 1))
sst.ann <- sst.ann.s0*sst.ann.s3 # redefining sst.ann, now want to use this for everything

cover.type.s <- disaggregate(cover.type, n.per.ll, method="") # depth data!
# 1 = land
# 2 = low land
# 3 = high water
# 4 = shelf
# 5 = deep


sst.mu.s0 <- stackApply(sst.ann.s0, indices=rep(1, nlayers(sst.ann.s0)), fun=mean)
sst.mu2 <- stackApply(sst.ann, indices=rep(1, nlayers(sst.ann)), fun=mean)
crs(sst.mu0) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # need projection for terrain()
crs(sst.mu.s0) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # need projection for terrain()
crs(sst.mu2) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # need projection for terrain()


# ==================================
# = Get the temporal trend for SST =
# ==================================
# TODO It seems like the temporal trend should be calculated at a finer temporal scale, and that there shouldn't be just 1 climate velocity for the entire time period. I will certainly need to do this calculation for different starting times, as the trawl time series start at different times.
timeTrend <- stackApply(sst.ann, indices=rep(1,nlayers(sst.ann)), fun=timeSlope)


# =========================
# = Get the spatial slope =
# =========================
# Get the spatial gradient from the original coarse sst, then disaggregate; this is the simplest form
spatSlope0 <- disaggregate(slope(sst.mu0, latlon=TRUE), n.per.ll) # spatial gradient, then disaggregate
spatSlope0[is.nan(spatSlope0)] <- NA # turn NaN's to NA's

# Problem: above spatial gradient has NA's where there are SST's
# Solution: spatial averaging to fill in gaps
spatSlope.f <- slope(sst.mu.s0, latlon=TRUE) # spatial slopes taken from a fine spatial resolution
spatSlope.03 <- sloFill(spatSlope0, 3, 3) # do a spatial average of simple slope to get slopes for most of the problematic NA's in spatSlope0
spatSlope.f7 <- sloFill(spatSlope.f, 7, 7) # to fill in the remaining NA's, do a spatial averaging of the spatial slopes that were calculated from the bilinearly interpolated sst's (to be used most sparingly b/c it's on the largest grid, and b/c it required initial bilinear interpolation before slope was even calculated)

# Set up Logic for what's NA in each of the slope rasters, as well as what's NA in the sst raster
sst2NA <- !is.finite(sst.mu2)
ss0NA <- !is.finite(spatSlope0)
ss03NA <- !is.finite(spatSlope.03)
ssf7NA <- !is.finite(spatSlope.f7)

# Set up logic for which slope raster to use
pick.ss0 <- !ss0NA & !sst2NA
pick.ss03 <- ss0NA & !sst2NA & !ss03NA
pick.ssf7 <- ss0NA & !sst2NA & ss03NA & !ssf7NA

# Annoying: 0*NA = NA, so have to change NA's to 0's before adding up slopes (adding is a way of adding values to a subset)
ss0.vals <- spatSlope0
ss0.vals[is.na(ss0.vals)] <- 0
ss0.vals <- ss0.vals*pick.ss0

ss03.vals <- spatSlope.03
ss03.vals[is.na(ss03.vals)] <- 0
ss03.vals <- ss03.vals*pick.ss03

ssf7.vals <- spatSlope.f7
ssf7.vals[is.na(ssf7.vals)] <- 0
ssf7.vals <- ssf7.vals*pick.ssf7

# Now tally up the final spatial gradient
spatSlope <- ss0.vals + ss03.vals + ssf7.vals + sst.mu2*(sst2NA) # last term makes sure we didn't average-in velocities for places that we don't even have temperature


# ===============================
# = Calcualte the spatial angle =
# ===============================
# Calculate the angle of the spatial slope
spatAng0 <- disaggregate(terrain(sst.mu0, opt="aspect", unit="radians"), n.per.ll) # direction of spatial gradient
spatAng0[is.nan(spatAng0)] <- NA

# Problem: above spatial gradient has NA's where there are SST's
# Solution: spatial averaging to fill in gaps
# Note!: Can't just do the average of the angles, like I did with the slopes, because you run into circular problems.
# Instead I'm doing the spatial averaging of the water temperatures, then taking those angles
# spatAng.03 <- terrain(disaggregate(sloFill(sst.mu, 3, 3), n.per.ll), opt="aspect", unit="radians") #
# spatAng.f7 <- sloFill(terrain(sst.mu.s0, opt="aspect", unit="radians"), 7, 7) # to fill in the remaining NA's, do a spatial averaging of the spatial slopes that were calculated from the bilinearly interpolated sst's (to be used most sparingly b/c it's on the largest grid, and b/c it required initial bilinear interpolation before slope was even calculated)
spatAng.03 <- angFill(disaggregate(terrain(sst.mu0, opt="aspect", unit="radians"), n.per.ll), 3, 3)
spatAng.f7 <- angFill(terrain(sst.mu.s0, opt="aspect", unit="radians"), 7, 7) #angFill(terrain(sst.mu.s0, opt="aspect", unit="radians"), 7, 7)

# Set up Logic for what's NA in each of the slope rasters, as well as what's NA in the sst raster
sst2NA <- !is.finite(sst.mu2)
sa0NA <- !is.finite(spatAng0)
sa03NA <- !is.finite(spatAng.03)
saf7NA <- !is.finite(spatAng.f7)

# Set up logic for which slope raster to use
pick.sa0 <- !sa0NA & !sst2NA
pick.sa03 <- sa0NA & !sst2NA & !sa03NA
pick.saf7 <- sa0NA & !sst2NA & sa03NA & !saf7NA

# Annoying: 0*NA = NA, so have to change NA's to 0's before adding up slopes (adding is a way of adding values to a subset)
sa0.vals <- spatAng0
sa0.vals[is.na(sa0.vals)] <- 0
sa0.vals <- sa0.vals*pick.sa0

sa03.vals <- spatAng.03
sa03.vals[is.na(sa03.vals)] <- 0
sa03.vals <- sa03.vals*pick.sa03

saf7.vals <- spatAng.f7
saf7.vals[is.na(saf7.vals)] <- 0
saf7.vals <- saf7.vals*pick.saf7

# Now tally up the final spatial gradient
spatAng <- sa0.vals + sa03.vals + saf7.vals + sst.mu2*(sst2NA) # last term makes sure we didn't average-in velocities for places that we don't even have temperature


# ==============================================
# = Define Great Lakes Region Extent and Cells =
# ==============================================
# These will need to be removed – I don't want to calculate velocities here b/c there really aren't enough cells
# extentGL <- drawExtent()
# extentGL
# class       : Extent
# xmin        : -91.84594
# xmax        : -75.69788
# ymin        : 40.2384
# ymax        : 49.70159
# See the extent coordinates in the map in the comment of this commit:
# https://github.com/rBatt/trawl/commit/7d14ea798861957e2d4d02cf8df41e6824a19103?diff=unified
extentGL <- extent(-91.84594, -75.69788, 40.23840, 49.70159)
cellsGL <- cellsFromExtent(spatSlope, extent=extentGL)


# ====================================
# = Trim/ create mean and annual sst =
# ====================================
# Trim to make GL extent NA for annual sst
sst.yrly <- sst.ann
sst.yrly[cellsGL] <- NA

# Trim to make GL extent NA for annual sst
sst.mu <- sst.mu2
sst.mu[cellsGL] <- NA


# ===========================
# = Calculate Climate Speed =
# ===========================
# Climate velocity and its angle, high resolution
climV <- (timeTrend/spatSlope)*(1/n.per.yr) #disaggregate(timeTrend/spatGrad.slope, n.per.ll)*(1/n.per.yr)  # climate speed in km/yr
climV[cellsGL] <- NA

ang <- spatAng # disaggregate(spatAng, n.per.ll) # final spatial resolution for the angle of climate velocity
ang[cellsGL] <- NA

# Calculate X and Y velocities
dXkm <- climV*sin(ang) # the X speed (km/yr to the east)
dYkm <- climV*cos(ang) # the Y speed (km/yr to the north)


# ============================
# = Get full lon, lat, cell# =
# ============================
# lons <- setValues(ang, rep(seq(xmin(ang), xmax(ang), length.out=ncol(ang)), nrow(ang)))
# lats <- setValues(ang, rep(seq(ymax(ang), ymin(ang), length.out=ncol(ang)), each=nrow(ang)))

lls <- xyFromCell(ang, 1:ncell(ang))
lons <- setValues(ang, lls[,1])
lats <- setValues(ang, lls[,2])

miLa <- min(values(lats)) - 0.25
maLa <- max(values(lats)) + 0.25
miLo <- min(values(lons)) - 0.25
maLo <- max(values(lons)) + 0.25

#
# # ===================================
# # = Get the rook velocities, angles =
# # ===================================
# rookV <- stack(adjV(4), adjV(6), adjV(2), adjV(8), adjV(5)) # these are the velocities for each of the 4 possible directions a trajectory can go when the calculated velocity would make it go from sea to land; which of the 4 directions chosen depends on the sign of the velocity,
# # rookV[is.na(rookV)] <- 0 # set NA's to 0 because these values will eventually just be added to other longitudes and latitudes, and if these velocities are NA,
# rookAng <- stack(adjAng(4), adjAng(6), adjAng(2), adjAng(8), adjAng(5)) # the angle, in radians, for the rook directions
# # cellNum <- setValues(ang, 1:length(ang))


# ==============================================
# = Calculate X&Y rook velocities, limit to 1º =
# ==============================================
# dXkm.rook0 <- rookV*sin(rookAng)
# dYkm.rook0 <- rookV*cos(rookAng)

conv.fact.lon.init <- 111.325*cos(lats/180*pi) # this value is used inside limitV(), but is defined here to reduce computation time
# dXkm.rook <- limitV(dXkm.rook0, dir="lon", conv.fact.lon=conv.fact.lon.init)
# dYkm.rook <- limitV(dYkm.rook0, dir="lat")
# dXkm.rook <- limitV(rookV, dir="lon", conv.fact.lon=conv.fact.lon.init)
# dYkm.rook <- limitV(rookV, dir="lat")
#
# # Changes NA rook velocities to 0, because they'll be added to starting lon/lat (after conversion from km to degrees)
# # I don't think any of these should be NA, though
# dXkm.rook[is.na(dXkm.rook)] <- 0
# dYkm.rook[is.na(dYkm.rook)] <- 0


# ====================================================
# = Define Initial Values for Trajectory Calculation =
# ====================================================
# Set initial values for destination velocities and locations
destV <- climV # TODO delete? is this needed? maybe was only needed if I wasn't going to calculate all the rook velocities ahead of time
# TODO O wait, destV might be needed b/c I'm not sure if I'm comfortable testing for the sign of velocity using dest.dX in adjDest(); should probably use destV. But I still don't think I need destAng.
# destAng <- ang # TODO delete? is this needed? same reasoning as for destV
dest.dX <- dXkm # the X speed in the previous destination location (updated at the end of each time step)
dest.dY <- dYkm # the Y speed in the previous destination location
# dest.dX.rook <- dXkm.rook
# dest.dY.rook <- dYkm.rook
dest.LL <- cbind(values(lons), values(lats)) # same as starting LL, but will be updated each iteration after adjDest


# Create empty bricks to hold trajectory lon/ lat at each time step
emptyBrick <- brick(array(NA, dim=dim(sst.ann0)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=miLo, xmx=maLo, ymn=miLa, ymx=maLa)
trajLon <- emptyBrick # empty lon brick
trajLon <- setValues(trajLon, values(lons), layer=1) # update first year (layer) of brick to give starting lon

trajLat <- emptyBrick # empty lat brick
trajLat <- setValues(trajLat, values(lats), layer=1) # update first year (layer) of brick to give starting lat

# Create empty brick to hold the number of trajectories starting and stopping in each cell
trajStart <- emptyBrick
trajStop <- emptyBrick

# Focal weight matrix: this is used by focal.min and focal.max when called within adjDest (faster to define globally than to continually recreate matrix thousands of times)
# fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix; called inside focal.min/max()
# fw.mat <- matrix(c(NA,1,NA,1,1,1,NA,1,NA),ncol=3) # focal weight matrix; called inside focal.min/max()

cellVals <- matrix(rep(1:ncell(climV), length(step.index)), ncol=length(step.index))
startLoc <- matrix(rep(NA, ncell(climV)*length(step.index)), ncol=length(step.index))
stopLoc <- matrix(rep(NA, ncell(climV)*length(step.index)), ncol=length(step.index))

# ====================================================
# = Begin the iterative construction of trajectories =
# ====================================================
sst.pb <- txtProgressBar(min=2, max=max(step.index), style=3)
for(i in step.index){
# for(i in 2:13){
	t.yr <- tYrs[i]
	t.temp <- subset(sst.yrly, t.yr)
	
	
	# ===================================================
	# = Starting values for trajectory (this time step) =
	# ===================================================
	start.temp <- setValues(t.temp, extract(t.temp, dest.LL))
	start.cover <- setValues(cover.type.s, extract(cover.type.s, dest.LL)) # CHANGED added depth ...!
	
	if(!all(values(start.cover == cover.type.s))){stop("cover type boundary has been crossed (start of loop)")}
	
	# Extract the longitude and latitude of starting location
	start.lon <- subset(trajLon, i-1) # longitude of the trajectory at the start of this time step (end of last time step)
	start.lat <- subset(trajLat, i-1) # latitude of the trajectory at the start of this time step (end of last time step)
	start.LL <- cbind(values(start.lon), values(start.lat)) # format starting LL
	
	start.cell <- setValues(start.temp, cellFromXY(start.temp, start.LL)) # change LL to cell#
	
	
	# =================================
	# = Proposed trajectory locations =
	# =================================
	# Calculate the longitude and latitude of proposed destination
	prop.dLon <- dest.dX/(111.325*cos(start.lat/180*pi)) # convert horizontal km/timeStep speed into dLon/timeStep # CHANGED added parentheses ... 
	prop.dLat <- dest.dY/111.325
	
	# Friendly warning for large velocities
	if(any(values(prop.dLon)>=(1/n.per.ll) & !is.na(values(prop.dLon))) | any(values(prop.dLat)>=(1/n.per.ll) & !is.na(values(prop.dLat)))){warning("Proposed velocities exceed 1 grid cell! Use smaller time steps")}
	
	prop.lon <- start.lon + prop.dLon # calculate the proposed longitude from speeds and starting LL
	prop.lat <- start.lat + prop.dLat # calculate the proposed latitude from speeds and starting latitude
	prop.LL <- cbind(values(prop.lon), values(prop.lat)) # format proposed LL	
	prop.LL[is.na(values(dest.dX)),] <- cbind(values(start.lon), values(start.lat))[is.na(values(dest.dX)),] # if the velocity is NA, it's not going anywhere; but still need to keep track of the location of the cell.	
	prop.cell <- setValues(start.temp, cellFromXY(start.temp, prop.LL)) # change LL to cell#; could do prop.temps, but haven't subset yet; is a location on the map
	prop.temp <- setValues(t.temp, extract(t.temp, prop.LL)) # the temperature in the proposed location (used to determine if destination is on land)
	prop.cover <- setValues(cover.type.s, extract(cover.type.s, prop.LL)) # CHANGED added depth ... !
	
	
	# =============================
	# = Bad proposed destinations =
	# =============================
	out.of.range <- (!is.na(prop.lon) & (prop.lon<=(miLo) | prop.lon>=(-maLo))) | (!is.na(prop.lat) & (prop.lat<=(miLa) | prop.lat>=(maLa)))
	
	any.miss <- (is.na(prop.lon) | is.na(prop.lat) | is.na(prop.cover)) & is.finite(start.temp) # this is important – previously just assumed that the only NA's in the prop.lon/lat were due to NA velocities ... this is correct as far as I can tell. You only get the NA velocity if in a place with NA temp (land, e.g.). However, it's the prop.cover that's catching missings. These are locations with finite starting temperatures, and missing cover types.
	cross.cover <- prop.cover != start.cover & !is.na(prop.cover) # change in cover type
	land.range.temp <- !is.finite(prop.temp) & is.finite(start.temp) # could be b/c crossing to land, or b/c going off edge of map, or if temp is missing for some other reaso
	badProp <- land.range.temp | out.of.range | cross.cover | any.miss # this is an index corresponding to cell ID's
	# old logic:  (!is.finite(prop.cover) | (prop.cover != start.cover)) | (!is.finite(prop.temp) & is.finite(start.temp)) | out.of.range 
	badProp.cellID <- which(values(badProp)) # ID's
	badProp.cell <- values(prop.cell)[values(badProp)] # destination cell#'s for bad proposals; is the location on the map
	badProp.start.cell <- values(start.cell)[values(badProp)] # starting cell#'s for bad proposed trajectory
	
	
	# ==========================================
	# = Adjust trajectories with bad proposals =
	# ==========================================
	if(any(values(badProp))){
		catDir <- function(x){ # converting dest-start cell#'s into rook directions
			x0 <- integer(length(x))
			x0[x==-1] <- 4
			x0[x==1] <- 6
			x0[x==-ncol(t.temp)] <- 2
			x0[x==ncol(t.temp)] <- 8
			x0[x==0] <- 5
			x0
		}
	
		neighs0 <- adjacent(t.temp, badProp.start.cell, sorted=TRUE, id=TRUE, include=TRUE) # what are the neighboring cell#'s?
		# Note: this previous step with adjactent() is extremely important but potentially confusing
			# Remember that in some cases a raster cell represents a true lon/lat, whereas in other cases it just represents the lon/lat of a trajectory at the beginning of the time series
			# the t.temp is not important
			# what is important is that I am searching for neighboring cell#'s, not for neighboring temperatures of specific cells
			# the distinction is that in the raster, a cell doesn't necessarily have a spatial relationship to its neighbors after the first time step, so I can't search for neighboring temperatures *directly*, i have to find the neighboring cells, then reference a temperature raster that has the spatial relationship of cells intact (t.temp)
		nfrom <- neighs0[,"from"] # starting cell # (refers to a location on the map, not to the ID of the trajectory (ID is the location in the first step))
		nto <- neighs0[,"to"] # rook cell#'s around the starting cell
	
		# A bunch of values to keep track of while adjusting the trajectory using rook neighbors
		# Note: in relation to previous statement about maintaining spatial relationships when appropriate
			# notice that the "from" and "to" temperatures are taken from t.temp, which has the spatial relationship intact
			# if I wanted to get the velocities of neighbors, it would NOT work to reference dest.dX, e.g., because the raster neighbors in dest.dX were only spatial neighbors at the first time step, and now they may or may not be next to each b/c the trajectories evolve
		fromTemp <- extract(t.temp, nfrom) # temperature of the starting cells that had bad proposed destinations
		toTemp <- extract(t.temp, nto) # temperature of the potential rook destinations (NOT proposed destinations)
		fromCover <- extract(cover.type.s, nfrom)
		toCover <- extract(cover.type.s, nto) # TODO BUG! need to check not the rook (which just gives the direction), but the actual destionation. SO the "to" is where the adjusted trajectory is "aiming", but I need to do the checks for where it "hits"
		toDir <- catDir(nto-nfrom) # the rook direction
		delTemp <- toTemp-fromTemp # the change in temperature between the rook destination and the starting cell
		fromV <- extract(climV, nfrom) # the velocity in the starting cell
		fromAng <- extract(ang, nfrom) # the angle of the velocity in the starting cell
		delTemp.adj <- delTemp*sign(fromV) # find cooler for +vel, warmer for -vel; flip sign of dTemp if -vel, so I can just use which.min() for all
		toAng <- adjAng(toDir) # the angle (0 is north) corresponding to each rook direction
		delAng <- toAng-fromAng # the change in angle between the rook direction and the original angle of the spatial velocity
		
		# CHANGED These 3 lines are where the #13 "why do some trajectories end up on land?" bug was fixed
		# Other fixes along the way may have helped, but this was the last one :)
		new.fromLL <- start.LL[values(badProp),][neighs0[,"id"],] # CHANGED hopefully this is actually a fix – I know it does something different (see next 2 lines). I hope I'm not confused about the original code being wrong due to me being confused about the difference between ID and map location. ID is just map location at the first time step.
		fromLon <- new.fromLL[,1] #extract(start.lon, nfrom) # starting lons
		fromLat <- new.fromLL[,2] #extract(start.lat, nfrom) # starting lats		
		
		climSpeed <- abs(fromV/cos(delAng)) # the "raw" (will have to be converted to degrees and limited) speed for adjusted traj
		climV.adj <- convV(climSpeed, toDir, lat=fromLat, n.per.ll=n.per.ll) # convert speed into lon/lat components in degrees, and limit magnitude
		adjLon <- fromLon+climV.adj$dLon # change in longitude for each rook direction
		adjLat <- fromLat+climV.adj$dLat # change in latitude for each rook direction
		
		# The chosen rook is where the adjusted trajectory "aims"
		# Below I am grabbing information about where the adjusted trajectory will "hit"
		to.hit <- cellFromXY(t.temp, cbind(adjLon, adjLat))
		cover.hit1 <- extract(cover.type.s, to.hit)
		cover.hit2 <- extract(cover.type.s, cbind(adjLon, adjLat))
		
		# Check for some Errors
		if(sum(cover.hit1 != cover.hit2 & (!is.na(cover.hit1) & !is.na(cover.hit2)))!=0){warning("Apparent rounding error in adjTraj when converting Lon Lat to cell #")}
		
		# Rook Neighbor Info
		neighs <- cbind(neighs0, # store all of the above variables together so they can be conveniently searched w/ ddply()
			badPropCell=badProp.cell[neighs0[,1]], # the destination of the bad proposal
			to.hit,
			fromTemp=fromTemp,
			toTemp=toTemp,
			toDir=toDir,
			delTemp=delTemp,
			fromV=fromV, 
			fromAng=fromAng,
			delTemp.adj=delTemp.adj,
			fromLon=fromLon,
			fromLat=fromLat,
			adjLon=fromLon+climV.adj$dLon,
			adjLat=fromLat+climV.adj$dLat,
			fromCover=fromCover,
			toCover=toCover,
			cover.hit1
		)
		
		# Check for some Errors
		if(!all(cellFromXY(t.temp, neighs[,c("fromLon","fromLat")]) == neighs[,"from"])){stop("In neighs, 'from' coordinates do not match 'from' cell!")}
		
		# Function to Find Best Rook
		findAdj <- function(x){
			# rawChoice <- order(x[,"delTemp.adj"])
			cvrLogic <- x[,"fromCover"]==x[,"toCover"]
			bndLogic <- x[,"adjLon"]<=(maLo) & x[,"adjLon"]>=(miLo) & x[,"adjLat"]<=(maLa) & x[,"adjLat"]>=(miLa)
			
			if(x[1,"toDir"]!=5){warning("x[1,'toDir']!=5. Check findAdj function.")}
			
			if(sum((cvrLogic&bndLogic))==0){return(x[1,])} # it should be basically impossible for this to happen
			
			if(any(is.finite(x[cvrLogic&bndLogic,"delTemp.adj"]))){
				return(x[cvrLogic&bndLogic,][which.min(x[cvrLogic&bndLogic,"delTemp.adj"]),])
			}else{
				return(x[1,]) # should just be the same as x[x[,"toDir"]==5,] == x[,1]; ugh, ok, I'll be explicit just to make sure; on the other hand, if that row somehow gets dropped ... Also, is the 5 a char? No, it's numeric. I'm being paranoid.
			}
		}
	
		# Find Best Rook
		adjTraj <- ddply(as.data.frame(neighs), c("id"), findAdj) # the rows of neighs corresponding to the rook matches
		
		
		# Check for some Errors
		if(!all(cellFromXY(t.temp, adjTraj[,c("fromLon","fromLat")]) == adjTraj[,"from"])){stop("In adjTraj, 'from' coordinates do not match 'from' cell!")}
	
		if(!all(adjTraj[,"fromCover"] == adjTraj[,"cover.hit1"])){stop("cover type boundary has been crossed (right after adjTraj)")}
			
	
	}
	
	
	# =====================
	# = Final Destination =
	# =====================
	dest.LL <- prop.LL # in many cases, the proposed location is OK, and will be the destination
	# values(badProp) is an index of trajectory ID, not of a current location on the map
	if(any(values(badProp))){
		dest.LL[values(badProp),] <- as.matrix(adjTraj[, c("adjLon","adjLat")]) # where the proposal were bad, replace them with adjusted trajectory destinations
	}
	dest.lon <- dest.LL[,1]
	dest.lat <- dest.LL[,2]
	
	# Update dest speeds to reflect those in the final destination cell
	dest.dX <- setValues(dXkm, extract(dXkm, dest.LL)) # use setValues() to preserve raster class and structure
	dest.dY <- setValues(dXkm, extract(dYkm, dest.LL)) # note that the 1st object in setValues doesn't matter aside from its extent()
	
	# Update the destination LL in the trajectories (not rounded to correspond to cell)
	trajLon <- setValues(trajLon, dest.lon, layer=i)
	trajLat <- setValues(trajLat, dest.lat, layer=i)
	
	
	# =========================================
	# = Count starting and stopping locations =
	# =========================================
	sumStart <- integer(ncell(start.temp))
	tableStart <- c(table(cellFromXY(start.temp, start.LL)))
	sumStart[as.integer(names(tableStart))] <- tableStart
	trajStart <- setValues(trajStart, sumStart, layer=i)
	
	sumStop <- integer(ncell(start.temp))
	tableStop <- c(table(cellFromXY(start.temp, dest.LL)))
	sumStop[as.integer(names(tableStop))] <- tableStop
	trajStop <- setValues(trajStop, sumStop, layer=i)
		
	startLoc[,i-1] <- cellFromXY(start.temp, start.LL)
	stopLoc[,i-1] <- cellFromXY(start.temp, dest.LL)	
	
	setTxtProgressBar(sst.pb, i)
}


# ===========================
# = Categorize Trajectories =
# ===========================
trajEnd <- subset(trajStop, nlayers(trajStop))
trajStart <- subset(trajStart, 2)
uniStart0 <- table(c(cellVals), c(startLoc))
uniStart <- rowSums(uniStart0)>0

tblt <- function(x){
	tabulate(x, nbin=ncell(start.temp))
}

finalStops <- tblt(stopLoc[,ncol(stopLoc)])

nStops000 <- apply(stopLoc, 1, tblt)
nStops00 <- pmin(nStops000, 1)
nStops0 <- rowSums(nStops00, na.rm=TRUE)
nStops <- matrix(c(1:ncell(start.temp), nStops0), ncol=2)


nEnd0 <- setValues(climV, finalStops)
nEnd <- aggregate(nEnd0, n.per.ll, fun=sum)

nFlow0 <- setValues(climV, nStops0)-nEnd0
nFlow <- aggregate(nFlow0, n.per.ll, fun=sum)


n.denom <- nFlow+nEnd+n.per.ll*2
n.end <- nEnd/n.denom
n.ft <- nFlow/n.denom
n.start <-( n.per.ll*2)/n.denom

cSource <- n.end==0
cSink <- n.end>0.45 & n.start<0.15
cCorridor <- n.ft>0.7 & n.end>0
cDivergence <- n.end>n.start & !cCorridor & !cSink
cConvergence <- n.end<n.start & !cCorridor & !cSource


# plot(n.end==0, main="Source") # Source
# plot(n.end>0.45 & n.start<0.15) # Sink
# plot(n.ft>0.7 & n.end>0) # Corridor
# plot(n.end>n.start & !(n.ft>0.7 & n.end>0) & !(n.end>0.45 & n.start<0.15)) # Divergence
# plot(n.end<n.start & !(n.ft>0.7 & n.end>0) & !(n.end>0.45 & n.start<0.15)) # Convergence


#
# source.logic <- n.end==0
# sink.logic <- n.end>0.45 & n.start<0.15
# corridor.logic <- n.ft>0.7 & n.end>0
# divergence.logic <- n.end>n.start & !corridor.logic & !sink.logic
# convergence.logic <- n.start>n.end & !corridor.logic & !source.logic
# balance.logic <- n.start==n.end & !corridor.logic

# ===========================
# = Save Trajectory Results =
# ===========================
# Save a few objects used specifically for a couple figures I made (in plotHadISST.R)
# save(sst.mu, ang, spatSlope, timeTrend, file="./trawl/Results/HadISST/HadISST_tempGrads.RData")

# Save the trajectory locations
save(trajLon, trajLat, file="./trawl/Results/HadISST/HadISST_trajectories.shelf.RData")

# Save categories
save(cSource, cSink, cDivergence, cConvergence, file="./trawl/Results/HadISST/HadISST_categories.shelf.RData")

# Save the full image
save.image("./trawl/Results/HadISST/HadISST_trajectoriesImage.shelf.RData", compress="xz")

