
# =================
# = Load Packages =
# =================
library(raster)
library(SDMTools)


# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/HadISST.RData")


# =============================
# = Load statistics functions =
# =============================
stat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# ==================================
# = Get the temporal trend for SST =
# ==================================
# TODO It seems like the temporal trend should be calculated at a finer temporal scale, and that there shouldn't be just 1 climate velocity for the entire time period. I will certainly need to do this calculation for different starting times, as the trawl time series start at different times.
timeTrend <- stackApply(sst.ann, indices=rep(1,length(sst)), fun=sstTimeSlope)


# ============================
# = Get the spatial gradient =
# ============================
spatGrad.slope <- slope(sst.mu, latlon=TRUE)

sst.mu2 <- sst.mu # TODO Can probably just keep sst.mu and redefine its projection; shouldn't affect other calcs on sst.mu, but might be minor optimization improvement to not (essentially) duplicated the object in memory. If so, should maybe redefine it from start, just to ensure consistency.
crs(sst.mu2) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # The terrain() function requires that the projection be defined. As far as I can tell, the +lon_0 value in the projection doesn't affect the aspect calculation via terrain, so I'm not worrying about it. using the aspect() function was returning a lot of "no slope" values, so I don't want to use it.
spatGrad.aspect <- terrain(sst.mu2, opt="aspect", unit="degrees") # direction of spatial gradient

save(sst.mu, spatGrad.aspect, spatGrad.slope, timeTrend, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/HadISST_tempGrads.RData") # TODO I don't want to rewrite this .RData file every time I run this script during testing. What I do with this might depend on how much of this script I turn into separate functions, etc. So for now leaving it here.

# ===========================
# = Calculate Climate Speed =
# ===========================
climSpeed <- timeTrend/spatGrad.slope # climate speed in km/yr

spdX0 <- climSpeed*sin(spatGrad.aspect) # in km/yr (to the east)
spdY0 <- climSpeed*cos(spatGrad.aspect) # in km/yr (to the north)


# ==========================
# = Calculate Trajectories =
# ==========================
# Set up a series of initial values and options

# Define spatial and temporal resolution of trajectory iteration
n.per.yr <- 2 # number of time steps per year (burrows used 10)
n.per.ll <- 3 # sqrt(number of cells per 1 degree grid cell) (burrows used 10)

n.yrs <- dim(sst.ann)[3] # number of years
step.index <- seq(2, n.yrs*n.per.yr, length.out=n.yrs*n.per.yr-1) # time counter for loop
tYrs <- rep(1:n.yrs, each=n.per.yr) # reference to the year # that lines up with step.index

# Expand rasters to a higher resolution
spdX <- disaggregate(spdX0, 3) # fine spatial resolution for longitudinal climate speed
spdY <- disaggregate(spdY0, 3) # fine spatial resolution for latitudinal climate speed
ang <- disaggregate(spatGrad.aspect, 3) # final spatial resolution for the angle of climate velocity
sst.ann.s <- disaggregate(sst.ann, 3, method="bilinear") # "small" grid size for annual sea surface temperature

# Create empty bricks to hold trajectory lon/ lat at each time step
trajLon <- brick(array(NA, dim=dim(sst.ann)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=-190, xmx=-40, ymn=20, ymx=65) # empty lon brick
lons <- seq(xmin(trajLon), xmax(trajLon), length.out=ncol(trajLon)) # longitude values (X values)
lons.s <- rep(lons, nrow(trajLon)) # "small" (finer resolution) lons
trajLon <- setValues(trajLon, lons.s, layer=1) # update first year (layer) of brick to give starting lon

trajLat <- brick(array(NA, dim=dim(sst.ann)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=-190, xmx=-40, ymn=20, ymx=65) # empty lat brick
lats <- seq(ymax(trajLat), ymin(trajLat), length.out=ncol(trajLat)) # latitude values (Y values)
lats.s <- rep(lats, each=nrow(trajLat)) # "small" (finer resolution) lats
trajLat <- setValues(trajLat, lats.s, layer=1) # update first year (layer) of brick to give starting lat

# Define appropriate resolution for velocities
dXkm.s <- spdX*(1/n.per.yr) # "small" (per-time-step) change in X
dYkm.s <- spdY*(1/n.per.yr) # "small" (per-time-step) change in Y

# Set initial values for destination velocities and locations
dest.dX <- dXkm.s # the X speed in the previous destination location (updated at the end of each time step)
dest.dY <- dYkm.s # the Y speed in the previous destination location
dest.LL <- cbind(lons.s, lats.s) # same as starting LL, but will be updated each iteration after adjDest

# A few functions to prepare for trajectory adjustment (or to be used in said process)
focal.min <- function(r.min){ # where is the smallest value in the rook focus?
	# calculate smallest (or biggest) temperature in the focal region, do smallestFocalTemp - temp, and find which is smallest *and* is also negative. This is going to be tough. Might have to do this in the other function, later.
	fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	focal(r.min, w=fw.mat, which.min, pad=TRUE) # focal raster cell# containing smallest value
}

focal.max <- function(r.max){ # where is the biggest value in the rook focus?
	fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	focal(r.max, w=fw.mat, which.max, pad=TRUE) # focal raster cell# containing biggest value
}

focal2cell <- function(f){ # convert the focal raster cell# to parent raster cell#
	f.cell <- setValues(f, 1:length(f)) # cell #'s for raster
	f.ncol <- ncol(f) # number of columns in raster
	f.conv <- reclassify(f, cbind(c(4,6,2,8),c(-1,1,-f.ncol,f.ncol))) # convert focal cell# to raster cell# (assumes 3x3 focal)
	f.cell+f.conv # convert
}

# Function to adjust the proposed destination of the trajectory, if it needs it.
adjDest <- function(start.vel, start.temp, prop.temp, startCell, propCell){
	
	# Find coolest and warmest rook neighbors (location and temperature)
	coolFocal <- focal.min(start.temp) # gets the cell# within the focal matrix around start.temp
	coolCell <- focal2cell(coolFocal) # get convert the cell # in the focal (3x3) raster to the cell# in the full raster
	coolTemp <- setValues(start.temp, extract(start.temp,values(coolCell))) # temp of coolest rook neighbor
	
	warmFocal <- focal.max(start.temp) # gets the cell # within the focal matrix
	warmCell <- focal2cell(warmFocal) # convert focal cell # to full-raster cell#
	warmTemp <- setValues(start.temp, extract(start.temp,values(warmCell))) # temp of warmest rook neighbor
	
	# Define logical rasters associated with each possible categorical outcome (to which category does each cell belong?)
	belAdj <- is.na(prop.temp) & !is.na(start.temp) # logical: does the destination need to be adjusted?
	belProp <- !belAdj # logical: is the proposed trajectory OK?
	belCool <- belAdj & start.vel<0 & coolTemp<start.temp # logical: should+can we reject the prop & find cooler neighbor?
	belWarm <- belAdj & start.vel>0 & warmTemp>start.temp # logical: should+can we reject the prop & find warmer neighbor?
	belStart <- belAdj & !(belCool|belWarm) # logical: needs adj, but can't find warmer/cooler rook? (this is coastal sink)
	
	# A cell should not be TRUE in more than one of the bel___ rasters; if it does, my logic is flawed
	sumBelongs <- values(belProp+belCool+belWarm+belStart) # the number adjustment outcomes to which each cell belongs
	stopifnot(is.na(sumBelongs)|sumBelongs<=1) # check to ensure that *at most* 1 adjustment is made to each proposed trajectory
	
	# Rasters can be easily added, and it is easy to define a cell as T/F/NA
	# T/F are interpreted as 1/0
	# A cell should not be TRUE in more than one of the bel___ rasters (see check above)
	# This provides means to do easy conditional replacement/ matching
	# Multiply each category of outcome w/ the appropriate logic, add products
	# This applies the appropriate adjustment to each proprosed trajectory that needs adjusting (values are cell#)
	destCell <- belProp*propCell + belStart*startCell + belCool*coolCell + belWarm*warmCell # checked/adjusted destination cell
	
	return(destCell)
}





sst.pb <- txtProgressBar(min=2, max=max(step.index), style=3)
for(i in step.index){
	t.yr <- tYrs[i]
	
	t.temp <- subset(sst.ann.s, t.yr)
	start.temp <- setValues(t.temp, extract(t.temp, dest.LL))
	#
	# t.temp.coarse <- aggregate(t.temp, n.per.ll)
	# start.temp.coarse <- aggregate(start.temp, n.per.ll)
	
	# Extract the longitude and latitude of starting location
	start.lon <- subset(trajLon, i-1) # longitude of the trajectory at the start of this time step (end of last time step)
	start.lat <- subset(trajLat, i-1) # latitude of the trajectory at the start of this time step (end of last time step)
	start.LL <- cbind(values(start.lon), values(start.lat)) # format starting LL
	start.cell <- setValues(start.temp, cellFromXY(start.temp, start.LL)) # change LL to cell#
	
	# Calculate the longitude and latitude of proposed destination
	prop.lon <- start.lon + dest.dX/111.325*cos(start.lat) # calculate the proposed longitude from speeds and starting LL
	prop.lat <- start.lat + dest.dY/111.325 # calculate the proposed latitude from speeds and starting latitude
	prop.LL <- cbind(values(prop.lon), values(prop.lat)) # format proposed LL
	prop.cell <- setValues(start.temp, cellFromXY(start.temp, prop.LL)) # change LL to cell#; could do prop.temps, but haven't subset yet
	
	# Extract cell# and X/Y speeds of proposed cell
	prop.temp <- setValues(t.temp, extract(start.temp, prop.LL)) # the temperature in the proposed location (used to determine if destination is on land)
	# prop.dX <- extract(dXkm.s, prop.LL, cellnumbers=TRUE) # extract X speed and cell # of proposed cell
	# prop.dY <- extract(dYkm.s, prop.LL) # extract Y speed of proposed cell (cell numbers are the same for ex.dX and ex.dY)
	
	# Where necessary, adjust the proposed destination to avoid land via rook-search for warmest/ coolest neighbor
	destCell <- adjDest(
		start.vel=dest.dX, # note that this is the destination velocity from the previous time step (thus, starting velocity)
		start.temp=start.temp, 
		prop.temp=prop.temp, 
		startCell=start.cell, 
		propCell=prop.cell
	)
	# TODO get destination latitude and longitude? Yes, for trajLon and trajLat
	dest.LL <- xyFromCell(start.temp, destCell)
	dest.lon <- xFromCell()
	# dest.lat <- 
		
	# Update dest speeds to reflect those in proposed cell
	# TODO Need to change use destination lat/lon instead of prop.lat/lon
	dest.dX <- setValues(dest.dX, prop.dX[,2])
	dest.dY <- setValues(dest.dY, prop.dY)
	
	# Update the destination LL in the trajectories (not rounded to correspond to cell)
	# TODO change t.X/Y.lon/lat to dest.lon/lat
	trajLon <- setValues(trajLon, t.X.lon, layer=i)
	trajLat <- setValues(trajLat, t.Y.lat, layer=i)
	
	# Extract LL of cell corresponding to proposed location
	# TODO I think new.LL is now pointless, delete?
	new.LL <- xyFromCell(dXkm.s, ex.dX[,1])
	
	
	setTxtProgressBar(sst.pb, i)
}




