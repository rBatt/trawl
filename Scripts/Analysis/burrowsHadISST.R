
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
timeTrend <- stackApply(sst.ann, indices=rep(1,nlayers(sst.ann)), fun=timeSlope)


# ============================
# = Get the spatial gradient =
# ============================
spatGrad.slope <- slope(sst.mu, latlon=TRUE)

sst.mu2 <- sst.mu # TODO Can probably just keep sst.mu and redefine its projection; shouldn't affect other calcs on sst.mu, but might be minor optimization improvement to not (essentially) duplicated the object in memory. If so, should maybe redefine it from start, just to ensure consistency.
crs(sst.mu2) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # The terrain() function requires that the projection be defined. As far as I can tell, the +lon_0 value in the projection doesn't affect the aspect calculation via terrain, so I'm not worrying about it. using the aspect() function was returning a lot of "no slope" values, so I don't want to use it.
spatGrad.aspect <- terrain(sst.mu2, opt="aspect", unit="radians") # direction of spatial gradient


# ===========================
# = Calculate Climate Speed =
# ===========================
climSpeed <- timeTrend/spatGrad.slope # climate speed in km/yr

spdX0 <- climSpeed*sin(spatGrad.aspect) # in km/yr (to the east)
spdY0 <- climSpeed*cos(spatGrad.aspect) # in km/yr (to the north)


# ====================================================
# = Define Initial Values for Trajectory Calculation =
# ====================================================
# Define spatial and temporal resolution of trajectory iteration
n.per.yr <- 3 # number of time steps per year (burrows used 10)
n.per.ll <- 4 # sqrt(number of cells per 1 degree grid cell) (burrows used 10)

n.yrs <- dim(sst.ann)[3] # number of years
step.index <- seq(2, n.yrs*n.per.yr, length.out=n.yrs*n.per.yr-1) # time counter for loop
tYrs <- rep(1:n.yrs, each=n.per.yr) # reference to the year # that lines up with step.index

# Expand rasters to a higher resolution
spdX <- disaggregate(spdX0, n.per.ll) # fine spatial resolution for longitudinal climate speed
spdY <- disaggregate(spdY0, n.per.ll) # fine spatial resolution for latitudinal climate speed
ang <- disaggregate(spatGrad.aspect, n.per.ll) # final spatial resolution for the angle of climate velocity

sst.ann.s0 <- disaggregate(sst.ann, n.per.ll, method="bilinear") # "small" grid size for annual sea surface temperature
sst.ann.s3 <- reclassify(disaggregate(sst.ann, n.per.ll), cbind(-Inf, Inf, 1))
sst.ann.s <- sst.ann.s0*sst.ann.s3 # this is ONLY used for nearest (rook) neighbor searching along coast when proposed trajectory goes to land; OK, actually, it'll be advantageous to use 

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

# Focal weight matrix: this is used by focal.min and focal.max when called within adjDest (faster to define globally than to continually recreate matrix thousands of times)
fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix; called inside focal.min/max


# ====================================================
# = Begin the iterative construction of trajectories =
# ====================================================
sst.pb <- txtProgressBar(min=2, max=max(step.index), style=3)
for(i in step.index){
	t.yr <- tYrs[i]
	
	t.temp <- subset(sst.ann.s, t.yr)
	start.temp <- setValues(t.temp, extract(t.temp, dest.LL))
	
	# Extract the longitude and latitude of starting location
	start.lon <- subset(trajLon, i-1) # longitude of the trajectory at the start of this time step (end of last time step)
	start.lat <- subset(trajLat, i-1) # latitude of the trajectory at the start of this time step (end of last time step)
	start.LL <- cbind(values(start.lon), values(start.lat)) # format starting LL
	start.cell <- setValues(start.temp, cellFromXY(start.temp, start.LL)) # change LL to cell#
		
	# Calculate the longitude and latitude of proposed destination
	prop.lon <- start.lon + dest.dX/111.325*cos(start.lat/180*pi) # calculate the proposed longitude from speeds and starting LL
	prop.lat <- start.lat + dest.dY/111.325 # calculate the proposed latitude from speeds and starting latitude
	prop.LL <- cbind(values(prop.lon), values(prop.lat)) # format proposed LL	
	prop.LL[is.na(values(dest.dX)),] <- cbind(values(start.lon), values(start.lat))[is.na(values(dest.dX)),] # if the velocity is NA, it's not going anywhere; but still need to keep track of the location of the cell.
	
	prop.cell <- setValues(start.temp, cellFromXY(start.temp, prop.LL)) # change LL to cell#; could do prop.temps, but haven't subset yet	
	
	# Extract cell# and X/Y speeds of proposed cell
	prop.temp <- setValues(t.temp, extract(start.temp, prop.LL)) # the temperature in the proposed location (used to determine if destination is on land)
	
	# Where necessary, adjust the proposed destination to avoid land via rook-search for warmest/ coolest neighbor
	dest.cell.LL <- adjDest(
		startVel=dest.dX, # note that this is the destination velocity from the previous time step (thus, starting velocity)
		startTemp=start.temp, 
		propTemp=prop.temp, 
		startCell=start.cell, 
		propCell=prop.cell,
		propLL=prop.LL
	)
	# TODO forgot a detail: the rook neighbor isn't the dest cell; it defines a new angle that the trajectory should move in. To quote: "If a cooler or warmer cell was found then the trajectory was moved along in the direction to that cell (phi) at a speed given by (V/cos(phi-theta)), and limited to a maximum displacement of 1º of latitude or longitude".
	dest.cell <- dest.cell.LL$cell
	dest.LL <- dest.cell.LL$LL
	dest.lon <- dest.LL[,1]
	dest.lat <- dest.LL[,2]
	
	# Update dest speeds to reflect those in proposed cell
	dest.dX <- setValues(dXkm.s, extract(dXkm.s, dest.LL)) # use setValues() to preserve raster class and structure
	dest.dY <- setValues(dXkm.s, extract(dYkm.s, dest.LL)) # note that the 1st object in setValues doesn't matter aside from its extent()
	
	# Update the destination LL in the trajectories (not rounded to correspond to cell)
	trajLon <- setValues(trajLon, dest.lon, layer=i)
	trajLat <- setValues(trajLat, dest.lat, layer=i)
	
	setTxtProgressBar(sst.pb, i)
}


# ===========================
# = Categorize Trajectories =
# ===========================
# TODO Need to categorize trajectories according to Burrows


# ===========================
# = Save Trajectory Results =
# ===========================
# Save a few objects used specifically for a couple figures I made (in plotHadISST.R)
save(sst.mu, spatGrad.aspect, spatGrad.slope, timeTrend, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/HadISST_tempGrads.RData") # TODO I don't want to rewrite this .RData file every time I run this script during testing. What I do with this might depend on how much of this script I turn into separate functions, etc. So for now leaving it here.

# Save the trajectory locations
save(trajLon, trajLat, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/HadISST_trajectories.RData")

# Save the full image
save.image("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/HadISST_trajectoriesImage.RData", compress="xz")
