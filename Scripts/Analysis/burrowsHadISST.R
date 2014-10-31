
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
# spatGrad.aspect <- aspect(sst.mu+runif(n=length(sst.mu), -0.05, 0.05), latlon=FALSE)
# spatGrad.aspect <- reclassify(spatGrad.aspect, cbind(-1, NA))

sst.mu2 <- sst.mu
crs(sst.mu2) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # as far as I can tell, the +lon_0 value in the projection doesn't affect the aspect calculation via terrain, so I'm not worrying about it. using the aspect() function was returning a lot of "no slope" values, so I don't want to use it.
spatGrad.aspect <- terrain(sst.mu2, opt="aspect", unit="degrees")

save(sst.mu, spatGrad.aspect, spatGrad.slope, timeTrend, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/HadISST_tempGrads.RData")

# ===========================
# = Calculate Climate Speed =
# ===========================
climSpeed <- timeTrend/spatGrad.slope # climate speed in km/yr

spdX0 <- climSpeed*sin(spatGrad.aspect) # in km/yr (to the east)
spdY0 <- climSpeed*cos(spatGrad.aspect) # in km/yr (to the north)


# ==========================
# = Calculate Trajectories =
# ==========================
n.per.yr <- 2
n.per.ll <- 3

n.yrs <- dim(sst.ann)[3] # number of years
step.index <- seq(2, n.yrs*n.per.yr, length.out=n.yrs*n.per.yr-1) # time counter for loop
tYrs <- rep(1:n.yrs, each=n.per.yr) # reference to the year # that lines up with step.index

# Expand rasters
spdX <- disaggregate(spdX0, 3) # fine spatial resolution for longitudinal climate speed
spdY <- disaggregate(spdY0, 3) # fine spatial resolution for latitudinal climate speed
ang <- disaggregate(spatGrad.aspect, 3) # final spatial resolution for the angle of climate velocity

# Create empty bricks to hold trajectory lon/ lat at each time step
trajLon <- brick(array(NA, dim=dim(sst.ann)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=-190, xmx=-40, ymn=20, ymx=65)
lons <- seq(xmin(trajLon), xmax(trajLon), length.out=ncol(trajLon))
trajLon <- setValues(trajLon, rep(lons, nrow(trajLon)), layer=1)

trajLat <- brick(array(NA, dim=dim(sst.ann)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=-190, xmx=-40, ymn=20, ymx=65)
lats <- seq(ymax(trajLat), ymin(trajLat), length.out=ncol(trajLat))
trajLat <- setValues(trajLat, rep(lats, each=nrow(trajLat)), layer=1)

dXkm.s <- spdX*(1/n.per.yr) # "small" (per-time-step) change in X
dYkm.s <- spdY*(1/n.per.yr) # "small" (per-time-step) change in Y

# Initial values for velocities
dest.dX <- dXkm.s # the X speed in the previous location (updated at each time step)
dest.dY <- dYkm.s # they Y speed in the previous location


# ===========
# = Testing =
# ===========
test <- raster(matrix(c(rep(NA,6), 1, 2, NA, 3:9), ncol=4))
test.dest <- raster(matrix(c(rep(NA,6), NA, 2, NA, NA, 4:9), ncol=4))
fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix

coolFocal <- focal(test, w=fw.mat, which.min, pad=TRUE) # gets the cell # within the focal matrix
coolNeigh.cell <- focal2cell(coolFocal)
coolNeigh <- matrix(extract(test,c(as.matrix(coolNeigh.cell))), ncol=ncol(test))

warmFocal <- focal(test, w=fw.mat, which.max, pad=TRUE) # gets the cell # within the focal matrix
warmNeigh.cell <- focal2cell(warmFocal)
warmNeigh <- matrix(extract(test,c(as.matrix(warmNeigh.cell))), ncol=ncol(test))


ctest <- raster(matrix(c(rep(NA,6), 1, 2, NA, 1, rep(1:2, 3)), ncol=4))
r1 <- raster(matrix(rep(10, 16), ncol=4))
r2 <- raster(matrix(rep(-33, 16), ncol=4))
ctest.refLayers <- stack(r1, r2)

belongs1 <- ctest==1
belongs2 <- ctest==2

ct.comb <- subset(ctest.refLayers,1)*belongs1 + subset(ctest.refLayers,2)*belongs2


# ===============
# = End Testing =
# ===============

# Function to adjust the proposed destination of the trajectory, if it needs it.
adjDest <- function(start.vel, start.temp, prop.temp, startCell, propCell){
	
	# Find coolest and warmest rook neighbors (location and temperature)
	coolFocal <- focal(start.temp, w=fw.mat, which.min, pad=TRUE) # gets the cell # within the focal matrix around start.temp
	coolCell <- focal2cell(coolFocal)
	coolTemp <- matrix(extract(start.temp,c(as.matrix(coolCell))), ncol=ncol(start.temp)) # temp of coolest rook neighbor
	
	warmFocal <- focal(start.temp, w=fw.mat, which.max, pad=TRUE) # gets the cell # within the focal matrix
	warmCell <- focal2cell(warmFocal)
	warmTemp <- matrix(extract(start.temp,c(as.matrix(warmCell))), ncol=ncol(start.temp)) # temp of warmest rook neighbor
	
	# Define logical rasters associated with each possible categorical outcome (to which category does each cell belong?)
	belAdj <- is.na(prop.temp) & !is.na(start.temp) # logical: does the destination need to be adjusted?
	belProp <- !belAdj # logical: is the proposed trajectory OK?
	belCool <- belAdj & start.vel<0 & coolTemp<start.temp # logical: should+can we reject the prop & find cooler neighbor?
	belWarm <- belAdj & start.vel>0 & warmTemp>start.temp # logical: should+can we reject the prop & find warmer neighbor?
	belStart <- belAdj & !(belCool|belWarm) # logical: does it need adjusting, but can't/shouldn't find a warmer or cooler rook?
	
	destCell <- belProp*propCell + belStart*startCell + belCool*coolCell + belWarm*warmCell
	
	return(destCell)
	
	
}

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
	f.conv <- reclassify(f, cbind(c(4,6,2,8),c(-1,1,-f.ncol,f.ncol))) # the conversion between focal cell# and raster cell#
	f.cell+f.conv # convert
}




sst.pb <- txtProgressBar(min=2, max=max(step.index), style=3)
for(i in step.index){
	t.yr <- tYrs[i]
	t.temps <- subset(sst.ann, t.yr) # TODO need to change this to not reference the origin of the trajectory, but to reference the updated location of the trajectory. So do this, but wrap in the LL determined after adjDest (would be prop.LL if the proposed location is OK)
	
	# Extract the longitude and latitude of starting location
	start.lon <- subset(trajLon, i-1) # longitude of the trajectory at the start of this time step (end of last time step)
	start.lat <- subset(trajLat, i-1) # latitude of the trajectory at the start of this time step (end of last time step)
	start.LL <- cbind(values(start.lon), values(start.lat)) # format starting LL
	start.cell <- cellFromXY(t.temps, start.LL) # change LL to cell#
	
	# Calculate the longitude and latitude of proposed destination
	prop.lon <- start.lon + dest.dX/111.325*cos(start.lat) # calculate the proposed longitude from speeds and starting LL
	prop.lat <- start.lat + dest.dY/111.325 # calculate the proposed latitude from speeds and starting latitude
	prop.LL <- cbind(values(prop.lon), values(prop.lat)) # format proposed LL
	prop.cell <- cellFromXY(t.temps, prop.LL) # change LL to cell#; could do prop.temps, but haven't subset yet
	
	# Extract cell# and X/Y speeds of proposed cell
	prop.temps <- extract(t.temps, prop.LL) # the temperature in the proposed location (used mainly to determine if )
	prop.dX <- extract(dXkm.s, prop.LL, cellnumbers=TRUE) # extract X speed and cell # of proposed cell
	prop.dY <- extract(dYkm.s, prop.LL) # extract Y speed of proposed cell (cell numbers are the same for ex.dX and ex.dY)
	
	# Insert check to make sure not crossing boundary
	destCell <- adjDest(
		start.vel=dest.dX, 
		start.temp=t.temps, 
		prop.temp=prop.temps, 
		startCell=start.cell, 
		propCell=prop.cell
	)
	
	# Update dest speeds to reflect those in proposed cell
	dest.dX <- setValues(dest.dX, prop.dX[,2])
	dest.dY <- setValues(dest.dY, prop.dY)
	
	# Update the destination LL in the trajectories (not rounded to correspond to cell)
	trajLon <- setValues(trajLon, t.X.lon, layer=i)
	trajLat <- setValues(trajLat, t.Y.lat, layer=i)
	
	# Extract LL of cell corresponding to proposed location
	new.LL <- xyFromCell(dXkm.s, ex.dX[,1])
	
	
	setTxtProgressBar(sst.pb, i)
}




