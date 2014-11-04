
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


# ================================
# = Define Trajectory Resolution =
# ================================
# Define spatial and temporal resolution of trajectory iteration
n.per.yr <- 2 # number of time steps per year (burrows used 10)
n.per.ll <- 2 # sqrt(number of cells per 1 degree grid cell) (burrows used 10)

n.yrs <- dim(sst.ann)[3] # number of years
step.index <- seq(2, n.yrs*n.per.yr, length.out=n.yrs*n.per.yr-1) # time counter for loop
tYrs <- rep(1:n.yrs, each=n.per.yr) # reference to the year # that lines up with step.index


# ==================================================
# = High Res & Correct Projection for Temperatures =
# ==================================================
# Expand sst to higher resolution, while avoiding extra NA's and repeated values in the rook
sst.ann.s0 <- disaggregate(sst.ann, n.per.ll, method="bilinear") # "small" grid size for annual sea surface temperature
sst.ann.s3 <- reclassify(disaggregate(sst.ann, n.per.ll), cbind(-Inf, Inf, 1))
sst.ann <- sst.ann.s0*sst.ann.s3 # redefining sst.ann, now want to use this for everything

sst.mu0 <- stackApply(sst.ann.s0, indices=rep(1, length(sst.ann)), fun=mean)
sst.mu2 <- stackApply(sst.ann, indices=rep(1, length(sst.ann)), fun=mean)
crs(sst.mu) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # need projection for terrain()
crs(sst.mu0) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # need projection for terrain()
crs(sst.mu2) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84" # need projection for terrain()


# ==================================
# = Get the temporal trend for SST =
# ==================================
# TODO It seems like the temporal trend should be calculated at a finer temporal scale, and that there shouldn't be just 1 climate velocity for the entire time period. I will certainly need to do this calculation for different starting times, as the trawl time series start at different times.
timeTrend <- stackApply(sst.ann, indices=rep(1,nlayers(sst.ann)), fun=timeSlope)


# =========================
# = Get the spatial slope =
# =========================
# Get the spatial gradient from the original course sst, then disaggregate; this is the simplest form
spatSlope0 <- disaggregate(slope(sst.mu, latlon=TRUE), n.per.ll) # spatial gradient, then disaggregate
spatSlope0[is.nan(spatSlope0)] <- NA # turn NaN's to NA's

# Problem: above spatial gradient has NA's where there are SST's
# Solution: spatial averaging to fill in gaps
spatSlope.f <- slope(sst.mu0, latlon=TRUE) # spatial slopes taken from a fine spatial resolution
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

# Plot to show where we had problematic NA's from each of the spatial slope rasters
par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(spatSlope0))
plot(is.finite(sst.mu2)&!is.finite(spatSlope.03))
plot(is.finite(sst.mu2)&!is.finite(spatSlope.f7))
plot(is.finite(sst.mu2)&!is.finite(spatSlope))


# ===============================
# = Calcualte the spatial angle =
# ===============================
# Calculate the angle of the spatial slope
spatAng0 <- disaggregate(terrain(sst.mu, opt="aspect", unit="radians"), n.per.ll) # direction of spatial gradient
spatAng0[is.nan(spatAng0)] <- NA

# Problem: above spatial gradient has NA's where there are SST's
# Solution: spatial averaging to fill in gaps
# Note!: Can't just do the average of the angles, like I did with the slopes, because you run into circular problems.
# Instead I'm doing the spatial averaging of the water temperatures, then taking those angles
spatAng.03 <- terrain(disaggregate(sloFill(sst.mu, 3, 3), n.per.ll), opt="aspect", unit="radians") # 
spatAng.f7 <- sloFill(terrain(sst.mu0, opt="aspect", unit="radians"), 7, 7) # to fill in the remaining NA's, do a spatial averaging of the spatial slopes that were calculated from the bilinearly interpolated sst's (to be used most sparingly b/c it's on the largest grid, and b/c it required initial bilinear interpolation before slope was even calculated)

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

par(mfrow=c(2,2))
plot(spatAng0)
plot(spatAng.03)
plot(spatAng.f7)
plot(spatAng))


par(mfrow=c(2,2))
plot(is.finite(spatSlope)&!is.finite(spatAng0))
plot(is.finite(spatSlope)&!is.finite(spatAng.03))
plot(is.finite(spatSlope)&!is.finite(spatAng.f7))
plot(is.finite(spatSlope)&!is.finite(spatAng))


#
#
# # Calculate the angle of the spatial slope
# spatAng0 <- disaggregate(terrain(sst.mu, opt="aspect", unit="radians"), n.per.ll) # direction of spatial gradient
# spatAng0[is.nan(spatAng0)] <- NA
#
# # Problem: above spatial gradient has NA's where there are SST's
# # Solution: spatial averaging to fill in gaps
# spatAng.f <- terrain(sst.mu0, opt="aspect", unit="radians") # spatial angles taken from a fine spatial resolution
# spatAng.03 <- angFill(spatAng0, 3, 3) # do a spatial average of simple slope to get slopes for most of the problematic NA's in spatAng0
# spatAng.f7 <- angFill(spatAng.f, 7, 7) # to fill in the remaining NA's, do a spatial averaging of the spatial slopes that were calculated from the bilinearly interpolated sst's (to be used most sparingly b/c it's on the largest grid, and b/c it required initial bilinear interpolation before slope was even calculated)
#
# # Set up Logic for what's NA in each of the slope rasters, as well as what's NA in the sst raster
# sst2NA <- !is.finite(sst.mu2)
# sa0NA <- !is.finite(spatAng0)
# sa03NA <- !is.finite(spatAng.03)
# saf7NA <- !is.finite(spatAng.f7)
#
# # Set up logic for which slope raster to use
# pick.sa0 <- !sa0NA & !sst2NA
# pick.sa03 <- sa0NA & !sst2NA & !sa03NA
# pick.saf7 <- sa0NA & !sst2NA & sa03NA & !saf7NA
#
# # Annoying: 0*NA = NA, so have to change NA's to 0's before adding up slopes (adding is a way of adding values to a subset)
# sa0.vals <- spatAng0
# sa0.vals[is.na(sa0.vals)] <- 0
# sa0.vals <- sa0.vals*pick.sa0
#
# sa03.vals <- spatAng.03
# sa03.vals[is.na(sa03.vals)] <- 0
# sa03.vals <- sa03.vals*pick.sa03
#
# saf7.vals <- spatAng.f7
# saf7.vals[is.na(saf7.vals)] <- 0
# saf7.vals <- saf7.vals*pick.saf7
#
# # Now tally up the final spatial gradient
# spatAng <- sa0.vals + sa03.vals + saf7.vals + sst.mu2*(sst2NA) # last term makes sure we didn't average-in velocities for places that we don't even have temperature


# ===========================
# = Calculate Climate Speed =
# ===========================
# Climate velocity and its angle, high resolution
climV <- (timeTrend/spatGrad.slope)*(1/n.per.yr) #disaggregate(timeTrend/spatGrad.slope, n.per.ll)*(1/n.per.yr)  # climate speed in km/yr
ang <- disaggregate(spatGrad.aspect, n.per.ll) # final spatial resolution for the angle of climate velocity

# Calculate X and Y velocities
dXkm <- climV*sin(ang) # the X speed (km/yr to the east)
dYkm <- climV*cos(ang) # the Y speed (km/yr to the north)


# ============================
# = Get full lon, lat, cell# =
# ============================
lons <- setValues(ang, rep(seq(xmin(ang), xmax(ang), length.out=ncol(ang)), nrow(ang)))
lats <- setValues(ang, rep(seq(ymax(ang), ymin(ang), length.out=ncol(ang)), each=nrow(ang)))


# ===================================
# = Get the rook velocities, angles =
# ===================================
rookV <- stack(adjV(4), adjV(6), adjV(2), adjV(8)) # these are the velocities for each of the 4 possible directions a trajectory can go when the calculated velocity would make it go from sea to land; which of the 4 directions chosen depends on the sign of the velocity, 
rookAng <- stack(adjAng(4), adjAng(6), adjAng(2), adjAng(8)) # the angle, in radians, for the rook directions
# cellNum <- setValues(ang, 1:length(ang))


# ==============================================
# = Calculate X&Y rook velocities, limit to 1º =
# ==============================================
dXkm.rook0 <- rookV*sin(rookAng)
dYkm.rook0 <- rookV*cos(rookAng)

conv.fact.lon.init <- 111.325*cos(lats/180*pi) # this value is used inside limitV(), but is defined here to reduce computation time
dXkm.rook <- limitV(dXkm.rook0, dir="lon", conv.fact.lon=conv.fact.lon.init)
dYkm.rook <- limitV(dYkm.rook0, dir="lat")


# ====================================================
# = Define Initial Values for Trajectory Calculation =
# ====================================================
# Set initial values for destination velocities and locations
destV <- climV # TODO delete? is this needed? maybe was only needed if I wasn't going to calculate all the rook velocities ahead of time
# TODO O wait, destV might be needed b/c I'm not sure if I'm comfortable testing for the sign of velocity using dest.dX in adjDest(); should probably use destV. But I still don't think I need destAng.
destAng <- ang # TODO delete? is this needed? same reasoning as for destV
dest.dX <- dXkm # the X speed in the previous destination location (updated at the end of each time step)
dest.dY <- dYkm # the Y speed in the previous destination location
dest.dX.rook <- dXkm.rook
dest.dY.rook <- dYkm.rook
dest.LL <- cbind(values(lons), values(lats)) # same as starting LL, but will be updated each iteration after adjDest



# TODO I'm running into a problem where the coastal velocities are NA b/c the slopes there aren't defined; but temperature trajectories start there, and this is the place where they run into the coast
# sum(is.na(values(climV))&!is.na(values(subset(sst.ann.s,1))))
# plot(is.na(climV)&!is.na(subset(sst.ann.s,1)))
plot(is.na(spatGrad.slope02)&!is.na(subset(sst.ann.s,1)))

# Create empty bricks to hold trajectory lon/ lat at each time step
trajLon <- brick(array(NA, dim=dim(sst.ann)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=-190, xmx=-40, ymn=20, ymx=65) # empty lon brick
trajLon <- setValues(trajLon, values(lons), layer=1) # update first year (layer) of brick to give starting lon

trajLat <- brick(array(NA, dim=dim(sst.ann)*c(n.per.ll,n.per.ll,n.per.yr)), xmn=-190, xmx=-40, ymn=20, ymx=65) # empty lat brick
trajLat <- setValues(trajLat, values(lats), layer=1) # update first year (layer) of brick to give starting lat

# Focal weight matrix: this is used by focal.min and focal.max when called within adjDest (faster to define globally than to continually recreate matrix thousands of times)
fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix; called inside focal.min/max()


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
	start.conv.factor.lon <- 111.325*cos(lats/180*pi) # used in limitV()
		
	# Calculate the longitude and latitude of proposed destination
	# TODO with new approach to rook, adjDest() just needs to add rook velocities instead of dest.dX or dest.dY
	# TODO need to limit velocities before I do adjDest; NO, because Burrows just uses small time step to minimize this effect, otherwise could just limit velocities from the start, definitely don't need to do this every iteration
	prop.dLon <- dest.dX/111.325*cos(start.lat/180*pi)
	prop.dLat <- dest.dY/111.325
	prop.lon <- start.lon + prop.dLon # calculate the proposed longitude from speeds and starting LL
	prop.lat <- start.lat + prop.dLat # calculate the proposed latitude from speeds and starting latitude
	prop.LL <- cbind(values(prop.lon), values(prop.lat)) # format proposed LL	
	prop.LL[is.na(values(dest.dX)),] <- cbind(values(start.lon), values(start.lat))[is.na(values(dest.dX)),] # if the velocity is NA, it's not going anywhere; but still need to keep track of the location of the cell.
	
	prop.cell <- setValues(start.temp, cellFromXY(start.temp, prop.LL)) # change LL to cell#; could do prop.temps, but haven't subset yet	
	
	# Extract cell# and X/Y speeds of proposed cell
	prop.temp <- setValues(t.temp, extract(start.temp, prop.LL)) # the temperature in the proposed location (used to determine if destination is on land)
	
	# Where necessary, adjust the proposed destination to avoid land via rook-search for warmest/ coolest neighbor
	dest.cell.LL <- adjDest(
		startLon=start.lon,
		startLat=start.lat,
		startCell=start.cell, 
		startVel=destV, # note that this is the destination velocity from the previous time step (thus, starting velocity)
		startTemp=start.temp, 
		
		propTemp=prop.temp, 
		propCell=prop.cell,
		propLL=prop.LL, 
		
		rook.dLon=dest.dX.rook/111.325*cos(start.lat/180*pi),
		rook.dLat=dest.dY.rook/111.325
	)
	# TODO forgot a detail: the rook neighbor isn't the dest cell; it defines a new angle that the trajectory should move in. To quote: "If a cooler or warmer cell was found then the trajectory was moved along in the direction to that cell (phi) at a speed given by (V/cos(phi-theta)), and limited to a maximum displacement of 1º of latitude or longitude".
	dest.cell <- dest.cell.LL$cell
	dest.LL <- dest.cell.LL$LL
	dest.lon <- dest.LL[,1]
	dest.lat <- dest.LL[,2]
	
	# Update dest speeds to reflect those in proposed cell
	dest.dX <- setValues(dXkm, extract(dXkm, dest.LL)) # use setValues() to preserve raster class and structure
	dest.dY <- setValues(dXkm, extract(dYkm, dest.LL)) # note that the 1st object in setValues doesn't matter aside from its extent()
	# TODO Update these destinations!
	dest.dX.rook
	dest.dY.rook
	destV
	
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
