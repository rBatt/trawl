
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

# heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
# plot(climSpeed0, col=heat.cols)
# plot(spdX0, col=heat.cols)
# plot(spdY0, col=heat.cols)


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






test <- raster(matrix(c(rep(NA,6), 1, 2, NA, 3:9), ncol=4))
test.f <- focal(test, w=fw.mat, which.min) # gets the cell # within the focal matrix

adjDest <- function(start.vel, stop.vel, r.prop){
	# if start.vel is !is.na(start.vel)
	naStart.cell <- as.matrix(Which(is.na(test), cells=TRUE))
	setValues(test, rep(3, length(naStart.cell)), naStart.cell)
	
}

focal.min <- function(r.min){ # where is the smallest value in the rook focus?
	fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	focal(r.min, w=fw.mat, which.min) # focal raster cell# containing smallest value
}

focal.max <- function(r.max){ # where is the biggest value in the rook focus?
	fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	focal(r.max, w=fw.mat, which.max) # focal raster cell# containing biggest value
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
	t.temps <- subset(sst.ann, t.yr)
	
	# Calculate the longitude and latitude of proposed destination
	prop.lon <- subset(trajLon, i-1) + t.dest.dX/111.325*cos(subset(trajLat, i-1))
	prop.lat <- subset(trajLat, i-1) + t.dest.dY/111.325
	
	# Extract cell# and X/Y speeds of proposed cell
	prop.LL <- cbind(values(prop.lon), values(prop.lat)) # format proposed LL
	prop.dX <- extract(dXkm.s, prop.LL, cellnumbers=TRUE) # extract X speed and cell # of proposed cell
	prop.dY <- extract(dYkm.s, prop.LL) # extract Y speed of proposed cell (cell numbers are the same for ex.dX and ex.dY)
	
	# Insert check to make sure not crossing boundary
	# If the destination X velocity is NA, so too is the Y velocity, probably; could probably just check for either
	failProp <- !is.na(dest.dX) & is.na(prop.dX[,2])
	
	
	if(!is.na(dest.dX) & is.na(prop.dX[,2])){ # if moving from cell with non-NA to NA velocity ...
		if(dest.dX>0){ # if the velocity was positive, search for lowest temp non-diagonal neighbor
			# need to get non-diagonal focal temperatures, find the index of the lowest temp
		}else{
			# if the velocity is negative, then find the highest temp
		}
		# need
	}
	
	# Update dest speeds to reflect those in proposed cell
	dest.dX <- setValues(dest.dX, prop.dX[,2])
	dest.dY <- setValues(t.dest.dY, prop.dY)
	
	# Update the destination LL in the trajectories (not rounded to correspond to cell)
	trajLon <- setValues(trajLon, t.X.lon, layer=i)
	trajLat <- setValues(trajLat, t.Y.lat, layer=i)
	
	# Extract LL of cell corresponding to proposed location
	new.LL <- xyFromCell(dXkm.s, ex.dX[,1])
	
	
	setTxtProgressBar(sst.pb, i)
}




# Create a raster for each of lat and lon values

# Create TrajArrive brick w/ # layers = # years
	# 


# Calculate x and y displacements in km

# Convert displacements






