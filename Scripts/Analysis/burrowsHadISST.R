
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
spatGrad.aspect <- aspect(sst.mu, latlon=FALSE)
reclassify(spatGrad.aspect, cbind(-1, NA))


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

t.dest.dX <- dXkm.s # the X speed in the previous location 
t.dest.dY <- dYkm.s # they Y speed in the previous location

sst.pb <- txtProgressBar(min=2, max=max(step.index), style=3)
for(i in step.index){
	
	# Calculate the longitude and latitude of proposed destination
	t.X.lon <- subset(trajLon, i-1) + t.dest.dX/111.325*cos(subset(trajLat, i-1))
	t.Y.lat <- subset(trajLat, i-1) + t.dest.dY/111.325
	
	# Extract cexll# and X/Y speeds of proposed cell
	t.LL <- cbind(values(t.X.lon), values(t.Y.lat)) # format proposed LL
	ex.dX <- extract(dXkm.s, t.LL, cellnumbers=TRUE) # extract X speed and cell # of cell
	ex.dY <- extract(dYkm.s, t.LL) # extract Y speed of proposed cell (cell numbers are the same for ex.dX and ex.dY)
	
	# Insert check to make sure not crossing boundary
	# If the destination X velocity is NA, so too is the Y velocity, probably; could probably just check for either
	if(!is.na(t.dest.dX) & is.na(ex.dX[,2])){ # if moving from cell with non-NA to NA velocity ...
		if(t.dest.dX>0){ # if the velocity was positive, search for lowest temp non-diagonal neighbor
			# need to get non-diagonal focal temperatures, find the index of the lowest temp
		}else{
			# if the velocity is negative, then find the highest temp
		}
		# need
	}
	
	# Update dest speeds to reflect those in proposed cell
	t.dest.dX <- setValues(t.dest.dX, ex.dX[,2])
	t.dest.dY <- setValues(t.dest.dY, ex.dY)
	
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






