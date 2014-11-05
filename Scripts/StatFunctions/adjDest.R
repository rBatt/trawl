# Function to select the right layer from rook-based velocities given a raster containing focal (rook) cell#'s
focal2dL <- function(f, dL){
	# f contains the focal rook matches
	# dL contains either latitude or longitude velocities associated with those rook directions
	v4 <- (f==4L)*subset(dL, 1)
	v6 <- (f==6L)*subset(dL, 2)
	v2 <- (f==2L)*subset(dL, 3)
	v8 <- (f==8L)*subset(dL, 4)
	v4+v6+v2+v8
}

# dest.cell.LL <- adjDest(
# 	startLon=start.lon,
# 	startLat=start.lat,
# 	startCell=start.cell,
# 	startVel=destV, # note that this is the destination velocity from the previous time step (thus, starting velocity)
# 	startTemp=start.temp,
#
# 	propTemp=prop.temp,
# 	propCell=prop.cell,
# 	propLL=prop.LL,
#
# 	rook.dLon=dest.dX.rook/111.325*cos(start.lat/180*pi),
# 	rook.dLat=dest.dY.rook/111.325
# )

dev.new()
par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(spatAng0))
plot(is.finite(sst.mu2)&!is.finite(spatAng.03))
plot(is.finite(sst.mu2)&!is.finite(spatAng.f7))
plot(is.finite(sst.mu2)&!is.finite(spatAng))

par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(spatSlope0))
plot(is.finite(sst.mu2)&!is.finite(spatSlope.03))
plot(is.finite(sst.mu2)&!is.finite(spatSlope.f7))
plot(is.finite(sst.mu2)&!is.finite(spatSlope))

plot(is.finite(sst.mu2)&!is.finite(climV))


par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLon,1)))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLon,2)))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLon,3)))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLon,4)))

par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLat,1)))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLat,2)))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLat,3)))
plot(is.finite(sst.mu2)&!is.finite(subset(rook.dLat,4)))

par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(cool.dLon), main="cool.dLon")
plot(is.finite(sst.mu2)&!is.finite(cool.dLat), main="cool.dLat")
plot(is.finite(sst.mu2)&!is.finite(warm.dLon), main="warm.dLon")
plot(is.finite(sst.mu2)&!is.finite(warm.dLat), main="warm.dLat")

par(mfrow=c(2,2))
plot(is.finite(sst.mu2)&!is.finite(coolLon), main="coolLon")
plot(is.finite(sst.mu2)&!is.finite(coolLat), main="coolLat")
plot(is.finite(sst.mu2)&!is.finite(warmLon), main="warmLon")
plot(is.finite(sst.mu2)&!is.finite(warmLat), main="warmLat")


par(mfrow=c(2,2))
plot(is.finite(startVel)&!is.finite(coolLon), main="coolLon")
plot(is.finite(startVel)&!is.finite(coolLat), main="coolLat")
plot(is.finite(startVel)&!is.finite(warmLon), main="warmLon")
plot(is.finite(startVel)&!is.finite(warmLat), main="warmLat")

par(mfrow=c(2,2))
plot(is.finite(startLon)&!is.finite(coolLon), main="coolLon")
plot(is.finite(startLat)&!is.finite(coolLat), main="coolLat")
plot(is.finite(startLon)&!is.finite(warmLon), main="warmLon")
plot(is.finite(startLat)&!is.finite(warmLat), main="warmLat")


# Function to adjust the proposed destination of the trajectory, if it needs it.
adjDest <- function(startLon, startLat, startCell, startVel, startTemp, propTemp, propCell, propLL, rook.dLon, rook.dLat){
	# Find coolest and warmest rook neighbors (location and temperature)
	coolFocal <- focal.min(startTemp) # gets the cell# within the focal matrix around startTemp
	cool.dLon <- focal2dL(f=coolFocal, dL=rook.dLon)
	cool.dLat <- focal2dL(f=coolFocal, dL=rook.dLat)
	coolLon <- startLon + cool.dLon
	coolLat <- startLat + cool.dLat
	coolLL <- cbind(values(coolLon), values(coolLat))
	coolCell <- cellFromXY(startTemp, coolLL)
	coolTemp <- setValues(startTemp, extract(startTemp,coolCell)) # temp of coolest rook neighbor
	
	warmFocal <- focal.max(startTemp) # gets the cell# within the focal matrix around startTemp
	warm.dLon <- focal2dL(f=warmFocal, dL=rook.dLon)
	warm.dLat <- focal2dL(f=warmFocal, dL=rook.dLat)
	warmLon <- startLon + warm.dLon
	warmLat <- startLat + warm.dLat
	warmLL <- cbind(values(warmLon), values(warmLat))
	warmCell <- cellFromXY(startTemp, warmLL)
	warmTemp <- setValues(startTemp, extract(startTemp,warmCell)) # temp of warmest rook neighbor
	
	
	posVel <- is.finite(startVel)&startVel>0
	negVel <- is.finite(startVel)&startVel<0
	
	# Define logical rasters associated with each possible categorical outcome (to which category does each cell belong?)
	belAdj <- !is.finite(propTemp) & is.finite(startTemp) # logical: does the destination need to be adjusted?
	belProp <- !belAdj # logical: is the proposed trajectory OK?
	belCool <- belAdj & posVel & coolTemp<startTemp # logical: should+can we reject the prop & find cooler neighbor?
	belWarm <- belAdj & negVel & warmTemp>startTemp # logical: should+can we reject the prop & find warmer neighbor?
	belStart <- belAdj & !(belCool|belWarm) # logical: needs adj, but can't find warmer/cooler rook? (this is coastal sink)
	
	# A cell should not be TRUE in more than one of the bel___ rasters; if it does, my logic is flawed
	sumBelongs <- values(belProp+belCool+belWarm+belStart) # the number adjustment outcomes to which each cell belongs
	stopifnot(all(is.na(sumBelongs)|sumBelongs<=1)) # check to ensure that *at most* 1 adjustment is made to each proposed trajectory
	
	# Rasters can be easily added, and it is easy to define a cell as T/F/NA
	# T/F are interpreted as 1/0
	# A cell should not be TRUE in more than one of the bel___ rasters (see check above)
	# This provides means to do easy conditional replacement/ matching
	# Multiply each category of outcome w/ the appropriate logic, add products
	# This applies the appropriate adjustment to each proprosed trajectory that needs adjusting (values are cell#)
	# destCell <- belProp*propCell + belStart*startCell + belCool*coolCell + belWarm*warmCell # checked/adjusted destination cell
	
	destLL <- propLL
	destLL[values(belCool),] <- coolLL[values(belCool),]
	destLL[values(belWarm),] <- warmLL[values(belCool),]
	destLL[values(belStart),] <- cbind(values(startLon), values(startLat))[values(belCool),]
	
	# destLL[values(belAdj),] <- xyFromCell(destCell, values(destCell))[values(belAdj),]
	
	# return(list(cell=destCell, LL=destLL))
	return(destLL)
}