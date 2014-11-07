# Function to select the right layer from rook-based velocities given a raster containing focal (rook) cell#'s
focal2dL <- function(f, dL){
	# f contains the focal rook matches
	# dL contains either latitude or longitude velocities associated with those rook directions
	# If none of those matches, the value of the sum at the end will be 0; so works for NA's and for focal 5 (b/c 5 would mean 0 velocity and the NA's don't matter, but 0 works)
	v4 <- (f==4L)*subset(dL, 1)
	v6 <- (f==6L)*subset(dL, 2)
	v2 <- (f==2L)*subset(dL, 3)
	v8 <- (f==8L)*subset(dL, 4)
	# v4+v6+v2+v8
	 abs(v6) + abs(v2) + -1*abs(v8) + -1*abs(v4)
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


# Function to adjust the proposed destination of the trajectory, if it needs it.
adjDest <- function(startLon, startLat, startCell, startVel, startTemp, propTemp, propCell, propLL, rook.dLon, rook.dLat){

	# ===============================
	# = Find coolest rook neighbors =
	# ===============================
	# Find the neighbors
	coolFocal <- focal.min(startTemp) # gets the cell# within the focal matrix around startTemp
	# coolMin <- getF.min(startTemp)
	cool.dLon <- focal2dL(f=coolFocal, dL=rook.dLon) # change neighbor location into a velocity in the X direction
	cool.dLat <- focal2dL(f=coolFocal, dL=rook.dLat) # same, but in the Y direction
	coolLon <- startLon + cool.dLon # add the X velocity to the starting X location
	coolLat <- startLat + cool.dLat # same, but Y
	coolLL <- cbind(values(coolLon), values(coolLat)) # combine the new lon/lats into a matrix
	coolCell <- cellFromXY(startTemp, coolLL) # use the lon/lat matrix of new location to get new cell#'s
	coolTemp <- setValues(startTemp, extract(startTemp,coolCell)) # use new cell#'s to get new temperatures (of coolest neighbor)
	
	
	cProb <- values(is.na(coolTemp)&!is.na(startTemp))
	cProb.cell <- coolCell[cProb]
	extract(coolFocal, cProb.cell)
	extract(coolMin, cProb.cell)
	extract(startTemp, cProb.cell)


	par(mfrow=c(2,2))
	plot(is.finite(startLon)&(coolLon>xmax(startLon) | coolLon<xmin(startLon)), main="coolLon")
	plot(is.finite(startLat)&(coolLat>ymax(startLat) | coolLat<ymin(startLat)), main="coolLat")
	plot(is.finite(startTemp)&is.na(coolTemp), main="cool Temp")

	 minLon <- which.min(coolLL[,1])
	
	table(values(coolFocal)[values(coolFocal)>1&values(coolFocal)<9])
	
	# Fix bad cools by selecting center of chosen rook
	# I cannot figure out why some of these destinations are NA
	# However, when making the correction, the correct cell is always 301  cells later; in the resolution I'm using this is the row below the problem cell, then 1 column to the right
	# So this is basically converting a 2 to a 6, or a 4 to an 8
	# These are possibly cases where something was added to the latitude (choosing a 2) instead of longitude (meant 6?)
	coolProblem <- is.na(coolTemp)&!is.na(startTemp)
	if(any(values(coolProblem))){ # if there any problematic cool destinations, then ...
		coolCorrectCell <- focal2cell(coolFocal)[coolProblem] # get the new destination cell # from the rook focal cell #
		coolCell[values(coolProblem)] <- coolCorrectCell # replace the old cool cell#'s with the corrected ones
		coolTemp <- setValues(startTemp, extract(startTemp,coolCell)) # update temperatures to corrected rook location
		coolCorrectLL <- xyFromCell(coolTemp, coolCorrectCell) # get corrected rook lon/lat (this is the lon/lat at the center of the rook)
		coolLL[values(coolProblem),] <- coolCorrectLL # update the lon/lat to corrected values
	}
	
	extract(startLon, coolCorrectCell)
	extract(cool.dLon, coolCorrectCell)
	extract(startLat, coolCorrectCell)
	extract(cool.dLat, coolCorrectCell)
	
	extract(startLon, cProb.cell)
	extract(cool.dLon, cProb.cell)
	extract(startLat, cProb.cell)
	extract(cool.dLat, cProb.cell)
	
	blah <- cbind(
		coolFocal=extract(coolFocal, cProb.cell),
		
		startLon=extract(startLon, cProb.cell),
		cool.dLon=extract(cool.dLon, cProb.cell),
		coolLon=extract(coolLon, cProb.cell),
		correctLon=extract(startLon, coolCorrectCell),
	
		startLat=extract(startLat, cProb.cell),
		cool.dLat=extract(cool.dLat, cProb.cell),
		coolLat=extract(coolLat, cProb.cell),
		correctLat=extract(startLat, coolCorrectCell)
	)
	
	extract(startTemp, cellFromXY(startTemp, blah[,c("startLon","startLat")]))
	extract(startTemp, cellFromXY(startTemp, blah[,c("coolLon","coolLat")]))
	extract(startTemp, cellFromXY(startTemp, blah[,c("correctLon","correctLat")]))
	
	
	# ================================
	# = Find warming rooks neighbors =
	# ================================
	# see cool for detailed notes
	# find warm neighbors
	warmFocal <- focal.max(startTemp) # gets the cell# within the focal matrix around startTemp
	warm.dLon <- focal2dL(f=warmFocal, dL=rook.dLon)
	warm.dLat <- focal2dL(f=warmFocal, dL=rook.dLat)
	warmLon <- startLon + warm.dLon
	warmLat <- startLat + warm.dLat
	warmLL <- cbind(values(warmLon), values(warmLat))
	warmCell <- cellFromXY(startTemp, warmLL)
	warmTemp <- setValues(startTemp, extract(startTemp,warmCell)) # temp of warmest rook neighbor
	
	
	# par(mfrow=c(2,2))
	plot(is.finite(startLon)&(warmLon>xmax(startLon) | warmLon<xmin(startLon)), main="coolLon")
	plot(is.finite(startLat)&(warmLat>ymax(startLat) | warmLat<ymin(startLat)), main="coolLat")
	
	
	# Fix bad warms by selecting center of chosen rook
	# warmProblem <- is.na(warmTemp)&!is.na(startTemp)
# 	if(any(values(warmProblem))){
# 		warmCorrectCell <- focal2cell(warmFocal)[warmProblem]
# 		warmCell[values(warmProblem)] <- warmCorrectCell
# 		warmTemp <- setValues(startTemp, extract(startTemp,warmCell))
# 		warmCorrectLL <- xyFromCell(warmTemp, warmCorrectCell)
# 		warmLL[values(warmProblem),] <- warmCorrectLL
# 	}
#
	# ===============================================
	# = Logic for correcting a proposed destination =
	# ===============================================
	posVel <- is.finite(startVel)&startVel>0
	negVel <- is.finite(startVel)&startVel<0
	
	# Define logical rasters associated with each possible categorical outcome (to which category does each cell belong?)
	belAdj <- !is.finite(propTemp) & is.finite(startTemp) # logical: does the destination need to be adjusted?
	belProp <- !belAdj # logical: is the proposed trajectory OK?
	belCool <- belAdj & posVel & coolTemp<startTemp # logical: should+can we reject the prop & find cooler neighbor?
	belWarm <- belAdj & negVel & warmTemp>startTemp # logical: should+can we reject the prop & find warmer neighbor?
	belStart <- belAdj & !(belCool|belWarm) # logical: needs adj, but can't find warmer/cooler rook? (this is coastal sink)
	
	# =================
	# = A quick check =
	# =================
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
	
	# ================================================
	# = OK, get the (corrected) destination lon/ lat =
	# ================================================
	destLL <- propLL
	destLL[values(belCool),] <- coolLL[values(belCool),]
	destLL[values(belWarm),] <- warmLL[values(belCool),]
	destLL[values(belStart),] <- cbind(values(startLon), values(startLat))[values(belStart),]
	
	
	# ===============================
	# = Return destination lon/ lat =
	# ===============================
	return(destLL)
}