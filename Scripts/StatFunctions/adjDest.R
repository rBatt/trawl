focal2dL <- function(f, dL){
	# f contains the focal rook matches
	# dL contains either latitude or longitude velocities associated with those rook directions
	v4 <- (f==4L)*subset(dL, 1)
	v6 <- (f==6L)*subset(dL, 2)
	v2 <- (f==2L)*subset(dL, 3)
	v8 <- (f==8L)*subset(dL, 4)
	v4+v6+v2+v8
}
# Function to adjust the proposed destination of the trajectory, if it needs it.
adjDest <- function(startVel, startTemp, propTemp, startCell, propCell, propLL){
	# Find coolest and warmest rook neighbors (location and temperature)
	coolFocal <- focal.min(startTemp) # gets the cell# within the focal matrix around startTemp
	cool.dLon <- focal2dL(f=coolFocal, dL=rook.dLon)
	cool.dLat <- focal2dL(f=coolFocal, dL=rook.dLat)
	coolLon <- startLon + cool.dLon
	coolLat <- startLat + cool.dLat
	coolLL <- cbind(values(coolLon), values(coolLat))
	coolCell <- 
	coolCell <- focal2cell(coolFocal) # get convert the cell # in the focal (3x3) raster to the cell# in the full raster
	coolTemp <- setValues(startTemp, extract(startTemp,values(coolCell))) # temp of coolest rook neighbor
	
	warmFocal <- focal.max(startTemp) # gets the cell # within the focal matrix
	warmCell <- focal2cell(warmFocal) # convert focal cell # to full-raster cell#
	warmTemp <- setValues(startTemp, extract(startTemp,values(warmCell))) # temp of warmest rook neighbor
	
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
	stopifnot(is.na(sumBelongs)|sumBelongs<=1) # check to ensure that *at most* 1 adjustment is made to each proposed trajectory
	
	# Rasters can be easily added, and it is easy to define a cell as T/F/NA
	# T/F are interpreted as 1/0
	# A cell should not be TRUE in more than one of the bel___ rasters (see check above)
	# This provides means to do easy conditional replacement/ matching
	# Multiply each category of outcome w/ the appropriate logic, add products
	# This applies the appropriate adjustment to each proprosed trajectory that needs adjusting (values are cell#)
	destCell <- belProp*propCell + belStart*startCell + belCool*coolCell + belWarm*warmCell # checked/adjusted destination cell
	
	destLL <- propLL
	destLL[values(belAdj),] <- xyFromCell(destCell, values(destCell))[values(belAdj),]
	
	return(list(cell=destCell, LL=destLL))
}