
# "If a cooler or warmer cellw as found then the trajectory was moved along in the direction of that cell (phi) at a speed given by V/cos(phi-theta)" (Burrows et al. 2014 Nature, 'Calculating trajectories' in Methods, 3rd from last sentence).
convV <- function(speed.km, rook, lat, n.per.ll){
	
	limit <- 1/(n.per.ll^2+0.1) # this is the size of a cell in degrees lat or lon; limit to rook (adjusted if bad proposal) velocity
	cr <- as.character(rook) # convert rook integer to character for subsetting
	
	d46 <- speed.km/(111.325*cos(lat/180*pi)) # convert climate speeds in km/timeStep to climate speeds in degrees longitude/timeStep
	dLon <- rep(NA, length(rook)) # create an empty vector for storing longitudinal changes
	dLon[cr=="5"] <- 0 # if the chosen rook cell is 5, it is the focal cell, so the longitudinal change should be 0
	dLon[cr=="4"] <- pmax(-d46[cr=="4"], -limit) # if the chosen rook is 4, then it is to the left (negative)
	dLon[cr=="6"] <- pmin(d46[cr=="6"], limit) # if 6, then the adjusted trajectory is to the right, and is + change in Lon
	dLon[cr=="2"] <- 0 # if 2, adjusted trajectory is up, and no change in Lon
	dLon[cr=="8"] <- 0 # if 8, adjusted trajectory is down, and no change in Lon
	
	d28 <- speed.km/111.325 # convert climate speeds in km/timeStep to climate speeds in degrees latitude/timeStep
	dLat <- rep(NA, length(rook)) # create an empty vectory for storing latitudinal changes
	dLat[cr=="5"] <- 0 # if chosen rook is 5, no movement
	dLat[cr=="4"] <- 0 # if chosen rook is 4, it is to left, and no latitudinal movement
	dLat[cr=="6"] <- 0 # if chosen rook is 6, it is to the right, and no latitudinal movement
	dLat[cr=="2"] <- pmin(d28[cr=="2"], limit) # if 2, it is up, and positive latitudinal movement
	dLat[cr=="8"] <- pmax(-d28[cr=="8"], -limit) # if 8, it is down, and negative latitudinal movement
		
	return(list(dLon=dLon, dLat=dLat)) # return the delta-lon/lat as a named list
	
}