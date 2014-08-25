calcarea <- function(lonlat){
	hullpts = chull(x=lonlat[,1], y=lonlat[,2]) # find indices of vertices
	hullpts = c(hullpts,hullpts[1]) # close the loop
	ps = appendPolys(NULL,mat=as.matrix(lonlat[hullpts,]),1,1,FALSE) # create a Polyset object
	attr(ps,"projection") = "LL" # set projection to lat/lon
	psUTM = convUL(ps, km=TRUE) # convert to UTM in km
	polygonArea = calcArea(psUTM,rollup=1)
	return(polygonArea$area)
}