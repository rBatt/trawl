#' Plot Simulated Biomass at Time t
#' 
#' Plot the spatial biomass of each species during the specified time step
#' 
#' @param X a spp object
#' @param time the time step
#' @param ... not used
#' 
#' @export
plot_sim_bio_t <- function(X, time=1, ...){
	stopifnot("spp"%in%class(X))
	stopifnot(length(time)==1)
	
	spp.bio <- attr(X, "spp.bio")
	S <- getS(X)
	
	spp.1 <- setValues(S[[time]], spp.bio[,,time]) # cell ordering is for raster
	smplt <- c(0.9,0.92, 0.2,0.8)
	bgplt <- c(0.05,0.89,0.15,0.95)
	axargs <- list(mgp=c(0.5,0.15,0))
	# dev.new(width=12, height=5)
	plot(spp.1, maxnl=200, col=tim.colors(), zlim=range(values(spp.1), na.rm=TRUE),smallplot=smplt, bigplot=bgplt, axis.args=axargs, nr=8, nc=25, legend=FALSE, colNA="darkgray")
}
