#' Plot Environment
#' 
#' Plot simulated environment from a spp object
#' 
#' @param X a spp object
#' @param ... not used
#' 
#' @details
#' creates a plot of the environmental variable
#' 
#' @return 
#' nothing at the moment
#' 
#' @import fields
#' @import raster
#' @export

plot_env <- function(X, ...){
	stopifnot("spp"%in%class(X))
	
	grid.X <- attr(X, "grid.X")
	temp.range <- range(values(grid.X))
	smplt <- c(0.89,0.95, 0.2,0.8)
	bgplt <- c(0.15,0.85,0.12,0.95)
	axargs <- list(mgp=c(0.75,0.25,0), tcl=-0.15)
	plot(grid.X, zlim=c(temp.range[1], temp.range[2]), col=tim.colors(), smallplot=smplt, bigplot=bgplt, axis.args=axargs, nc=5,mgp=c(0.75,0.25,0), tcl=-0.15)
}

