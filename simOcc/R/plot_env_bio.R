#' Plot Space-Time Environmental Variable and Biomass
#' 
#' For one species at a time, and for a subset of the time steps, plot the biomass of that species in space
#' 
#' @param X a spp object
#' @param n.steps number of time steps
#' @param spp.id integer (length 1) indicating index of species to plot; if missing picks one at random
#' @param ... not used
#' 
#' @export
plot_env_bio <- function(X, n.steps=6, spp.id, ...){
	
	stopifnot("spp" %in% class(X))
	stopifnot(length(n.steps)==1)
	
	dims <- attr(X, "dims")
	grid.t <- dims["grid.t"]
	ns <- dims["ns"]
	grid.h <- dims["grid.h"]
	spp.bio <- attr(X, "spp.bio")
	grid.X <- attr(X, "grid.X")
	
	if(missing(spp.id)){
		spp.id <- sample(1:ns, 1)
	}
	stopifnot(length(spp.id)==1)
	
	smplt <- c(0.85,0.88, 0.2,0.8)
	bgplt <- c(0.05,0.82,0.15,0.95)
	axargs <- list(mgp=c(0.5,0.15,0))
	

	


	par(mfrow=c(2, n.steps), oma=c(0.5,1,0,0))
	year.index <- seq(1, grid.t, length.out=n.steps)
	for(i in 1:n.steps){
		t.y <- year.index[i]
	
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(1,i))
		image.plot(t(matrix(values(subset(grid.X, t.y)), nrow=grid.h, byrow=TRUE)), zlim=range(values(grid.X), na.rm=TRUE), ylim=c(1.085,0), axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
	
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(2,i))
		image.plot(t(matrix(spp.bio[,spp.id,t.y], nrow=grid.h, byrow=TRUE)), zlim=range(spp.bio[,spp.id,], na.rm=TRUE), ylim=c(1.085,0), bg="gray", axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
	}
	
}