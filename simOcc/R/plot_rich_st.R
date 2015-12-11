#' Plot Richness over Space and Time
#' 
#' Plot the 2D map of richness, and make that map for each time step
#' 
#' @param X a spp object
#' @param time time steps to plot; can be vector to plot multiple maps in same figure
#' 
#' @import fields
#' 
#' @export
plot_rich_st <- function(X, time=1, ...){
	stopifnot("spp"%in%class(X))
	
	grid.X <- attr(X, "grid.X")
	grid.t <- attr(X, "dims")["grid.t"]
	grid.h <- attr(X, "dims")["grid.h"]
	spp.bio <- attr(X, "spp.bio")
	
	
	space.rich <- grid.X
	for(i in time){
		t.bio <- spp.bio[,,i]
		t.rich <- apply(t.bio, 1, function(x, ...)sum(!is.na(x)))
		values(space.rich[[i]]) <- t.rich
	
	}

	smplt <- c(0.85,0.88, 0.2,0.8)
	bgplt <- c(0.05,0.82,0.15,0.95)
	axargs <- list(mgp=c(0.5,0.15,0))

	zlim <- range(values(space.rich), na.rm=TRUE)
	ylim <- c(1.085,0)
	dev.new()
	par(mfrow=rbLib::auto.mfrow(length(time)))

	mfg.mat <- matrix(1:grid.t, nrow=rbLib::auto.mfrow(length(time))[1], byrow=TRUE)
	for(i in 1:grid.t){
		t.mfg <- which(mfg.mat==i, T)
		par(mar=c(0.25,0.25,0.15,0), ps=6, mgp=c(1,0.15,0), tcl=-0.15, cex=1, mfg=c(t.mfg[1],t.mfg[2]))
		tp <- t(matrix(values(subset(space.rich, i)), nrow=grid.h, byrow=TRUE))
		image.plot(tp, zlim=zlim, ylim=ylim, axis.args=axargs, xaxt="n", yaxt="n", smallplot=smplt, bigplot=bgplt)
	}
	
}