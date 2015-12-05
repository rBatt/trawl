#' Plot time series of true regional richness
#' 
#' Plot a simple time series of regional richness, meaning the richness plotted represented the aggregated number of unique species among all sites in each time step
#' 
#' @param X a spp object
#' @param ... not used
#' 
#' @export
plot_regRich_ts <- function(X, ...){
	stopifnot("spp"%in%class(X))
	
	dims <- attr(X, "dims")
	grid.t <- dims["grid.t"]
	
	
	true.rich <- c()
	for(i in 1:grid.t){
		t.pres <- X[,,i]
		true.rich[i] <- sum(apply(t.pres, 2, function(x, ...)any(!is.na(x))))
	}
	dev.new(width=3.5, height=3.5)
	par(mar=c(2, 2, 0.1, 0.1), mgp=c(1.1, 0.15, 0), tcl=-0.15, ps=10)
	plot(1:grid.t, true.rich, type="l", xlab="Time", ylab="Total Grid Richness")
}