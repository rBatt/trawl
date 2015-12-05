#' Plot Simulated Response Curve
#' 
#' Plot the probability of presence against the environmental variable for all species
#' 
#' @param X a spp object
#' @param ... not used
#' 
#' @export
plot_simResp <- function(X, ...){
	stopifnot("spp"%in%class(X))
	
	dims <- attr(X, "dims")
	ns <- dims["ns"]
	spp.densX <- attr(X, "spp.densX")
	
	# Plot Temperature Distributions for the Whole Community
	store <- c()
	# sdt2 <- lapply(S.dens.X, FUN=function(x){x$y <- x$y/max(x$y); x})
	sdt2 <- lapply(spp.densX, FUN=function(x){x})
	for(d in 1:length(spp.densX[[1]]$x)){
		ti <- c()
		for(i in 1:length(sdt2)){
			ti[i] <- sdt2[[i]]$y[d]
		}
		store[d] <- mean(ti)
	}

	par(mar=c(2.15,2.15,0.1,0.1), ps=10, mgp=c(1.15,0.2,0), tcl=-0.2)
	plot(sdt2[[1]]$x, sdt2[[1]]$y, type="l", ylim=c(0,1), xlab="Environmental Variable", ylab="Relative Density", lwd=0.1)
	for(i in 2:ns){
		lines(sdt2[[i]]$x, sdt2[[i]]$y, type="l", lwd=0.1)
	}
	lines(spp.densX[[1]]$x, store, type="l", xlab="Environmental Variable", ylab="Density (community average)", lwd=3, col="red")
	
}