#' Plot Richness Accumulation Curve for Each Year
#' 
#' As you observe more places, you'll see more species. Plot that curve of species accumulated per sites visited for each year.
#' 
#' @param X a spp object
#' @param ... not used
#' 
#' @export
plot_rich_accum <- function(X, ...){
	stopifnot("spp"%in%class(X))
	
	dims <- attr(X, "dims")
	grid.t <- dims["grid.t"]
	
	par(mar=c(1.75, 1.75, 0.1, 0.1), mgp=c(0.85, 0.15, 0), tcl=-0.15, ps=10)

	spp.sample <- function(x, n){
		sub <- matrix(x[sample(nrow(x), n),], ncol=ncol(x))
		sum(apply(sub, 2, function(x)any(!is.na(x))))
	}

	rich.cols <- tim.colors(n=grid.t)

	n.max <- min(dim(out)[1],200)
	n.iter <- 5
	min.effort <- 1
	
	for(j in 1:grid.t){
		spp.t.m <- matrix(X[,,j], nrow=dim(X)[1], ncol=dims["ns"]) # cell ordering is for raster

		rich <- matrix(NA, nrow=n.max, ncol=n.iter)
		for(k in 1:n.iter){
			for(i in min.effort:n.max){
				rich[i,k] <- spp.sample(spp.t.m, n=i)
			}
		}
	
		rich.mu <- rowMeans(rich)
	
		if(j==1){
			plot(1:n.max, rich.mu, col=rich.cols[j], ylab="Avg Richness", xlab="# Cells Sampled", type="l", ylim=c(00,max(true.rich)))
		}else{
			lines(1:n.max, rich.mu, col=rich.cols[j])
		}
	
	
	}
}