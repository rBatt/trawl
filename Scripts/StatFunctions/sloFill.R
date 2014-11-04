sloFill <- function(slo, nr=3, nc=3){ # fill in the spatial slope with a spatial average
	focal(slo, w=matrix(1, nr=nr, nc=nc), fun=function(x)mean(x, na.rm=TRUE), pad=TRUE)
}
