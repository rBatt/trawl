
focal.max <- function(r.max){ # where is the biggest value in the rook focus?
	# fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	focal(r.max, w=fw.mat, which.max, pad=TRUE, padValues=-Inf) # focal raster cell# containing biggest value
}