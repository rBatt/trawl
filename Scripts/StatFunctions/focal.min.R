
focal.min <- function(r.min){ # where is the smallest value in the rook focus?
	# fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	focal(r.min, w=fw.mat, which.min, pad=TRUE) # focal raster cell# containing smallest value
}