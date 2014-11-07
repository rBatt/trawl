
focal.min <- function(r.min){ # where is the smallest value in the rook focus?
	# fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
	f0 <- focal(r.min, w=fw.mat, which.min, pad=TRUE, padValue=NA) # focal raster cell# containing smallest value
	setValues(f0, round(values(f0)))
}

# getF.min <- function(r.min){ # where is the smallest value in the rook focus?
# 	# fw.mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA),ncol=3) # focal weight matrix
# 	focal(r.min, w=fw.mat, min, pad=TRUE, padValue=NA, na.rm=TRUE) # focal raster cell# containing smallest value
# }