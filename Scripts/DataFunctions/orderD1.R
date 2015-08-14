
# Reorder 1st dim of array w/o knowing total dims
orderD1 <- function(x, ord){	
	dims <- dim(arr)
	ndim <- length(dims)
	
	stopifnot(ndim>0)
	
	if(ndim==1){
		return(x[ord])
	}

	wl_i <- which(letters=="i")
	dimLetters <- letters[wl_i:(wl_i+ndim-1)]

	dimList <- structure(vector("list",ndim), .Names=dimLetters)
	dimList[[1]] <- ord
	for(i in 2:ndim){
		dimList[[i]] <- 1:dims[i]
	}
	do.call("[",c(list(x=arr),dimList))
}

# # Example
# orderD1(arr, 4:1)
# arr <- array(1:24, dim=c(4,3,2))
# arr[4:1,,]
#
# library(microbenchmark)
# microbenchmark(arr[4:1,,], orderD1(arr, 4:1), times=1E3)
# Unit: nanoseconds
#               expr   min    lq      mean median      uq    max neval
#       arr[4:1, , ]   864  1241  1445.876   1451  1596.0  17191  1000
#  orderD1(arr, 4:1) 52020 54061 56286.856  54909 56194.5 179363  1000
