
raster.nan2na <- function(x){
	x2 <- values(x)
	x2[is.nan(x2)] <- NA
	setValues(x, x2)
}
