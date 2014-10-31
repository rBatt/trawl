
focal2cell <- function(f){ # convert the focal raster cell# to parent raster cell#
	f.cell <- setValues(f, 1:length(f)) # cell #'s for raster
	f.ncol <- ncol(f) # number of columns in raster
	f.conv <- reclassify(f, cbind(c(4,6,2,8),c(-1,1,-f.ncol,f.ncol))) # convert focal cell# to raster cell# (assumes 3x3 focal)
	f.cell+f.conv # convert
}
