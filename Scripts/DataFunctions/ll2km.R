# taken from leapfrog, was in another script first though

# convert lon-lat to km
ll2km <- function(x,y){
	stopifnot(require(PBSmapping))
	# x should be longitude
	# y should be latitude
	blah <- data.frame(X=x, Y=y) 
	attr(blah, "projection")="LL"
	blah2 <- convUL(blah)
	list(lat.km=blah2[,"Y"], lon.km=blah2[,"X"]) # returning a named list format is handy for use with data.table :=
}

