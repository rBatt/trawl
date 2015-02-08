# http://stackoverflow.com/questions/8664976/r-round-to-nearest-5-or-1/8665511#8665511
rN <- function(a,b){ # round a to the nearest decimal (b)
	round(a/b)*b
}
