
# Scale Weight: Replicate x based on the value of y (rep rows based on wtcpue)
sw <- function(x,y, y.range=NULL, thresh=25){ # scale the abundance according to wtcpue 
	# x is a value to repeat a number of times proportional to its y value
	# a positive numeric representing the relative abundance of y
	# rep(x, y/min(y[y>0], na.rm=TRUE))
	# rep(x, y)
	
	if(is.null(y.range)){
		y.min <- min(y[y>0], na.rm=TRUE)
	}else{
		y.min <- y.range[1]
		if(y.min<=0){y.min <- 0.001}
	}
	
	
	if(is.null(y.range)){
		y.max <- max(y, na.rm=TRUE)
	}else{
		y.max <- y.range[2]
	}
	
	
	
	rep.f0 <- y/y.min # y/min(y[y>0], na.rm=TRUE)
	mrf0 <- y.max/y.min #max(rep.f0, na.rm=TRUE)
	if(mrf0>thresh){
		rep.f <- rep.f0/(mrf0/thresh)
	}else{
		rep.f <- rep.f0
	}
	rep(x, pmax(1,rep.f))
}