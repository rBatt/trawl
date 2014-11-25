
# Scale Weight: Replicate x based on the value of y (rep rows based on wtcpue)
sw <- function(x,y, thresh=25){ # scale the abundance according to wtcpue 
	# x is a value to repeat a number of times proportional to its y value
	# a positive numeric representing the relative abundance of y
	# rep(x, y/min(y[y>0], na.rm=TRUE))
	# rep(x, y)
	rep.f0 <- y/min(y[y>0], na.rm=TRUE)
	mrf0 <- max(rep.f0, na.rm=TRUE)
	if(mrf0>thresh){
		rep.f <- rep.f0/(mrf0/thresh)
	}else{
		rep.f <- rep.f0
	}
	rep(x, pmax(1,rep.f))
}