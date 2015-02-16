

plot.traj <- function(x, y, thin.rate=4, nearB=1E-4, col="black", thinDots=TRUE, adjArr=1, ...){	
	step.seq <- seq(1, nlayers(x), by=10) #c(1, 30, 60, 90)
	step.seq[length(step.seq)] <- nlayers(x)
	

	
	# =======================================
	# = Thinning of arrows, segments, point =
	# =======================================
	thin <- seq(1, ncell(x), by=thin.rate)
	to.ss.arr <- sort(c(matrix(1:ncell(x), ncol=ncol(x), byrow=TRUE)[seq(1,nrow(x), by=thin.rate), seq(1,ncol(x), by=thin.rate)])) # get cell #'s that skip every ith row and every ith column, where ith is thin.rate. If you simply pick every ith cell #, and the number of columns is a multiple of i, then you get all rows and  every ith column
	to.ss.seg <- sort(c(matrix(1:ncell(x), ncol=ncol(x), byrow=TRUE)[seq(1,nrow(x), by=trunc(max(thin.rate/4,2))), seq(1,ncol(x), by=trunc(max(thin.rate/4,2)))]))
	
	
	# ===============================
	# = Final Destinations (points) =
	# ===============================
	fd.x <- subset(x, max(step.seq))
	fd.y <- subset(y, max(step.seq))
	if(thinDots){
		col2 <- rep(NA, ncell(x))
		col2[to.ss.seg] <- col
	}else{
		col2 <- col
		
	}
	plot(fd.x, fd.y, pch=20, col=col2, ...)
	
	
	# ============
	# = Segments =
	# ============
	for(l in 2:nlayers(x)){
		
		x0 <- values(subset(x,(l-1)))[to.ss.seg]
		y0 <- values(subset(y,(l-1)))[to.ss.seg]
		x1 <- values(subset(x,l))[to.ss.seg]
		y1 <- values(subset(y,l))[to.ss.seg]
		
		mvs <- x0!=x1 | y0!=y1
		
		# segments(x0[mvs], y0[mvs], x1[mvs], y1[mvs], col=heat.cols[l], lwd=0.5)
		segments(x0[mvs], y0[mvs], x1[mvs], y1[mvs], lwd=0.25, col="black")
		
	}
	
	
	# ==========
	# = Arrows =
	# ==========
	# Plot arrows
	# for(i in 2:length(step.seq)){
	for(i in c(75)){
		ss.t <- step.seq[i]
		ss.t1 <- ss.t-74
		
		x0 <- rN(values(subset(x,ss.t1))[to.ss.arr], nearB)
		y0 <- rN(values(subset(y,ss.t1))[to.ss.arr], nearB)
		x1 <- rN(values(subset(x,ss.t))[to.ss.arr], nearB)
		y1 <- rN(values(subset(y,ss.t))[to.ss.arr], nearB)
		
		mvs <- x0!=x1 | y0!=y1
		
		arrows(x0[mvs], y0[mvs], x1[mvs], y1[mvs], length=0.05*adjArr, col="black", lty=1, angle=20, lwd=0.75)
	}
	

	# ===================
	# = Add map outline =
	# ===================
	invisible(map(add=TRUE, fill=FALSE, col="black", lwd=0.5))
}
