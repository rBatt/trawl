# ===============
# = spline.poly =
# ===============
#
# # http://gis.stackexchange.com/questions/24827/how-to-smooth-the-polygons-in-a-contour-map/24929#24929
#
#
# Splining a polygon.
#
#   The rows of 'xy' give coordinates of the boundary vertices, in order.
#   'vertices' is the number of spline vertices to create.
#              (Not all are used: some are clipped from the ends.)
#   'k' is the number of points to wrap around the ends to obtain
#       a smooth periodic spline.
#
#   Returns an array of points. 
# 
spline.poly <- function(xy, vertices, k=3, ...) {
    # Assert: xy is an n by 2 matrix with n >= k.

    # Wrap k vertices around each end.
    n <- dim(xy)[1]
    if (k >= 1) {
        data <- rbind(xy[(n-k+1):n,], xy, xy[1:k, ])
    } else {
        data <- xy
    }

    # Spline the x and y coordinates.
    data.spline <- spline(1:(n+2*k), data[,1], n=vertices, ...)
    x <- data.spline$x
    x1 <- data.spline$y
    x2 <- spline(1:(n+2*k), data[,2], n=vertices, ...)$y

    # Retain only the middle part.
    cbind(x1, x2)[k < x & x <= n+k, ]
}



# ==============
# = splineHull =
# ==============
# based on:
# http://stackoverflow.com/questions/13577918/r-plotting-a-curve-around-a-set-of-points

# Original from SO:
# plot(NA,xlim=c(0,10),ylim=c(0,10))
# points(testpts,pch=19)
# chuld <- lapply(testpts,"[",chull(testpts))
# polygon(chuld,lty=2,border="gray")
# polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border="red",lwd=2)

splineHull <- function(x, aval=1){ # x is a lsit with components "x" and "y"
	# names(x) <- c("x","y")
	# chuld <- lapply(x,"[",chull(x))
	# polygon(chuld,lty=2,border="gray")
	# polygon(spline.poly(as.matrix(as.data.frame(chuld)),100),border="red",lwd=2)
	
	# load the required library
	require(alphahull)

	# plot(NA,xlim=c(0,10),ylim=c(0,10))
	# points(x,pch=19)
	# remove duplicate points so the ahull function doesn't error out
	testptsnodup <- x #as.data.frame(x) #lapply(x,"[",which(!duplicated(as.matrix(as.data.frame(x)))))
	
	ahull.obj <- ahull(testptsnodup,alpha=aval)
	plot(ahull.obj,add=TRUE,wpoints=FALSE, lwd=1, col="black")
}




