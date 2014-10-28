

sProj <- function(a,b, lenA=NULL){
	# Compute the scalar projection of vector A in the direction of vector B
	# So A could be the deltaLon,deltaLat of a community, and B would be the deltaLL of bottom temperature, e.g. (this is length==2 case)
	# Then sProj() would give
	
	
	# Do a bunch of checks on classes, dimensions, lengths
	stopifnot(class(a)==class(b)) # Check that a & b are of the same class
	if(is.matrix(a)){
		mn <- dim(a)
		arow <- mn[1]
		stopifnot(all(mn==dim(b))) # if a matrix, make sure the dimensions match
		vecCheck <- !mn[2]%in%c(2,4)|!ncol(b)%in%c(2,4) # if they're matrices, make sure same # columns
	}else{
		arow <- 1
		vecCheck <- !arow%in%c(2,4)|!length(b)%in%c(2,4) # if they're vectors, make sure same length
	}
	if(vecCheck){ # print out error if there is a dimension mismatch between a and b
		stop("Wrong lengths for vectors a and/or b: length 4 interpreted as c(lon0,lon1,lat0,lat1), length 2 as c(lon1-lon0,lat1-lat0)")
	}
	
	
	
	# Convert a and b to matrices (if not already)
	if(!is.matrix(a)){
		a <- matrix(a, ncol=length(a), nrow=1)
		b <- matrix(b, ncol=length(b), nrow=1)
	}
	

	# Convert to point relative to 0,0
	if(ncol(a)==4){ # convert a
		a <- matrix(a[,3:4]-a[,1:2], ncol=2)
	}
	if(ncol(b)==4){ # convert b
		b <- matrix(b[,3:4]-b[,1:2], ncol=2)
	}
	

	
	# Compute the length of vector A if not already given
	if(is.null(lenA)){
		lenA <- sqrt(a[,1]^2+a[,2]^2)
	}
	
	

	# Formula for theta: http://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r
	# cos.theta <- sum(a*b)/(sqrt(sum(a*a))*sqrt(sum(b*b))) # for vector a & b only
	# aa <- a*a
	# ab <- a*b
	# bb <- b*b
	# cos.theta <- (ab[,1]+ab[,2])/(sqrt(aa[,1]+aa[,2])*sqrt(bb[,1]+bb[,2])) # slightly slower version of final
	cos.theta <- .rowSums(a*b, m=arow, n=2)/(sqrt(.rowSums(a*a, m=arow, n=2))*sqrt(.rowSums(b*b, m=arow, n=2)))
		
	# a.same <- .rowSums(a==0, m=arow, n=2)==2
	# b.same <- .rowSums(b==0, m=arow, n=2)==2
	#
	# sp <- rep(NA, arow)
	
	
	# Return the scalar projection of A in the direction of B
	# Wikipedia page for formula: http://en.wikipedia.org/wiki/Dot_product#Scalar_projection_and_first_properties
	# Wikipedia diagram showing what the scalar projection is: http://en.wikipedia.org/wiki/Dot_product#mediaviewer/File:Dot_Product.svg
	sp <- lenA*cos.theta
	sp
}
