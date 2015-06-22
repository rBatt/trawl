
r2m.cell <- function(x, nrow=NULL, ncol=NULL, from=c("raster", "matrix")){
	from <- match.arg(from)
	to <- c("matrix","raster")[c("matrix","raster")!=from]
	
	if(is.null(nrow)){
		nrow <- nrow(x)
		stopifnot(!is.null(nrow))
	}
	if(is.null(ncol)){
		ncol <- ncol(x)
		stopifnot(!is.null(ncol))
	}
	
	vals <- 1:(nrow*ncol)
	
	r.hold <- c(matrix(vals, nrow=nrow, ncol=ncol, byrow=TRUE))
	m.hold <- c(matrix(vals, nrow=nrow, ncol=ncol, byrow=FALSE))
	
	lookup <- data.frame(raster=r.hold, matrix=m.hold)
	lookup <- lookup[order(lookup[,from]),]
	
	m.x <- as.matrix(x)
	x2 <- c(m.x)
	x.converted <- lookup[,to][c(m.x)]
	
	x3 <- m.x
	x3[] <- x.converted
	
	return(x3)
	
}
