#' Generate a Grid
#' 
#' Generate a grid that is a raster
#' 
#' @param x an array, matrix or numeric vector; NULL will fill with NA's
#' @param dims vector of length 3 specifying the integer size of dimensions
#' @param ... arguments to be passed to \code{brick}
#' 
#' @details Use it like array(), except it's a brick
#' 
#' @import raster
#' @export
gen.grid <- function(x=NULL, dims=dim(x), byrow=FALSE, ...){
	
	ds <- length(dims)
	
	if(is.null(x)){ # e.g., if you want the raster to be empty (can't do it w/ brick())
		stopifnot(!is.null(dims) & ds==3) # can't have both arugments be NULL
		x <- array(NA, dim=dims)
	}else{
		x <- as.array(x) # recreate as an array
		dx <- length(dim(x))
		# if x was supplied, make sure that if it also has 3 dims those dims 
		
		if(dx==3 & ds==3){
			stopifnot(all(dims==dim(x)))
		}else if(dx!=3 & ds==3){
			x <- array(c(x), dim=dims)
		}else if(dx==3 & ds!=3){
			warning("Supplied `dims` not of length 3. Using `dim(x)`, which is of valid length, instead.")
		}else if(dx!=3 & ds!=3){
			stop("Either `dim(x)` or `dims` must be of length 3.")
		}
	}
	
	if(byrow){
		x <- aperm(x, c(2,1,3))
	}
	
	tr <- brick(x, ...)
	
	return(tr)
	
}