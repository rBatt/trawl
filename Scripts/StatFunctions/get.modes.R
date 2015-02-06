
mode <- function(x){
	xd <- density(x)
	xd$x[which.max(xd$y)]
}

get.modes <- function(x){
	ldx <- length(dim(x))
	if(ldx>1){
		apply(x, (2:ldx), mode)
	}else{
		mode(x)
	}
}

