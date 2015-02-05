
mode <- function(x){
	xd <- density(x)
	xd$x[which.max(xd$y)]
}

get.modes <- function(x){
	if(ncol(x)>1){
		apply(x, 2, mode)
	}else{
		mode(x)
	}
}

