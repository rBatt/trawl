
angMean <- function(x, na.rm=NULL){
	if(all(is.na(x))){return(NA)}
	tmean <- base:::atan2(mean(sin(x), na.rm=TRUE),mean(cos(x), na.rm=TRUE))
	if(sign(tmean)==-1L){
		tmean+2*pi
	}else{
		tmean
	}
}
angFill <- function(slo, nr=3, nc=3){ # fill in the spatial slope with a spatial average
	focal(slo, w=matrix(1, nr=nr, nc=nc), fun=angMean, pad=TRUE)
}
