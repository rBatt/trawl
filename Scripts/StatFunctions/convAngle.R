
# Convert between 2 angle conventions, one where "up" is 0º and angle increases clockwise, and other where "right" is 0º and angle increases counterclockwise
# rcos <- function(x, ...){cos(x)}
# rsin <- function(x, ...){sin(x)}

convAng <- function(x, ...){
	cosx <- base:::cos(x)
	sinx <- base:::sin(x)
	sml <- .Machine$double.eps
	cls <- class(x)
	
	if(!is.null(cls) & cls%in%c("RasterLayer","RasterBrick","RasterStack")){
		reclassify(cosx, matrix(c(-sml, sml, 0),ncol=3))
	}else{
		if(any(sml > abs(cosx))){
			cosx[sml > abs(cosx)] <- 0
		}
		if(any(sml > abs(sinx))){
			sinx[sml > abs(sinx)] <- 0
		}	
	}

	if(!is.null(cls) & cls%in%c("RasterLayer","RasterBrick","RasterStack")){
		angle0 <- base:::atan2(values(cosx), values(sinx))
		angle <- setValues(x, angle0)
		if(any(values(angle)<0)){
			angle <- angle + (!is.na(angle)&angle<0)*2*pi
		}
	}else{
		angle <- base:::atan2(cosx, sinx)
		if(any(angle<0)){
			angle[angle<0] <- angle[angle<0]+2*pi
		}
	}

	angle
}
#
# plot(convAng(spatAng)/pi*180, col=circular.colors(256))
#
# convAng(275/180*pi)/pi*180 # get 175, makes sense
# convAng(175/180*pi)/pi*180 # get 275; same formula converts the opposite way, too
#
# convAng(60/180*pi)/pi*180 # get 30, makes sense
# convAng(30/180*pi)/pi*180 # get 60
#
# convAng(120/180*pi)/pi*180 # get 330, makes sense
# convAng(330/180*pi)/pi*180 # get 120
#
# convAng(120/180*pi)/pi*180 # get 330, makes sense
# convAng(330/180*pi)/pi*180 # get 120
