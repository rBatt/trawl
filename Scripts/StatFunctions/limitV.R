# Limit a velocity (provided in km/timeStep) to 1º of lat or lon per timeStep
# Note that 1 variable is defined outside of the function – this is to reduce computation time
# Also note that conv.fact.lon needs to be defined inside the script; again, this is to reduce computation time

limitV.mat <- cbind(c(1,-Inf),c(Inf,-1),c(1,-1))

limitV <- function(r, dir="lon", conv.fact.lon=NULL){ # limit velocity
	if(dir=="lon"){
		stopifnot(!is.null(conv.fact.lon)) # if converting km/yr to lon/yr requires knowing latitude, so you need to supply conv.fact.lon, which is 111.325*cos(lats/180*pi); not asking for just lats b/c then this conversion factor has to be defined every time the function is called
		r.coord <- r/conv.fact.lon
		r.coord <- reclassify(r.coord, limitV.mat)
		r <- r.coord*conv.fact.lon
	}else{
		r.coord <- r/111.325
		r.coord <- reclassify(r.coord, limitV.mat)
		r <- r.coord*111.325
	}
	r
}