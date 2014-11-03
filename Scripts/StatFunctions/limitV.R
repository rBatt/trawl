# Limit a velocity (provided in km/timeStep) to 1º of lat or lon per timeStep
# Note that 1 variable is defined outside of the function – this is to reduce computation time
# Also note that conv.fact.lon needs to be defined inside the script; again, this is to reduce computation time
limitV.mat <- cbind(c(1,-Inf),c(Inf,-1),c(1,-1))
limitV <- function(r, dir="lon"){ # limit velocity
	if(dir=="lon"){
		r.coord <- r/conv.fact.lon # when coords is lats
		r.coord <- reclassify(r.coord, limitV.mat)
		r <- r.coord*conv.fact.lon
	}else{
		r.coord <- r/111.325
		r.coord <- reclassify(r.coord, limitV.mat)
		r <- r.coord*111.325
	}
	r
}