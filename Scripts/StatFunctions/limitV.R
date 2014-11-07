# Limit a velocity (provided in km/timeStep) to 1º of lat or lon per timeStep
# Note that 1 variable is defined outside of the function – this is to reduce computation time
# Also note that conv.fact.lon needs to be defined inside the script; again, this is to reduce computation time

limitV.mat <- cbind(c(1/n.per.ll,-Inf),c(Inf,-1/n.per.ll),c(1/n.per.ll,-1/n.per.ll))
# limitV.mat <- cbind(c(1,-Inf),c(Inf,-1),c(1,0))

limitV <- function(r, dir="lon", conv.fact.lon=NULL){ # limit velocity
	if(dir=="lon"){
		stopifnot(!is.null(conv.fact.lon)) # if converting km/yr to lon/yr requires knowing latitude, so you need to supply conv.fact.lon, which is 111.325*cos(lats/180*pi); not asking for just lats b/c then this conversion factor has to be defined every time the function is called
		r.coord <- r/conv.fact.lon
		r.coord <- reclassify(r.coord, limitV.mat)
		r <- r.coord*conv.fact.lon
		r <- setValues(r, rep(0, ncell(r)), layer=3) # 3rd layer is up, if dealing with longitude, vel should be 0
		r <- setValues(r, rep(0, ncell(r)), layer=4) # 4th layer is down, if dealing with longitude, vel should be 0
	}else{
		r.coord <- r/111.325
		r.coord <- reclassify(r.coord, limitV.mat)
		r <- r.coord*111.325
		r <- setValues(r, rep(0, ncell(r)), layer=1) # 1st layer is left, if dealing with latitude, vel should be 0
		r <- setValues(r, rep(0, ncell(r)), layer=2) # 2nd layer is right, if dealing with latitude, vel should be 0
	}
	r
}