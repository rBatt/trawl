library(raster)
library(ncdf)
library(ncdf4)

# setwd("~/Documents/School&Work/pinskyPost/trawl/")

soda.info <- open.ncdf("./Data/SODA/soda_temp_180W-20W_10N-85N_1958-1978.nc")
name.soda.sizes <- sapply(soda.info$var$temp$dim, function(x)x$name)
soda.sizes <- soda.info$var$temp$size
dim.units <- sapply(soda.info$var$temp$dim, function(x)x$units)
names(soda.sizes) <- name.soda.sizes
ntime <- soda.sizes["time"]
ndepth <- soda.sizes["depth"]

soda.time0 <- soda.info$var$temp$dim[[4]]$vals
ref.date <- as.Date(gsub("months since ", "", dim.units[4]))
n.month.before <- ceiling(abs(soda.time0[1])) + 1
time.start <- rev(seq.Date(ref.date, by="-1 month", length.out=n.month.before))[1]
soda.time <- seq.Date(time.start, by="1 month", length.out=ntime)


pb <- txtProgressBar(min=1, max=ntime, style=3)
for(i in 1:ntime){
	t.soda1 <- brick("./Data/SODA/soda_temp_180W-20W_10N-85N_1958-1978.nc", lvar=4, level=i)
	# need to switch missing value to actual NA # TODO is this automatically done by brick()? I think so.s
	soda.depths <- as.numeric(gsub("X", "", names(t.soda1))) # does weird rounding that I don't understand
	
	# get the deepest temperature to fill in the object that will eventually have all bot temp
	t.soda.bot <- subset(t.soda1, ndepth)
	
	
	# # way slow :(
	# for(j in (ndepth-1):1){
	# 	prev.depth.na <- is.na(t.soda.bot)
	# 	soda.depth.j <- subset(t.soda1, j)
	# 	t.soda.bot[prev.depth.na] <- soda.depth.j[prev.depth.na]
	# 	# are there cells where the accumulated object has NA, but current depth has temp?
	# 	# if so, replace NA's in accumulated grid with the temperatures of this current depth
	#
	# }
	
	# much faster than my loop!
	t.soda.bot <- do.call(cover, unstack(subset(t.soda1, length(soda.depths):1)))
	names(t.soda.bot) <- soda.time[i]
	# the subsetting piece flips the order of the layers (so that deepest is first layer)
	# by unstacking I reformat from raster with layers, to list of rasters
	# that list can be used as the list of arguments to a function taking ...
	# cover keeps the values in the first object of the ..., and replaces the NA's with values from the second ...
	# that process repeats until through all object listed in the ...
	# in other words, the final value will be the value in the first layer that has a non-NA value in that cell
	
	# the j loop gives the bottom temperature for a given month
	# need to go on accumulating months
	# this will be for the first time period (1958-1978)
	if(i==1){
		soda.bot1 <- t.soda.bot
	}else{
		# soda.bot1 <- stack(soda.bot1, t.soda.bot)
		soda.bot1 <- addLayer(soda.bot1, t.soda.bot)
	}
	setTxtProgressBar(pb, i)
}
close(pb)


plot(subset(soda.bot1, 252) - subset(soda.bot1, 1), zlim=c(0,1))


pb <- txtProgressBar(min=1, max=ntime, style=3)
for(i in 1:ntime){
	t.soda2 <- brick("./Data/SODA/soda_temp_180W-20W_10N-85N_1979-1999.nc", lvar=4, level=i)
	soda.depths <- as.numeric(gsub("X", "", names(t.soda2)))
	
	# get the deepest temperature to fill in the object that will eventually have all bot temp
	t.soda.bot <- subset(t.soda2, ndepth)
	
	# much faster than my loop!
	t.soda.bot <- do.call(cover, unstack(subset(t.soda2, length(soda.depths):1)))
	names(t.soda.bot) <- soda.time[i]
	
	# this will be for the second time period (1979-1999)
	if(i==1){
		soda.bot2 <- t.soda.bot
	}else{
		soda.bot2 <- addLayer(soda.bot2, t.soda.bot)
	}
	setTxtProgressBar(pb, i)
}
close(pb)




nl.sb <- nlayers(soda1)
soda.bot <- subset(soda1, nl.sb)