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


for(i in 1:ntime){
	t.soda1 <- brick("./Data/SODA/soda_temp_180W-20W_10N-85N_1958-1978.nc", lvar=4, level=i)
	soda.depths <- as.numeric(gsub("X", "", names(soda1))) # does weird rounding that I don't understand
	for(i in )
}





nl.sb <- nlayers(soda1)
soda.bot <- subset(soda1, nl.sb)