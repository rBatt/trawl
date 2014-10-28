

# http://stackoverflow.com/questions/24115110/importing-sea-surface-temperature-text-files-in-ascii-format-into-r
# ===========================================
# = Try the first answer to the SO question =
# ===========================================
# read.things <- function(f) {
#   # f is the file path of your ascii data
#   require(raster)
#   d <- readLines(f)
#   d <- split(d, rep(1:12, each=181))
#   d <- lapply(d, function(x) read.fwf(textConnection(x), rep(6, 360),
#                                       skip=1, stringsAsFactors=FALSE,
#                                       na.strings=c(-1000, -32768)))
#   d <- lapply(d, function(x) sapply(x, as.numeric))
#   out <- stack(lapply(d, raster))
#   names(out) <- month.abb
#   extent(out) <- c(-180, 180, -90, 90)
#   out/100
# }

# sst.1961.1990 <- read.things("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/HadISST/HadISST1_SST_1961-1990.txt")

# extremely slow!!
# sst2013 <- read.things("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/HadISST/HadISST1_SST_2013.txt")

# library(rasterVis)
# levelplot(sst2013, at=seq(min(sst2013[], na.rm=T), max(sst2013[], na.rm=T), len=100), col.regions=colorRampPalette(c('#2c7bb6', '#abd9e9', '#ffffbf', '#fdae61', '#d7191c')))

# =========================
# = Try the second answer =
# =========================
# http://stackoverflow.com/a/24115597/2343633
library(raster)
library(xts)
library(caTools)  

startYear <- 1968   # start of the period
endYear <- 2011     # end of the period
subp <- '1968-01-01/2012-12-01'   # period for the climatology calculation



sst00 <- brick("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/HadISST/HadISST_sst.nc")
Date <- substr(names(sst00),2,11) 
Date <- gsub('\\.', '\\-', Date)
Date <- as.Date(Date)

dstart.pat <- paste(startYear,'01','[0-9]{2}',sep='-')
dstart <- grep(dstart.pat, as.character(Date))

dend.pat <- paste(endYear,'12','[0-9]{2}',sep='-')
dend <- grep(dend.pat, Date)

sst0 <- subset(sst00, dstart:dend)
Date <- Date[dstart:dend]

tserie <- as.vector(extract(sst, cbind(116, -35)))
tserie <- xts(tserie, order.by=Date)

clim <- as.numeric()
for(ii in 1:12){
  clim[ii] <- mean(tserie[subp][(.indexmon(tserie[subp])+1) == ii])
}
clim <- xts(rep(clim, length(tserie)/12), order.by=Date)



r1 <- raster(nrow=10, ncol=10)
r1 <- setValues(r1, runif(ncell(r1)))
r2 <- setValues(r1, runif(ncell(r1)))
r3 <- setValues(r1, runif(ncell(r1)))
rb <- brick(r1,r2,r3)

as.array(rb)
apply(as.array(rb), c(1,2), mean)

as.matrix(mean(rb))


