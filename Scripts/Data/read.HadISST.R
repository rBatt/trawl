

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

# A function to get the rate of change of a vector
gSlope <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		as.numeric(lm(x~I(0:(length(x)-1)))$coef[2])
	}
}

# A function to define the 3x3 grid
# See grid here:
# http://webhelp.esri.com/arcgisdesktop/9.2/index.cfm?topicname=how_slope_works



# =====================
# = Read and trim SST =
# =====================
startYear <- 1968   # start of the period
endYear <- 2013     # end of the period
subp <- '1968-01-01/2012-12-01'   # period for the climatology calculation


# First, read in the brick
sst000 <- brick("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/HadISST/HadISST_sst.nc")

# Define desired starting and stopping dates, and define dates in brick
Date <- substr(names(sst000),2,11) # get brick dates
Date <- gsub('\\.', '\\-', Date) # format brick dates by replacing "." with "-"
Date <- as.Date(Date) # format brick dates as a date object

dstart.pat <- paste(startYear,'01','[0-9]{2}',sep='-') # define the start date pattern to be used in a regular expression
dstart <- grep(dstart.pat, as.character(Date)) # find the starting date pattern in the brick dates

dend.pat <- paste(endYear,'12','[0-9]{2}',sep='-') #define the end date pattern for the regex
dend <- grep(dend.pat, Date) # find the ending date pattern in the brick dates


# Second, trim the brick to the desired dates
sst00 <- subset(sst000, dstart:dend) # subset the brick to the dates within (inclusively) the start and end dates
Date <- Date[dstart:dend] # trim the brick dates to match the brick


# Third, trim the brick to the desired coordinates
xmin(sst00) <- xmin(sst00) - 30
xmax(sst00) <- xmax(sst00) + 30 
newExtent <- extent(c(-210, -30, 20, 70)) # define the new "extent"
sst0 <- crop(sst00, newExtent)


# Fourth, ice and land values as NA's
# http://stackoverflow.com/questions/11966503/how-to-replace-nas-in-a-raster-object
sst <- reclassify(sst0, matrix(c(-1000, -32768, NA, NA), ncol=2))


# Fifth, take annual means.
sst.ann <- stackApply(sst, indices=rep(1:(dim(sst)[3]/12),each=12), fun=mean)


# Sixth, take grand means
sst.mu <- stackApply(sst.ann, indices=rep(1, length(sst.ann)), fun=mean)


# ==================================
# = Get the temporal trend for SST =
# ==================================
timeTrend <- stackApply(sst.ann, indices=rep(1,length(sst)), fun=gSlope)


# ============================
# = Get the spatial gradient =
# ============================

# blah <- as.matrix(timeTrend)
# #  blah[25:27,35:37]
# #             [,1]        [,2]        [,3]
# # [1,] 0.008696129 0.008692858 0.009128356
# # [2,] 0.007054508 0.007128589 0.007470006
# # [3,] 0.006070242 0.006230357 0.006470819
#
# blah2 <- as.vector(blah)
# ind.26.36 <- nrow(blah)*35+26 # get the value at 26, 36
# blah2[ind.26.36] # ok, the index of 26,36 is 1776
# blah[ind.26.36-1] # go directly above the focal index

testGrid <- raster(matrix(c(50,30,8,45,30,10,50,30,10), ncol=3))

cent1mat <- matrix(c(rep(0,4),1,rep(0,4)), ncol=3)
f.padVal <- focal(sst.mu, w=cent1mat, fun=sum)
w.dzdx <- matrix(c(-1,-2,-1,0,0,0,1,2,1), ncol=3)
w.dzdy <- matrix(c(-1,0,1,-2,0,2,-1,0,1), ncol=3)

blah <- focal(testGrid, w.dzdx, sum)
blah <- focal(testGrid, w.dzdy, sum)

library(SDMTools)
# NOTE: I have now discovered SDMTools:::slope(), and am no longer going to try to write my own function for doing this calculation via focal. See later versions of this script for how this line of thinking progresses. (RDB, 29-Oct-2014)
spatGrad.slope <- slope(sst.mu, latlon=TRUE)
spatGrad.aspect <- aspect(sst.mu, latlon=TRUE)







#
# aiGrid <- function(x, focal.row, focal.col){
# 	dx <- dim(x)
# 	nr <- dx[1]
# 	nc <- dx[2]
#
# 	grid.out <- c("a"=NaN, "b"=NaN, "c"=NaN, "d"=NaN, "e"=NaN, "f"=NaN, "g"=NaN, "h"=NaN, "i"=NaN)
#
# 	# Simple case, where the focal element is not on the edge
# 	if(!focal.row%in%c(1, nr) & !focal.col%in%c(1, nc)){
# 		grid.33 <- as.matrix(x[(focal.row-1):(focal.row+1), (focal.col-1):(focal.col+1)])
# 		grid.out[names(grid.out)] <- c(t(grid.33))
# 	}
#
# 	# When the focal element is at either the minimum or maximum row
# 	if(focal.row%in%c(1, nr) & !focal.col%in%c(1, nc)){
# 		if(focal.row==1L){ # if at the minimum (top) row
# 			grid.23 <- as.matrix(x[(focal.row):(focal.row+1), (focal.col-1):(focal.col+1)])
# 			grid.out[letters[4:9]] <- c(t(grid.23))
# 		}else{ # if at the maximum (bottom) row
# 			grid.23 <- as.matrix(x[(focal.row-1):(focal.row), (focal.col-1):(focal.col+1)])
# 			grid.out[letters[1:6]] <- c(t(grid.23))
# 		}
# 	}
#
# 	# When the focal element is at either the minimum or maximum column
# 	if(!focal.row%in%c(1, nr) & focal.col%in%c(1, nc)){
# 		if(focal.col==1L){ # if at the minimum (left) column
# 			grid.32 <- as.matrix(x[(focal.row-1):(focal.row+1), (focal.col):(focal.col+1)])
# 			grid.out[c("b","c","e","f","h","i")] <- c(t(grid.32))
# 		}else{ # if at the maximum (bottom) row
# 			grid.23 <- as.matrix(x[(focal.row-1):(focal.row+1), (focal.col-1):(focal.col)])
# 			grid.out[c("a","b","d","e","g","h")] <- c(t(grid.32))
# 		}
# 	}
#
#
#
# 	focal.row.m1 <-
#
# 	grid.33 <- x[]
#
# }


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


