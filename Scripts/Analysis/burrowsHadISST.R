# =================
# = Load Packages =
# =================
library(raster)
library(SDMTools)



# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/HadISST.RData")

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
