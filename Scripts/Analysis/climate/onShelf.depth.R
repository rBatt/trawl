

# =================
# = Load Packages =
# =================
# library(ncdf)
library(raster)


# ===================
# = Set Shelf Depth =
# ===================
shDepth <- 750 # meters
 

# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}

# =============
# = Load Data =
# =============
load("./trawl/Data/ETOPO.RData")


# ========================
# = Set the Coord System =
# ========================
crs(depth) <- "+proj=lcc +lat_1=65 +lat_2=20 +lon_0=0 +ellps=WGS84"


# =========================
# = Aggregate Relief Data =
# =========================
matchDepth <- function(x, ...){
	# if(any(x<0)){
	if((sum(x<0 & x>-shDepth)>30) | (any(x<0)&any(x>-25)) ){ # if 1/3 of the cells are below sea level, or if has cells that are both above and below
		return(max(x[x<0], ...))
	}else{
		return(min(x, ...))
	}
}

depth.ag <- aggregate(depth, 30, matchDepth)




# ================================
# = Logic for each type of cover =
# ================================
land.ind <- is.na(sst.mu0)
deep.ice.ind <- depth.ag<=-shDepth & land.ind
deep.ind <- depth.ag<=-shDepth & !land.ind
shelf.ice.ind <- depth.ag> -shDepth & depth.ag<=0 & land.ind
shelf.ind <- depth.ag> -shDepth & depth.ag<=0 & !land.ind
limno.ind <- depth.ag>0 & !land.ind

sapply(list(land.ind, deep.ice.ind, deep.ind, shelf.ice.ind, shelf.ind), sum)


# ==================================
# = Define Cover Types as Integers =
# ==================================
land.int1 <- (land.ind & !(deep.ice.ind | shelf.ice.ind))*1 # 618 cells
ice.int2 <- (deep.ice.ind | shelf.ice.ind)*2 # ice or inland below sea level # 1551 cells
limno.int5 <- (limno.ind) * 3 # 1303 cells
shelf.int3 <- (shelf.ind) * 4 # 961 cells
deep.int4 <- (deep.ind) * 5 # 2317 cells



# ===============================
# = Combine Integer Cover Types =
# =============================== 
cover.type <- land.int1 + ice.int2 + shelf.int3 + deep.int4 + limno.int5


# ====================
# = Plot Cover Types =
# ====================
axargs2 <- list(mgp=c(0.75,0.5,0), at=c(1, 2, 3, 4, 5), labels=c("land","low\nland","high\nwater","shelf","deep"))
ct.cols <- c("chartreuse4", "aquamarine3", "cyan", "cornflowerblue", "darkblue")


smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.05,0.89,0.15,0.95)
axargs <- list(mgp=c(0.75,0.5,0))


pdf(height=3, width=9.5, file="./trawl/Figures/coverType.pdf")

par(mfrow=c(1,1), mar=c(2,2,0.5,0.1), ps=8, cex=1, mgp=c(0.5,0.15,0), tcl=-0.15, family="Times")
plot(cover.type, smallplot=smplt, bigplot=bgplt, axis.args=axargs2, col=ct.cols)
invisible(map(add=TRUE, fill=FALSE, col="black", lwd=0.5))

dev.off()



# ========
# = Save =
# ========
save(cover.type, file="./trawl/Results/HadISST/cover.type.RData")

