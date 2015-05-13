

# Scale Weight: Replicate x based on the value of y (rep rows based on wtcpue)
sw <- function(x,y, y.range=NULL, thresh=25){ # scale the abundance according to wtcpue 
	# x is a value to repeat a number of times proportional to its y value
	# a positive numeric representing the relative abundance of y
	# rep(x, y/min(y[y>0], na.rm=TRUE))
	# rep(x, y)
	
	if(is.null(y.range)){
		y.min <- min(y[y>0], na.rm=TRUE)
	}else{
		y.min <- y.range[1]
		if(y.min<=0){y.min <- 0.001}
	}
	
	
	if(is.null(y.range)){
		y.max <- max(y, na.rm=TRUE)
	}else{
		y.max <- y.range[2]
	}
	
	
	
	rep.f0 <- y/y.min # y/min(y[y>0], na.rm=TRUE)
	mrf0 <- y.max/y.min #max(rep.f0, na.rm=TRUE)
	if(mrf0>thresh){
		rep.f <- rep.f0/(mrf0/thresh)
	}else{
		rep.f <- rep.f0
	}
	rep(x, pmax(1,rep.f))
}


add.alpha <- function(col, alpha=1){
	if(missing(col)){
		stop("Please provide a vector of colours.")
	}
	apply(sapply(col, col2rgb)/255, 2, function(x)rgb(x[1],x[2],x[3],alpha=alpha))
}



map.catch <- function(x, y, z, main=NULL, ylim=range(y, na.rm=TRUE), xlim=range(x, na.rm=TRUE), zlim=range(z, na.rm=TRUE), ...){
# map.catch <- function(x, y, z, main=NULL, ylim, xlim, zlim, ...){
	
	stopifnot(require(maps))
	
	dots <- list(...)
	dots.args <- names(dots)
	
	
	# Colors
	zols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256) # create color gradient


	bg.gray <- add.alpha("gray", 0.5)
	
	# =========================================================
	# = Approach 1: replicate rows based on the biomass value =
	# =========================================================
	plot.data <- data.frame(x=sw(x, z, y.range=zlim), y=sw(y,z, y.range=zlim)) # create data frame w/ x or y observations being replicated based on z value
	
	# next few lines set up color values for points
	dcs <- densCols(plot.data[,"x"], plot.data[,"y"], colramp=colorRampPalette(c("black","white"))) # calculate color
	dfDens <- col2rgb(dcs)[1,] + 1L
	dfCol <- zols[dfDens] # change black/white scale to color scale
	
	plot.data[,"col"] <- dfCol# add colors to the data frame
	plot.data <- plot.data[order(dfDens),] # rearrange all values so that the warmest values are at the bottom, and thus will be plotted after cooler colors
	
	
	
	# Change the xlim or ylim so that the map is a square (of course the usefulness of this hinges on the plotting region being square ...)
	dx <- diff(xlim)
	dy <- diff(ylim)
	
	if(dx > dy){
		ylim <- ylim + c(-0.5,0.5)*(dx-dy)
	}else{
		xlim <- xlim + c(-0.5,0.5)*(dy-dx)
	}
	
	
	# add points where this spp in this region was caught, with points being colored according to wtcpue
	plot(xlim, ylim, xlab="", ylab="", xlim=xlim, ylim=ylim, type="n") 
	
	# add filled map polygon
	map(add=TRUE, fill=TRUE, col=bg.gray,mar=c(0.5,0.5,0.5,0.5)) 
	
	# add points where this spp in this region was caught, with points being colored according to wtcpue
	points(plot.data[,c("x","y")], cex=1, col=plot.data[,"col"], pch=19) 
	
	
	x0 <- xlim[2] - 0.15*diff(xlim)
	x1 <- x0 - 0.2*(0.15*diff(xlim))
	x.tick <- x1 - 0.5*diff(sort(c(x0,x1)))
	x.lab <- x.tick - 0.25*diff(sort(c(x0,x1)))
	y0 <- ylim[1] + 0.1*diff(ylim)
	y1 <- ylim[2] - 0.5*diff(ylim)
	
	# Add a Key
	segments(x0=x0, x1=x1, y0=seq(y0,y1,length.out=256), col=zols) # add colors for key
	segments(x0=x.tick, x1=x1, y0=seq(y0,y1, length.out=4), col="black") # add tick marks for key
	text(x.lab, y=seq(y0,y1, length.out=4), round(seq(zlim[1], zlim[2], length.out=4),2), adj=1, cex=0.8, col="black") # add labels for key
	text(mean(c(x0,x1)), ylim[2] - 0.4*diff(ylim), bquote(underline(Biomass))) # add label for key
	if(!is.null(main)){
		mtext(main, side=3, line=0.1, outer=FALSE)
	}
	
	
	# ===============================================================================
	# = Approach 2: bin, sum, then assign colors based on values, not pixel density =
	# ===============================================================================
	
	
	
	
	
}




# ===========
# = Example =
# ===========
# load("/Users/Battrd/Downloads/pewmaps_Apr 28.rdata") #pew.maps object
# head(pew.maps)
# pm.keys <- c("STRATA", "SPECIESCOMMONNAME", "SERIES") # columns to key on
# setkeyv(pew.maps, pm.keys) # set key for pew.maps
#
# (pm.uniques <- apply(pew.maps, 2, function(x)length(unique(x)))) # the number of unique values in each of those columns
# print(nrow(pew.maps)) # number of rows in our data set
# print(prod(pm.uniques[pm.keys])) # the number of rows we'd have if the data were fully crossed – i.e., all possible combinations of those identifying factors
# # OK, so data are already fully crossed
#
#
#
# # test <- pew.maps[SPECIESCOMMONNAME=="BANDED DRUM" & SERIES == "1990-94",]
# # y <- test[,LATITUDE]
# # x <- test[,LONGITUDE]
# # z <- test[,BIOMASS]
# # map.catch(x,y,z, main="BANDED DRUM")
#
#
# pdf("~/Desktop/jim.pew.maps.pdf", width=3.5, height=6.25)
#
# pew.maps[,
# 	j={
# 		t.spp <- SPECIESCOMMONNAME[1]
# 		t.zlim <- .SD[,range(BIOMASS, na.rm=TRUE)]
# 		t.ylim <- .SD[,range(LATITUDE, na.rm=TRUE)]
# 		t.xlim <- .SD[,range(LONGITUDE, na.rm=TRUE)]
#
# 		par(mfrow=c(2,1), mar=c(1.75,1.75,1,0.1), oma=c(0,0,0,0), mgp=c(1.5,0.25,0), tcl=-0.15, ps=10, cex=1)
#
# 		.SD[,
# 			j={
#
# 				main <- paste(t.spp, SERIES[1])
# 				map.catch(LONGITUDE,LATITUDE,BIOMASS, main=main, ylim=t.ylim, xlim=t.xlim, zlim=t.zlim)
#
# 			},
# 			by=c("SERIES")
# 		]
# 	},
# 	by=c("SPECIESCOMMONNAME")
#
# ]
#
#
# dev.off()
#





