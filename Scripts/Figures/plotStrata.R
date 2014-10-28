

library(data.table)
library(maps)

load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
source("~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions/splineHull.R")

# ================================
# = Combine 2 West Coast surveys =
# ================================
trawl[s.reg=="wcann",s.reg:="wc"]
trawl[s.reg=="wctri",s.reg:="wc"]
setkey(trawl, spp, s.reg, year)



# ===========================
# = Set Color Scheme Column =
# ===========================
reg.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(11)
trawl[,col:=reg.cols[as.factor(s.reg)]]


# ==========================
# = Set up lat/lon cushion =
# ==========================
cshn <- 5


# ==============================================================
# = Expressions for plotting sampling locations in each survey =
# ==============================================================
# For multi panel figure
map.reg <- bquote(map(regions=c("USA","Canada", "Mexico"), xlim=range(lon)+c(-cshn, cshn), ylim=range(lat)+c(-cshn, cshn), fill=TRUE, col="gray", mar=c(0.25,0.25,0.25,0.25)))
plot.ll <- bquote(points(lon, lat, pch=20, cex=0.5, col=col))

# For 1 panel full map figure
map.full <- bquote(map(regions=c("USA","Canada", "Mexico"), xlim=range(lon), ylim=range(lat), fill=TRUE, col="gray", mar=c(0.25,0.25,0.25,0.25), lwd=0.5))
ll.full <- bquote(points(lon, lat, pch=21, cex=0.25, bg=col))
# reg.hull <- bquote(trawl[,polygon(cbind(lon, lat)[chull(lon, lat),]), by="s.reg"])
# reg.hull <- bquote(trawl[,splineHull(list(x=x, y=y)), by="s.reg"])



# ==========================
# = Some helpful functions =
# ==========================
# Cut years into 9 chunks (or min(n.yrs))
cy <- function(x){
	lux <- length(unique(x))
	if(lux>1){
		as.character(cut(as.numeric(x), breaks=min(2,lux)))
	}else{
		unique(x)	
	}
}


# Scale Weight: Replicate x based on the value of y (rep rows based on wtcpue)
sw <- function(x,y, thresh=25){ # scale the abundance according to wtcpue 
	# x is a value to repeat a number of times proportional to its y value
	# a positive numeric representing the relative abundance of y
	# rep(x, y/min(y[y>0], na.rm=TRUE))
	# rep(x, y)
	rep.f0 <- y/min(y[y>0], na.rm=TRUE)
	mrf0 <- max(rep.f0, na.rm=TRUE)
	if(mrf0>thresh){
		rep.f <- rep.f0/(mrf0/thresh)
	}else{
		rep.f <- rep.f0
	}
	rep(x, pmax(1,rep.f))
}


# Map Width: Calculate appropriate figure width given height and coordinates
map.w <- function(ydat, xdat, height){
	# ydat = lat
	# xdat = lon
	# height = figure height (inches, e.g.)
	yrange <- range(ydat, na.rm=TRUE)
	xrange <- range(xdat, na.rm=TRUE)
	aspect <- c(cos((mean(yrange) * pi)/180), 1)
	d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0.01) * aspect
	w2l <- d[1]/d[2] # width to length ratio
	width <- height*w2l
	return(width)
}

# Length Unique: Convenience function for counting uniques
lu <- function(x) length(unique(x))


# ==============================
# = Plot each stratum, and all =
# ==============================
# dev.new(width=8, height=6)
pdf(width=8, height=6, file="~/Documents/School&Work/pinskyPost/trawl/Figures/stratHauls_byRegion.pdf")
par(mfrow=c(4,3), mar=c(1,1,0.5,0.5), ps=8, cex=1, family="Times", mgp=c(1, 0.25, 0), tcl=-0.15)

# Each stratum
unique(trawl, by=c("lat", "lon"))[,j={
	eval(map.reg)
	eval(plot.ll)
}, by=s.reg]

# all strata as last panel
unique(trawl, by=c("lat", "lon"))[,j={
	eval(map.reg)
	eval(plot.ll)
}]
dev.off()

# ===============================
# = Whole figure for all strata =
# ===============================
# yrange <- trawl[,range(lat)+c(-cshn,cshn)]
# xrange <- trawl[,range(lon)+c(-cshn,cshn)]
# aspect <- c(cos((mean(yrange) * pi)/180), 1)
# d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0.01) * aspect

# dev.new(height=3, width=trawl[,map.w(lat,lon,3)])
pdf(height=3, width=trawl[,map.w(lat,lon,3)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/stratHauls_full.pdf")
par(mar=c(1,1,0.5,0.5), ps=8, cex=1, family="Times", mgp=c(1, 0.25, 0), tcl=-0.15)
unique(trawl, by=c("lat", "lon"))[,j={
	eval(map.full)
	eval(plot.ll)
}]

# Add region outlines
uni.trawl.points <- unique(trawl, by=c("lat", "lon"))
setnames(uni.trawl.points, c("lat","lon"), c("y","x"))
uni.trawl.points[,splineHull(list(x=x, y=y), aval=ifelse(unique(s.reg)%in%c("ai","sa","wc"),10,1)), by="s.reg"]
dev.off()

# ==================================================
# = Trim data for plotting spatial catch over time =
# ==================================================
# Trim data
long.spp <- trawl[taxLvl=="Species"&is.finite(wtcpue)&wtcpue>0,][,n.yrs:=lu(year), by=c("spp","s.reg")][n.yrs>=4,]
long.spp[,cut.yrs:=cy(year), by=c("spp","s.reg")]
long.space.spp <- long.spp[,min.locs.yr:=min(colSums(table(paste(lat,lon),year)>0)),by=c("spp","s.reg")][min.locs.yr>=2 & is.finite(wtcpue),]

s.reg.key <- c(
	"ai"="Aleutian Islands", 
	"ebs"="Eastern Bering Strait", 
	"gmex"="Gulf of Mexico", 
	"goa"="Gulf of Alaska",
	"neus"="Northeast US",
	"newf"="Newfoundland",
	"ngulf"="Northern Gulf of St. Lawrence",
	"sa"="South Atlantic US",
	"sgulf"="Southern Gulf of St. Lawrence",
	"shelf"="Scotian Shelf",
	"wcann"="West Coast US (annual)",
	"wctri"="West Coast US (triennial)",
	"wc"= "West Coast US (combined annual & triennial)"
)

# ===================================
# = Spp in Space & Time: Expression =
# ===================================
spp.plot <- bquote({ # Create a back-quoted expression
	par(mfrow=c(2,1), mar=c(1.1,1.1,1.1,1.1), oma=c(1,1,3,1), mgp=c(1.5,0.05,0), tcl=-0.15, ps=8, cex=1, family="Times") # graphical parameters
	.SD[, # .SD is the data.table created for each group of "by" – i.e., it's the data.table in the current scope
		j={ # within the current scope's data.table (operating on a data.table that is a subset of a data table)
			print(paste("min locs per year =", min(colSums(table(paste(lat,lon),year)>0)), "for", unique(cut.yrs), spp, s.reg)) # give some progress info
			# dcs <- densCols(lon,lat, colramp=colorRampPalette(c("black","white")))
			
			# next few lines set up color values for points
			dcs <- densCols(sw(lon, wtcpue),sw(lat, wtcpue), colramp=colorRampPalette(c("black","white"))) # calculate color
			dfDens <- col2rgb(dcs)[1,] + 1L
			zols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256) # create color gradient
			dfCol <- zols[dfDens] # change black/white scale to color scale
			
			# add points where this spp in this region was caught, with points being colored according to wtcpue
			plot(sw(lon, wtcpue),sw(lat, wtcpue), cex=0.5, col=dfCol, pch=19, xlab="", ylab="") 
			
			# add filled map polygon
			map(add=TRUE, fill=TRUE, col="gray",mar=c(0.5,0.5,0.5,0.5)) 
			
			# create the short time series plot
			# have to set the ylim so that min/max wtcpue's are same across all panels
			par(new=TRUE) # next plot will be on its own x-y scale
			t.ts0 <- aggregate(list(wtcpue2=wtcpue),list(stratum2=stratum,year2=year),mean, na.rm=TRUE)
			t.ts <- aggregate(t.ts0[,"wtcpue2"], list(t.ts0[,"year2"]), sum, na.rm=TRUE)
			plot(t.ts, col="aliceblue", type="l", ylim=wt.yl2, xaxt="n", yaxt="n", xlab="", ylab="", lwd=3)
			lines(t.ts, col="red", type="l")
			axis(side=3, col.ticks="red", col="red")
			axis(side=4, col.ticks="red", col="red")
		}, 
		by="cut.yrs" # cut.yrs are groups of ~4 yr periods
	]
	
	# Label axes
	mtext("Longitude", outer=TRUE, side=1, font=2)
	mtext("Latitude", outer=TRUE, side=2, font=2)
	mtext("Year", outer=TRUE, side=3, font=2)
	mtext("Weight CPUE", outer=TRUE, side=4, font=2)
	
	# Add label in top outer margin for spp names and location
	t.cmn <- ifelse(is.na(unique(common)), "?", unique(common)) # grab the common name; if it's NA, just use a "?"
	t.spp <- unique(spp) # grab latin name
	t.reg <- s.reg.key[unique(s.reg)] # the short region name
	mtext(paste(t.spp," (", t.cmn, ")", sep=""), outer=TRUE, line=2, side=3, cex=1.2) # put the latin/ common names in the top outer margin
	mtext(t.reg, outer=TRUE, line=1, side=3) # put the region name in the top outer margin, but a little lower than spp names	
})

# ============================
# = Actually do the spp plot =
# ============================
pdf("~/Documents/School&Work/pinskyPost/trawl/Figures/catch.SpaceTime.pdf", width=4, height=7)
long.space.spp[, 
	j={
		xl <- .SD[, range(lon)]
		yl <- .SD[, range(lat)]
		
		ll.r <- c(diff(xl), diff(yl))
		
		max.ll <- max(ll.r)
		map.yl <- mean(yl) + c(-max.ll, max.ll)/2
		map.xl <- mean(xl) + c(-max.ll, max.ll)/2
		
		wt.yl <- .SD[,range(aggregate(wtcpue,list(year),sum, na.rm=TRUE)[[2]])]
		
		wt.yl2 <- .SD[,
			{
				t.ts0.yl <- aggregate(list(wtcpue2=wtcpue),list(stratum2=stratum,year2=year),mean, na.rm=TRUE)
				range(aggregate(t.ts0.yl[,"wtcpue2"], list(t.ts0.yl[,"year2"]), sum, na.rm=TRUE)[[2]])
			}
		]
		
		
		eval(spp.plot)
	},
	by=c("spp","s.reg")
]
dev.off()



