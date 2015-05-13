

# ==================
# = Load Libraries =
# ==================
library(data.table)
library(maps)


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load trawl2 Dataset =
# =======================
load("./trawl/Data/trawl2.RData")


# =======================
# = Load data functions =
# =======================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load Plot Functions =
# =======================
plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load Stat Functions =
# =======================
stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# ================================
# = Combine 2 West Coast surveys =
# ================================

trawl <- trawl2 # need this to update for the big overhaul of the trawl data set and how it's organized by strata and substrata

trawl[s.reg=="wcann",s.reg:="wc"]
trawl[s.reg=="wctri",s.reg:="wc"]
setkey(trawl, spp, s.reg, year)

# ==================================================
# = Trim data for plotting spatial catch over time =
# ==================================================
# Trim data
long.spp <- trawl[taxLvl=="Species",][,n.yrs:=lu(year[wtcpue>0]), by=c("spp","s.reg")][n.yrs>=4,]
long.spp[,cut.yrs:=cy(year), by=c("spp","s.reg")]
long.space.spp <- long.spp[,min.locs.yr:=min(colSums(table(stratum,year)>0)),by=c("spp","s.reg")][min.locs.yr>=4 & is.finite(wtcpue),]



s.reg.key <- c(
	"ai"="Aleutian Islands", 
	"ebs"="Eastern Bering Sea", 
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
			# par(new=TRUE) # next plot will be on its own x-y scale
			# t.ts0 <- aggregate(list(wtcpue2=wtcpue),list(stratum2=stratum,year2=year),mean, na.rm=TRUE)
			# t.ts <- aggregate(t.ts0[,"wtcpue2"], list(t.ts0[,"year2"]), sum, na.rm=TRUE)
			# plot(t.ts, col="aliceblue", type="l", ylim=wt.yl2, xaxt="n", yaxt="n", xlab="", ylab="", lwd=3)
			# lines(t.ts, col="red", type="l")
			# axis(side=3, col.ticks="red", col="red")
			# axis(side=4, col.ticks="red", col="red")
			mtext(paste(range(year), collapse=" - "), side=3, line=0.1, adj=0, font=2)
		}, 
		by="cut.yrs" # cut.yrs are groups of ~4 yr periods
	]
	
	# Label axes
	mtext("Longitude", outer=TRUE, side=1, font=2)
	mtext("Latitude", outer=TRUE, side=2, font=2)
	# mtext("Year", outer=TRUE, side=3, font=2)
	# mtext("Weight CPUE", outer=TRUE, side=4, font=2)
	
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
# pdf("~/Documents/School&Work/pinskyPost/trawl/Figures/catch.SpaceTime3.pdf", width=4, height=7)
pdf("~/Desktop/catch.SpaceTime_noLine.pdf", width=4, height=7)
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
