

# =================
# = Load Packages =
# =================
library(data.table)


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





# ================
# = Load Results =
# ================
# First, load data for referencing
load("./trawl/Data/MSOM/basic.dat.RData") # need basic.dat[[2]] for the region-year key to the rbo output list. Need dimnames(basic.dat[[1]])[2] in order to figure out which strata are in the rows of Z[,,,], and need dimnames(basic.dat[[1]])[3] to get the species (but remember the last 500 are just the augmented 0's)

if("rbo.RData"%in%list.files("./trawl/Results/Richness/")){
	load("./trawl/Results/Richness/rbo.RData")
}else{
	load("./trawl/Results/Richness/richness.basic.out.RData")
	load("./trawl/Results/Richness/last.out.RData")
	
	# NOTE: I screwed up the combine function for the run of richness.basic.R on 06-Feb-2015. I also forgot to run the last region-year combo. So for the first 274 I had to manually edit their combination, and only bothered with the sims.list (everything else is a summary of that anyway). Then when I noticed that I didn't run #275, I ran it by itself, called it last.out, and am adding it on here. I'm saving this as rbo.RData. So that richness.basic.out.RData is super messed up!

	rbo.11 <- richness.basic.out[[11]] # UGH
	rbo.11.1 <- unlist(rbo.11[[1]], F)

	# I got last out manually
	rbo <- c(rbo.11.1, unlist(rbo.11[-1], F), last.out[[11]])
	# save(rbo, file="./trawl/Results/Richness/rbo.RData", compress="xz")
	
}


# ===============================================
# = Need to redo prep.basic to drop same strata =
# ===============================================
prep.basic <- function(t.dat){	
	keep.strat <- apply(t.dat, 1, function(x)!all(is.na(x))) # which strata were never sampled?
	t.dat <- t.dat[keep.strat,,] # remove strata that were never sampled
	return(t.dat)

}


# =========
# = Get N =
# =========
rbo.N0 <- unlist(lapply(rbo, function(x)mode(x$N)), use.names=FALSE)
rbo.N <- basic.dat[[2]][,N:=rbo.N0]
rbo.N[,n.slope:=(lm(N~as.numeric(year))$coeff[2]), by="s.reg"]


# ==========
# = Plot N =
# ==========
# dev.new(height=8, width=4)
png("./trawl/Figures/Diversity/msom.basic.N.ts.png", width=4, height=8, res=200, units="in")
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
rbo.N[,
	j={
		plot(as.numeric(year), N, type="o", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]
dev.off()


# =========
# = Get Z =
# =========
# Get the means
get.z.mu <- function(x)rowSums(apply(x$Z, c(2,3), mean))
rbo.Z0 <- lapply(rbo, get.z.mu)

# Format into data.table w/ ID info (stratum, year, region)
z1.basic.dat <- prep.basic(basic.dat[[1]][[1]])
rbo.Z.1.stratNames <- dimnames(z1.basic.dat)[[1]]
rbo.Z.1 <- data.table(basic.dat[[2]][1,], Z=rbo.Z0[[1]], stratum=rbo.Z.1.stratNames)
rbo.Z <- copy(rbo.Z.1)

for(i in 2:length(basic.dat[[1]])){
	t.z.basic.dat <- prep.basic(basic.dat[[1]][[i]])
	t.rbo.Z.stratNames <- dimnames(t.z.basic.dat)[[1]]
	t.rbo.Z <- data.table(basic.dat[[2]][i,], Z=rbo.Z0[[i]], stratum=t.rbo.Z.stratNames)
	rbo.Z <- rbind(rbo.Z, t.rbo.Z)
}


# Split stratum name into lon lat
invisible(rbo.Z[,
	j={
		LL0 <- mapply(c, strsplit(stratum, " "))
		Lons <<- as.numeric(LL0[1,])
		Lats <<- as.numeric(LL0[2,])
		# c("lon","lat") := list(Lons, Lats)
	}
])
rbo.Z[,c("lon","lat") := list(Lons, Lats)]


# =============================================
# = Create Slope of Richness for Each Stratum =
# =============================================
rbo.Z[,z.slope:=(lm(Z~as.numeric(year))$coeff[2]), by=c("s.reg","stratum")]


# =================
# = Plot Map of Z =
# =================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
rbo.Z[,z.col:=heat.cols[cut(z.slope, 256)]]

# New device
# dev.new(height=4, width=beta.var.time[,map.w(lat,lon,4)])
pdf(height=4, width=rbo.Z[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/richness_slope_stratum_Z.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
rbo.Z[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")] # set plot region
invisible(rbo.Z[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
rbo.Z[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points

# Key
rbo.Z[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
rbo.Z[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
rbo.Z[,text(-167, y=seq(30,40, length.out=4), round(seq(min(z.slope), max(z.slope), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
rbo.Z[,text(-162.5, 41.5, bquote(Richness~Trend~(spp~per~yr)))] # add label for key

dev.off()



# =================================
# = Plot Time Series of Richness  =
# =================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
rbo.Z[,z.col:=heat.cols[cut(Z, 256)]]
lim.z <- rbo.Z[,range(Z)]
lim.lon <- rbo.Z[,range(lon)]
lim.lat <- rbo.Z[,range(lat)]
rbo.Z[,year:=as.numeric(year)]
setorder(rbo.Z,year)
saveHTML(
	{
		par(mar=c(1.75,1.5,0.5,0.5), oma=rep(0.1,4), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
		ani.options(inverval=0.5)
		rbo.Z[,
			j={	
				# Figure template
				plot(lon, lat, type="n", xlab="", ylab="", ylim=lim.lat, xlim=lim.lon)
				
				# Map
				invisible(map(add=TRUE, fill=TRUE, col="lightgray")) # add map
				
				# Richness
				points(lon, lat, pch=21, bg=z.col)
				
				# Legend
				segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
				segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
				text(-167, y=seq(30,40, length.out=4), round(seq(lim.z[1], lim.z[2], length.out=4),2), adj=1, cex=1, col="black")
#
# 				# Title
				text(-162.5, 41.5, bquote(Species~Richness))
				
				# Pause between years
				ani.pause()
			},
			by=c("year")
		]
	
	},
	ani.height=400,
	ani.width=rbo.Z[,map.w(lat,lon,400)],
	image.name="Species richness over time",
	imgdir="zpngs", #"trawl/Figures/Diversity/animateZ/zpngs",
	htmlfile="speciesRichness_timeSpace.html",
	autobrowse=FALSE,
	title="Species Richness",
	description="Species richness over time and space"
)



# ====================================
# = Smooth Time Zeries of Z Richness =
# ====================================
smooZ0 <- copy(rbo.Z)
smooZ0[,c("num","N","n.slope","z.col","z.slope"):=NULL]
setkey(smooZ0, year, stratum)

# Combine west coast trawl so there aren't duplicate strata in a year
smooZ0[,year:=as.character(year)]
smooZ0[s.reg%in%c("wctri","wcann"), s.reg:="wc"]
setkey(smooZ0, year, stratum)

# Average Z for duplicate year-strata (wc 2003? had a problem with this)
smooZ02 <- smooZ0[,list(Z=mean(Z)),by=c("year","stratum","lat","lon")]

# Create all year-stratum combinations
smooZ.template <- data.table(smooZ02[,expand.grid(year=unique(as.character(year)), stratum=unique(stratum))], key=c("year","stratum"))

# Merge the year-stratum combinations into the data
smooZ03.5 <- smooZ02[smooZ.template]
setkey(smooZ03.5,stratum, year)

# Create a data.table w/ region
add.sreg <- unique(data.table(smooZ0[,list(s.reg=s.reg,stratum=stratum)],key=c("stratum")))
smooZ03 <- add.sreg[smooZ03.5]

# Create a data.table with lon, lat, and region to be added back in
setkey(smooZ03, stratum)
smooZ03.loc <- unique(smooZ03[!is.na(lon)])[,list(stratum=stratum,lon=lon,lat=lat,s.reg=s.reg)]
setkey(smooZ03, stratum, year)

# Add lon, lat, and region back in
smooZ04 <- smooZ03[,c("lon","lat","s.reg"):=NULL][smooZ03.loc]
setkey(smooZ04, year, stratum)

# Function to fill missings with the mean – for strata that weren't sampled in that year (within-region differences are small due to model structure, so this makes sense; done for visual consistency)
fill.mean <- function(x){
	if(all(is.na(x))){
		return(x)
	}else{
		x[is.na(x)] <- mean(x, na.rm=TRUE)
	}
	x
}

# Average-in missing stratum richness
smooZ04[, Z:=fill.mean(Z), by=c("s.reg","year")]

# linearly interpolate between years (not spline; legacy name)
smooZ04[, Z.spline:=approx(x=year, y=Z, xout=year)$y, by=c("stratum")]
smooZ04[is.na(Z), Z:=Z.spline]

# create final data.table, and put richness on a log scale
smooZ <- copy(smooZ04)
smooZ[,Z:=log(Z)]


# =================
# = Plot Smooth Z =
# =================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
smooZ[,z.col:=heat.cols[cut(Z, 256)]]
lim.z <- smooZ[,range(Z, na.rm=TRUE)]
lim.lon <- smooZ[,range(lon)]
lim.lat <- smooZ[,range(lat)]
smooZ[,year:=as.numeric(year)]
setorder(smooZ,year)
saveHTML(
	{
		par(mar=c(1.75,1.5,0.5,0.5), oma=rep(0.1,4), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
		ani.options(inverval=0.5)
		smooZ[,
			j={
				
				# Figure template
				plot(lon, lat, type="n", xlab="", ylab="", ylim=lim.lat, xlim=lim.lon)
								
				# Map
				invisible(map(add=TRUE, fill=TRUE, col="lightgray")) # add map
				
				# Richness
				points(lon, lat, pch=21, bg=z.col)
				
				# Legend
				segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)
				segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")
				text(-167, y=seq(30,40, length.out=4), round(seq(lim.z[1], lim.z[2], length.out=4),2), adj=1, cex=1, col="black")
#
# 				# Title
				text(-162.5, 41.5, bquote(Species~Richness))
				
				# Pause between years
				ani.pause()
			},
			by=c("year")
		]
	
	},
	ani.height=400,
	ani.width=smooZ[,map.w(lat,lon,400)],
	image.name="Species richness over time",
	imgdir="zpngs", #"trawl/Figures/Diversity/animateZ/zpngs",
	htmlfile="speciesRichness_timeSpace_smooth.html",
	autobrowse=FALSE,
	title="Species Richness",
	description="Species richness over time and space"
)




# ========================
# = Save rbo.Z and rbo.N =
# ========================
save(rbo.Z, file="./trawl/Results/Richness/rbo.Z.RData")
save(rbo.N, file="./trawl/Results/Richness/rbo.N.RData")











