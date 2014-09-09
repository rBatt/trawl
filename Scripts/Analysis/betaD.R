
library(maps)
library(data.table)
library(vegan)
library(reshape2)


load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")

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


# ================================
# = Combine 2 West Coast surveys =
# ================================
trawl[s.reg=="wcann",s.reg:="wc"]
trawl[s.reg=="wctri",s.reg:="wc"]

# ==================
# = Begin trimming =
# ==================
# Trim to Genus or Spp
trawl1 <- trawl[taxLvl%in%c("Genus","Species")]

# Drop rows w/ no wtcpue
trawl1 <- trawl1[is.finite(wtcpue)&wtcpue>0,]

# Add # years observed
lu <- function(x) length(unique(x))
trawl1[,n.yrs:=lu(year), by=c("spp","s.reg")]

# Add column for total weight of a species in a stratum in a year (aggregate across multiple hauls)
trawl1[,sumWtStrat:=sum(wtcpue), by=c("spp","year","stratum","s.reg")]



# =====================================
# = Aggregate within stratum-year-spp =
# =====================================
# Avg by wtcpue
wtAvg <- function(x,y){
	# x is something like temperature (the value to be averaged)
	# y is something like wtcpue (the value to be used for weighting)
	totW <- sum(y[is.finite(x)])
	propW <- y/totW
	sumna(x*propW)
}

setkey(trawl1, s.reg, spp, common, year, stratum)
trawl2 <- trawl1[,list(lat=wtAvg(as.numeric(lat), wtcpue), lon=wtAvg(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)]


# =========================================
# = Pad to regular ts for spp in a strata =
# =========================================
# Pad so that all unique spp in a stratum have a row each year for that stratum
# Added rows are 0's for wtcpue (0 wt per effort)
# Does not include adding NA's for missing years (i.e., if no sampling occurred that year)
allSpp <- trawl2[,CJ(spp=unique(spp), year=unique(year)), by=c("s.reg","stratum")]
setkey(allSpp)


# Fill the NA values of a vector with the mean of the non-NA portion
fill.mean <- function(x){
	nai <- is.na(x)
	x[nai] <- meanna(x)
	x
}

trawl3 <- merge(allSpp, trawl2, all=TRUE)
trawl3[is.na(wtcpue), wtcpue:=0]
trawl3[, c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg","stratum")]
trawl3[, c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]


# =======================
# = Free up some memory =
# =======================
rm(list=c("trawl","trawl1"))




# ====================
# = Calculate a Beta =
# ====================
# # trawl3[s.reg=="neus", sum(wtcpue>0), by=c("stratum","year")][V1==max(V1), list(stratum, year, V1)]
# test <- trawl3[s.reg=="neus"&stratum=="1100"]
# test2 <- acast(melt(test, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
# test3 <- vegdist(test2, method="jaccard")
# t3.yr <- as.numeric(attributes(test3)$Labels)
# t3.delX <- dist(t3.yr, method="manhattan")
# plot(c(t3.delX), log(1-c(test3)), ylab=bquote((1-Delta*y[J])), xlab=bquote(Delta*x~(years))) # This is a scatter plot of the relationship that needs to be modeled for spatial/temporal turnover
# t3.mod <- lm(log(1-c(test3))~c(t3.delX))
# abline(t3.mod)
# summary(t3.mod)
# t3.beta <- t3.mod$coef[2]



# ==========================
# = Beta temporal turnover =
# ==========================
beta.turn.time.expr <- bquote({
	# print(paste(s.reg, stratum))
	# print(.SD)
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
		# d.jac <- vegdist(castExp, method="jaccard")
		d.jac <- vegdist(decostand(castExp, method="log", logbase=Inf), method="jaccard")
		# print(paste(s.reg, stratum, sum((1-d.jac)<=0|!is.finite(1-d.jac))))
		dX.yr <- dist(as.numeric(attributes(d.jac)$Labels), method="manhattan")
		# d.jac1 <- 1-c(d.jac) # change the distance matrix into the (1-DeltaY) vector
		# good.y1 <- d.jac1>0 # figure out which indices would throw error if took log
		d.jac1 <- c(d.jac) # change the distance matrix into the (1-DeltaY) vector
		good.y1 <- d.jac1>0 # figure out which indices would throw error if took log
		dy1 <- log(d.jac1[good.y1])
		dX <- c(dX.yr)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		decay.slope
		}else{
			as.numeric(NA)
		}

})

beta.turn.time <- trawl3[,list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr)), by=c("s.reg","stratum")]
beta.turn.time <- beta.turn.time[!is.na(turn.time),]


heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

beta.turn.time[,turn.time.col:=heat.cols[cut(rank(turn.time, na.last="keep"), 256)]]
# beta.turn.time[,turn.time.col:=heat.cols[cut(exp(pmin(turn.time,0)), 256)]]


dev.new(height=4, width=beta.turn.time[,map.w(lat,lon,4)])
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")
# par(mfrow=c(4,3))

beta.turn.time[,plot(lon, lat, col=turn.time.col, pch=21, cex=1, type="n")]

# beta.turn.time[,map(xlim=range(lon, na.rm=TRUE), ylim=range(lat, na.rm=TRUE),add=FALSE, type="n")]
beta.turn.time[,map(add=TRUE, fill=FALSE, col="black")]
# map.axes()

beta.turn.time[,points(lon, lat, col=turn.time.col, pch=21, cex=1)]

beta.turn.time[,key.lat:=seq(30,40,length.out=256)[cut(rank(turn.time, na.last="keep"), 256)]]
beta.turn.time[,segments(x0=-165, x1=-160, y0=key.lat, col=turn.time.col)]

beta.turn.time[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
beta.turn.time[,text(-167, y=seq(30,40, length.out=4), round(quantile(turn.time, na.rm=TRUE, probs=seq(0,1,length.out=4)),2), adj=1, cex=1, col="black")]

# beta.turn.time[,text(-162.5, 42.5, bquote(over({log[e](Dissimilarity)},{Year})))]
beta.turn.time[,text(-162.5, 41.5, bquote(Temporal~Turnover))]




# ==========================
# = Beta temporal variance =
# ==========================
beta.var.time.expr <- bquote({
	castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]

	mat.stand <- decostand(castExp, method="log", logbase=2)
	mat.stand2 <- decostand(castExp, method="log", logbase=Inf)
	# Note:
	# Will throw warning: non-integer data: divided by smallest positive value
	# This is because it expects counts of species, but units are in biomass per effort
	# So the function is dividing all values in that row by smallest positive value so that it's 1

	c(mean(vegdist(mat.stand, method="altGower"), na.rm=TRUE), mean(vegdist(mat.stand2, method="altGower"), na.rm=TRUE))
})

beta.var.time <- trawl3[,
	j={
		var.time0 <- eval(beta.var.time.expr)
		list(
			lat=mean(lat),
			lon=mean(lon),
			var.time.ID.abun=var.time0[1],
			var.time.ID.only=var.time0[2]
		)
	}, 
	by=c("s.reg","stratum")
]
setkey(beta.var.time, s.reg, stratum)



heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

beta.var.time[,var.time.col:=heat.cols[cut(rank(var.time.ID.abun, na.last="keep"), 256)]]
# beta.var.time[,var.time.col:=heat.cols[cut(exp(pmin(var.time,0)), 256)]]


dev.new(height=4, width=beta.var.time[,map.w(lat,lon,4)])
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")
# par(mfrow=c(4,3))

beta.var.time[,plot(lon, lat, col=var.time.col, pch=21, cex=1, type="n")]

# beta.var.time[,map(xlim=range(lon, na.rm=TRUE), ylim=range(lat, na.rm=TRUE),add=FALSE, type="n")]
beta.var.time[,map(add=TRUE, fill=FALSE, col="black")]
# map.axes()

beta.var.time[,points(lon, lat, col=var.time.col, pch=21, cex=1)]

beta.var.time[,key.lat:=seq(30,40,length.out=256)[cut(rank(var.time.ID.abun, na.last="keep"), 256)]]
beta.var.time[,segments(x0=-165, x1=-160, y0=key.lat, col=var.time.col)]

beta.var.time[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
beta.var.time[,text(-167, y=seq(30,40, length.out=4), round(quantile(var.time.ID.abun, na.rm=TRUE, probs=seq(0,1,length.out=4)),2), adj=1, cex=1, col="black")]

beta.var.time[,text(-162.5, 41.5, bquote(Temporal~Variance))]








# =========================
# = Beta spatial turnover =
# =========================
beta.turn.strat.expr <- bquote({
	castExp <- acast(melt(.SD, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
	d.jac <- vegdist(decostand(castExp, method="log", logbase=Inf), method="jaccard")
	
	# lonlat <- merge(.SD[,list(stratum)], .SD[,list(stratum,lon,lat)], all.x=TRUE, by="stratum")
	# dX.ll <- dist(matrix(c(lon,lat),ncol=2), method="manhattan")
	mu.ll <- .SD[,list(lon.mu=mean(lon), lat.mu=mean(lat)), by="stratum"]
	# print(mu.ll)
	dX.ll <- mu.ll[,dist(matrix(c(lon.mu,lat.mu),ncol=2), method="manhattan")]
	# print(dX.ll)
	
	d.jac1 <- c(d.jac) # change the distance matrix into the (1-DeltaY) vector
	good.y1 <- d.jac1>0 # figure out which indices would throw error if took log
	dy1 <- log(d.jac1[good.y1])
	dX <- c(dX.ll)[good.y1]
	
	decay.slope <- lm(dy1~dX)$coef[2]
	decay.slope
})

beta.turn.strat <- trawl3[,list(lon=mean(lon), lat=mean(lat), turn.strat=eval(beta.turn.strat.expr)), by=c("s.reg","year")]
beta.turn.strat <- beta.turn.yr[!is.na(turn.yr),]
setkey(beta.turn.strat, s.reg, year)

dev.new()
par(mfrow=c(4,3), mar=c(1.75,1.5,1,1), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.turn.strat[,
	{
		plot(year, turn.strat, type="l", main=s.reg, xlab="", ylab="")
		# par(new=TRUE)
		# plot(year, var.strat.ID.only, type="l", xaxt="n", xlab="", yaxt="n", ylab="", col="red")
		# axis(side=4, col="red")
		},
	by="s.reg"
]





# =========================
# = Beta spatial variance =
# =========================
beta.var.strat.expr <- bquote({
	castExp <- acast(melt(.SD, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]

	mat.stand <- decostand(castExp, method="log", logbase=2)
	mat.stand2 <- decostand(castExp, method="log", logbase=Inf)
	# Note:
	# Will throw warning: non-integer data: divided by smallest positive value
	# This is because it expects counts of species, but units are in biomass per effort
	# So the function is dividing all values in that row by smallest positive value so that it's 1

	c(mean(vegdist(mat.stand, method="altGower"), na.rm=TRUE), mean(vegdist(mat.stand2, method="altGower"), na.rm=TRUE))
})

beta.var.strat <- trawl3[,
	j={
		var.strat0 <- eval(beta.var.strat.expr)
		list(
			var.strat.ID.abun=var.strat0[1],
			var.strat.ID.only=var.strat0[2]
		)
	}, 
	by=c("s.reg","year")
]

setkey(beta.var.strat, s.reg, year)


dev.new()
par(mfrow=c(4,3), mar=c(1.75,1.5,1,1), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.var.strat[,
	{
		plot(year, var.strat.ID.abun, type="l", main=s.reg, xlab="", ylab="")
		# par(new=TRUE)
		# plot(year, var.strat.ID.only, type="l", xaxt="n", xlab="", yaxt="n", ylab="", col="red")
		# axis(side=4, col="red")
		},
	by="s.reg"
]




# look at cod again
# dev.new()
# par(mfrow=c(2,2))
# setkey(trawl3, s.reg, spp, year)
# trawl3[spp=="Gadus morhua", {plot(aggregate(wtcpue, list(year=year), mean), ylab=s.reg, type="l"); abline(v=1986)}, by="s.reg"]









