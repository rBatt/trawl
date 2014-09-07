
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
# trawl3[s.reg=="neus", sum(wtcpue>0), by=c("stratum","year")][V1==max(V1), list(stratum, year, V1)]
test <- trawl3[s.reg=="neus"&stratum=="1100"]
test2 <- acast(melt(test, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
test3 <- vegdist(test2, method="jaccard")
t3.yr <- as.numeric(attributes(test3)$Labels)
t3.delX <- dist(t3.yr, method="manhattan")
plot(c(t3.delX), log(1-c(test3)), ylab=bquote((1-Delta*y[J])), xlab=bquote(Delta*x~(years))) # This is a scatter plot of the relationship that needs to be modeled for spatial/temporal turnover
t3.mod <- lm(log(1-c(test3))~c(t3.delX))
abline(t3.mod)
summary(t3.mod)
t3.beta <- t3.mod$coef[2]


# ======================================================================
# = Beta turnover across years w/in stratum (temporal autocorrelation) =
# ======================================================================
beta.turn.yr.expr <- bquote({
	# print(paste(s.reg, stratum))
	# print(.SD)
	if(lu(year)>10){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
		d.jac <- vegdist(castExp, method="jaccard")
		# print(paste(s.reg, stratum, sum((1-d.jac)<=0|!is.finite(1-d.jac))))
		dX.yr <- dist(as.numeric(attributes(d.jac)$Labels), method="manhattan")
		d.jac1 <- 1-c(d.jac) # change the distance matrix into the (1-DeltaY) vector
		good.y1 <- d.jac1>0 # figure out which indices would throw error if took log
		dy1 <- log(d.jac1[good.y1])
		dX <- c(dX.yr)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		decay.slope
		}else{
			as.numeric(NA)
		}

})


beta.turn.yr <- trawl3[,list(lon=mean(lon), lat=mean(lat), turn.yr=eval(beta.turn.yr.expr)), by=c("s.reg","stratum")]
beta.turn.yr <- beta.turn.yr[!is.na(turn.yr),]


heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

beta.turn.yr[,turn.yr.col:=heat.cols[cut(rank(-turn.yr, na.last="keep"), 256)]]
# beta.turn.yr[,turn.yr.col:=heat.cols[cut(exp(pmin(turn.yr,0)), 256)]]


dev.new(height=5, width=beta.turn.yr[,map.w(lat,lon,5)])
par(mar=c(1.5,1.5,0.5,0.5), mgp=c(1,0.5,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")
# par(mfrow=c(4,3))

beta.turn.yr[,plot(lon, lat, col=turn.yr.col, pch=21, cex=1)]

# beta.turn.yr[,map(xlim=range(lon, na.rm=TRUE), ylim=range(lat, na.rm=TRUE),add=FALSE, type="n")]
beta.turn.yr[,map(add=TRUE, fill=FALSE, col="black")]
# map.axes()

beta.turn.yr[,points(lon, lat, col=turn.yr.col, pch=21, cex=1)]

beta.turn.yr[,key.lat:=seq(30,40,length.out=256)[cut(rank(-turn.yr, na.last="keep"), 256)]]
beta.turn.yr[,segments(x0=-165, x1=-160, y0=key.lat, col=turn.yr.col)]

beta.turn.yr[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
beta.turn.yr[,text(-167, y=seq(30,40, length.out=4), round(quantile(-turn.yr, na.rm=TRUE, probs=seq(0,1,length.out=4)),2), adj=1, cex=1, col="black")]











