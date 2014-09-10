


# library(maps)
library(data.table)
library(vegan)
library(reshape2)


load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions/beta.div.R")

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
# Trim to taxa identified to species level
trawl1 <- trawl[taxLvl%in%c("Species")]

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
# save(trawl3, "~/Documents/School&Work/pinskyPost/trawl/Data/trawl3.RData")


# ====================
# = Calculate a Beta =
# ====================
# shelf.space <- c("447", "449", "450", "455", "472", "482", "451", "480", "490", "491", "492", "493", "494", "485", "495", "441", "442", "444", "458", "459", "460", "461", "462", "470", "471", "476", "477", "481", "483", "484", "445", "466", "448", "440", "443", "446", "452", "453", "454", "456", "457", "463", "464", "465", "473", "475", "478")
# slopes <- c()
# vars <- c()
# for(i in 1:length(shelf.space)){
# 	test <- trawl3[s.reg=="shelf"&stratum==shelf.space[i]]
# 	test2 <- acast(melt(test, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
# 	test3 <- vegdist(decostand(test2, method="log", logbase=5), method="altGower")
# 	t3.yr <- as.numeric(attributes(test3)$Labels)
# 	t3.delX <- dist(t3.yr, method="manhattan")
# 	plot(c(t3.delX), log(c(test3)), ylab=bquote((1-Delta*y[J])), xlab=bquote(Delta*x~(years))) # This is a scatter plot of the relationship that needs to be modeled for spatial/temporal turnover
# 	t3.mod <- lm(log(c(test3))~c(t3.delX))
# 	abline(t3.mod)
# 	summary(t3.mod)
# 	t3.beta <- t3.mod$coef[2]
# 	slopes[i] <- t3.beta
# 	vars[i] <- mean(test3)
# }


### trawl3[s.reg=="neus", sum(wtcpue>0), by=c("stratum","year")][V1==max(V1), list(stratum, year, V1)]
# test <- trawl3[s.reg=="neus"&stratum=="1100"]
# test2 <- acast(melt(test, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1] # This is a community matrix, Y
#
# blah <- beta.div(test2, nperm=0, save.D=TRUE)
# sort(blah$SCBD)
# sort(blah$LCBD)


# test3 <- vegdist(decostand(test2, method="log", logbase=5), method="altGower")
# t3.yr <- as.numeric(attributes(test3)$Labels)
# t3.delX <- dist(t3.yr, method="manhattan")

# plot(c(t3.delX), log(c(test3)), ylab=bquote((1-Delta*y[J])), xlab=bquote(Delta*x~(years))) # This is a scatter plot of the relationship that needs to be modeled for spatial/temporal turnover
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
		d.helli0 <- beta.div(castExp, nperm=0, save.D=TRUE)$D
		d.helli <- c(d.helli0)

		
		dX.yr <- dist(as.numeric(attributes(d.helli0)$Labels), method="euclidean")
		
		good.y1 <- d.helli>0 # figure out which indices would throw error if took log
		dy1 <- log(d.helli[good.y1])
		dX <- c(dX.yr)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		decay.slope
		}else{
			as.numeric(NA)
		}

})



# beta.turn.time <- trawl3[,list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr)), by=c("s.reg","stratum")]
beta.turn.time <- trawl3[,
	j={
		list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr))
	}, 
	
	by=c("s.reg","stratum")
]
beta.turn.time <- beta.turn.time[!is.na(turn.time)&turn.time>0,]
beta.turn.time[,turn.time:=log(turn.time)]

setkey(beta.turn.time, s.reg, stratum)





# ==========================
# = Beta temporal variance =
# ==========================
beta.var.time.expr <- bquote({
	castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
	beta.div(castExp, nperm=0)[[1]][2]
})

beta.var.time <- trawl3[,
	j={
		var.time0 <- eval(beta.var.time.expr)
		list(
			lat=mean(lat),
			lon=mean(lon),
			var.time = var.time0
		)
	}, 
	by=c("s.reg","stratum")
]
beta.var.time <- beta.var.time[!is.na(var.time),]
setkey(beta.var.time, s.reg, stratum)









# =========================
# = Beta spatial turnover =
# =========================
beta.turn.space.expr <- bquote({	
	if(lu(stratum)>3){
		castExp <- acast(melt(.SD, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
		d.helli0 <- beta.div(castExp, nperm=0, save.D=TRUE)$D
		d.helli <- c(d.helli0)

		mu.ll <- .SD[,list(lon.mu=mean(lon), lat.mu=mean(lat)), by="stratum"]
		dX.ll <- mu.ll[,dist(matrix(c(lon.mu,lat.mu),ncol=2), method="euclidean")]
		
		good.y1 <- d.helli>0 # figure out which indices would throw error if took log
		dy1 <- log(d.helli[good.y1])
		dX <- c(dX.ll)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		decay.slope
		}else{
			as.numeric(NA)
		}
})

beta.turn.space <- trawl3[,list(lon=mean(lon), lat=mean(lat), turn.space=eval(beta.turn.space.expr)), by=c("s.reg","year")]
setkey(beta.turn.space, s.reg, year)






# =========================
# = Beta spatial variance =
# =========================
# Create expression
beta.var.space.expr <- bquote({
	castExp <- acast(melt(.SD, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
	beta.div(castExp, nperm=0)[[1]][2]


})

# Call expression and do spatial variance beta D analysis
beta.var.space <- trawl3[,
	j={
		list(
			var.space=eval(beta.var.space.expr)
		)
	}, 
	by=c("s.reg","year")
]

setkey(beta.var.space, s.reg, year)



# # look at cod again
# dev.new()
# par(mfrow=c(2,2))
# setkey(trawl3, s.reg, spp, year)
# trawl3[spp=="Gadus morhua", {plot(aggregate(wtcpue, list(year=year), mean), ylab=s.reg, type="l"); abline(v=1986)}, by="s.reg"]


# ====================================
# = Compare Spatial/Temporal Indices =
# ====================================
# See Mellin et al. Proc R. Soc. B "Strong but opposing β-diversity–stability relationships in coral reef fish communities"

# temporal.var <- beta.var.time[,list(var.time=mean(var.time)), by="s.reg"]
# spatial.var <- beta.var.space[,list(var.space=mean(var.space)), by="s.reg"]
#
# temporal.turn <- beta.turn.time[,list(turn.time=mean(turn.time)), by="s.reg"]
# spatial.turn <- beta.turn.space[, list(turn.space=mean(turn.space)), by="s.reg"]
#
# plot(spatial.turn[,turn.space], temporal.turn[,turn.time])
#
# plot(spatial.var[,var.space], temporal.var[,var.time])
#
#
# beta.temporal <- merge(beta.var.time[,list(s.reg,stratum,lon,lat,var.time)], beta.turn.time[,list(s.reg,stratum,lon,lat,turn.time)], by=c("s.reg","stratum","lon","lat"))
# beta.temporal[,plot((var.time), (turn.time))]
#
# beta.spatial <- merge(beta.var.space, beta.turn.space, by=c("s.reg", "year"))
# beta.spatial[,plot((var.space), (turn.space))]




# ===========================================================
# = How much did each year contribute to temporal variance? =
# ===========================================================
# beta.var.time.lcbd.expr <- bquote({
# 	castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
# 	beta.div(castExp, nperm=0)$LCBD
# })
#
# beta.var.time.lcbd <- trawl3[,
# 	j={
# 		lcbd <- eval(beta.var.time.lcbd.expr)
# 		list(
# 			year=names(lcbd),
# 			year.lcbd=lcbd
#
# 		)
# 	},
# 	by=c("s.reg","stratum")
# ]
# # beta.var.time.lcbd <- beta.var.time.lcbd[!is.na(var.time),]
# setkey(beta.var.time.lcbd, s.reg, stratum, year)
#
# dev.new(width=5, height=7)
# par(mfrow=c(5,2), mar=c(1.75,1.5,1,1), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
# beta.var.time.lcbd[,
# 	{
# 		plot(aggregate(year.lcbd, list(year=year), mean), type="l", main=s.reg, xlab="", ylab="")
# 		# par(new=TRUE)
# 		# plot(year, var.space.ID.only, type="l", xaxt="n", xlab="", yaxt="n", ylab="", col="red")
# 		# axis(side=4, col="red")
# 		},
# 	by="s.reg"
# ]




# ===========================================================
# = Which fish contributed the most to changes in beta div? =
# ===========================================================
# beta.var.time.scbd.expr <- bquote({
# 	castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
# 	beta.div(castExp, nperm=0)$SCBD
# })
#
# beta.var.time.scbd <- trawl3[,
# 	j={
# 		scbd <- rev(sort(eval(beta.var.time.scbd.expr)))
# 		list(
# 			taxon=names(scbd)[1:4],
# 			taxon.scbd=scbd[1:4]
#
# 		)
# 	},
# 	by=c("s.reg","stratum")
# ]
# # beta.var.time.lcbd <- beta.var.time.lcbd[!is.na(var.time),]
# setkey(beta.var.time.scbd, s.reg, stratum, taxon)
#
#
#
# beta.var.time.scbd2 <- beta.var.time.scbd[,mean(taxon.scbd), by=c("s.reg","taxon")]


save(beta.var.space, beta.var.time, beta.turn.space, beta.turn.time, file="~/Documents/School&Work/pinskyPost/trawl/Results/trawl.betaD.RData")



# ====================================================
# = Calculate temporal variability for whole regions =
# ====================================================

