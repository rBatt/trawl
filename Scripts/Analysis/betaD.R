

# ==================
# = Load Libraries =
# ==================
# library(maps)
library(data.table)
library(vegan)
library(reshape2)

# =========================
# = Load Data and Scripts =
# =========================
# Load trawl data
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")

# Load Legendre beta diversity functions
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions/beta.div.R")

# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))

# Load plottign functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))



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




setkey(trawl1, s.reg, spp, common, year, stratum)
trawl2 <- trawl1[,list(lat=wtAvg(as.numeric(lat), wtcpue), lon=wtAvg(as.numeric(lon), wtcpue), depth=wtAvg(as.numeric(depth), wtcpue), stemp=wtAvg(stemp, wtcpue), btemp=wtAvg(btemp, wtcpue), wtcpue=mean(wtcpue)), by=key(trawl1)]

good.strat.id <- c()
n.year.strat <- trawl2[,
	{
	nys <- rowSums(table(stratum, year)>1)
	nys.strat <- names(nys)
	nys.n <- as.numeric(nys)
	good.strat.id <<- c(good.strat.id, paste(unique(s.reg), nys.strat[nys.n==max(nys.n)]))
	},
	by=c("s.reg")
]
trawl2.1 <- trawl2[paste(s.reg,stratum)%in%good.strat.id,] # there were 35,259 strata that weren't there every year




# =========================================
# = Pad to regular ts for spp in a strata =
# =========================================
# Pad so that all unique spp in a stratum have a row each year for that stratum
# Added rows are 0's for wtcpue (0 wt per effort)
# Does not include adding NA's for missing years (i.e., if no sampling occurred that year)
allSpp <- trawl2.1[,CJ(spp=unique(spp), year=unique(year)), by=c("s.reg","stratum")]
setkey(allSpp)


trawl3 <- merge(allSpp, trawl2.1, all=TRUE)
trawl3[is.na(wtcpue), wtcpue:=0]
trawl3[, c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg","stratum")]
trawl3[, c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]


# =======================
# = Free up some memory =
# =======================
rm(list=c("trawl","trawl1"))
# save(trawl3, "~/Documents/School&Work/pinskyPost/trawl/Data/trawl3.RData")



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







