

# ==================
# = Load Libraries =
# ==================
# library(maps)
library(data.table)
library(vegan)
library(reshape2)



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
# Load trawl data
load("./trawl/Data/divData.RData")


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# ==========================
# = Beta temporal turnover =
# ==========================
beta.turn.time.expr <- bquote({
	# print(paste(s.reg, stratum))
	# print(.SD)
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
		
		# Add fix needed due to region-wide species padding (some spp were never observed in this stratum, but were obsd for other strata in the same region)
		# for "wc", if a species has NA's, that's the result of the species only being observed by wcann or by wctri, but not both.
		goodSpp0 <- apply(castExp, 2, function(x)all(!is.na(x))&any(!is.na(x) & x>0))
		goodSpp <- names(goodSpp0)[goodSpp0]
		castExp <- castExp[,colnames(castExp)%in%goodSpp]
		
		d.helli00 <- beta.div(castExp, nperm=0, save.D=TRUE)
		d.helli0 <- d.helli00$D
		d.helli <- c(d.helli0)
		

		dX.yr <- dist(as.numeric(attributes(d.helli0)$Labels), method="euclidean")
		
		good.y1 <- (sqrt(2)-d.helli)>0 # figure out which indices would throw error if took log
		dy1 <- log(sqrt(2)-d.helli[good.y1])
		dX <- c(dX.yr)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		

		# par(mfrow=c(2,1), mar=c(1.75,1.75,0.5,0.5), oma=c(0.1,0.1,1.5,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
		# mod <- lm(dy1~dX)
		# mod.coef <- as.numeric(mod$coef)
		# add.lines <- list(x=dX, y=as.numeric(predict(mod)))
		#
		# unX.mod <- -exp(as.numeric(predict(mod))) + sqrt(2)
		# add.lines2 <- list(x=dX[order(dX)], y=unX.mod[order(dX)])
		#
		# betaDvar <- d.helli00[[1]][2]
		# meanDist <- mean(d.helli[good.y1])
		#
		# plot(dX, d.helli[good.y1], main=paste(s.reg, stratum), ylim=c(0, sqrt(2)), xlab="", ylab=bquote(Hellinger~Distance~(Delta*y)))
		# lines(add.lines2, col="red")
		#
		# plot(dX, dy1, ylim=c(log(0.1), log(sqrt(2))), xlab=bquote(Delta*x~(years)), ylab=bquote(log[e](sqrt(2)~-~Delta*y)))
		# lines(add.lines, col="red")
		#
		# mtext(paste("intercept =", round(mod.coef[1],2), "  ", "slope =", round(mod.coef[2],2)), outer=TRUE, adj=0, line=0.25)
		# mtext(paste("meanDist =", round(meanDist,2), "  ", "betaDvar =", round(betaDvar,2)), outer=TRUE, adj=0, line=0.75)
		#
		
		
		
		
		-decay.slope
		
		}else{
			as.numeric(NA)
		}

})

# save.image("~/Documents/School&Work/pinskyPost/trawl/Results/forPlotGutsTurnover.RData")

# pdf("~/Desktop/test.pdf", width=3.5, height=6)
# beta.turn.time <- divData[,list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr)), by=c("s.reg","stratum")]
beta.turn.time0 <- divData[,
	j={
		list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr))
	}, 
	
	by=c("s.reg","stratum")
]
# dev.off()
beta.turn.time <- beta.turn.time0[!is.na(turn.time)&turn.time>0,]
beta.turn.time[,turn.time:=log(turn.time)]
# beta.turn.time[,turn.time:=turn.time]

setkey(beta.turn.time, s.reg, stratum)





# ==========================
# = Beta temporal variance =
# ==========================
beta.var.time.expr <- bquote({
	castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
	
	# Add fix needed due to region-wide species padding (some spp were never observed in this stratum, but were obsd for other strata in the same region)
	# for "wc", if a species has NA's, that's the result of the species only being observed by wcann or by wctri, but not both.
	goodSpp0 <- apply(castExp, 2, function(x)all(!is.na(x))&any(!is.na(x) & x>0))
	goodSpp <- names(goodSpp0)[goodSpp0]
	castExp <- castExp[,colnames(castExp)%in%goodSpp]
	
	beta.div(castExp, nperm=0)[[1]][2]
})

beta.var.time <- divData[,
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
		d.helli00 <- beta.div(castExp, nperm=0, save.D=TRUE)
		d.helli0 <- d.helli00$D
		d.helli <- c(d.helli0)

		mu.ll <- .SD[,list(lon.mu=mean(lon), lat.mu=mean(lat)), by="stratum"]
		dX.ll <- mu.ll[,dist(matrix(c(lon.mu,lat.mu),ncol=2), method="euclidean")]
		
		good.y1 <- (sqrt(2)-d.helli)>0 # figure out which indices would throw error if took log
		dy1 <- log(sqrt(2)-d.helli[good.y1])
		dX <- c(dX.ll)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		-decay.slope
		
		
		}else{
			as.numeric(NA)
		}
})

beta.turn.space <- divData[,list(lon=mean(lon), lat=mean(lat), turn.space=eval(beta.turn.space.expr)), by=c("s.reg","year")]
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
beta.var.space <- divData[,
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
# setkey(divData, s.reg, spp, year)
# divData[spp=="Gadus morhua", {plot(aggregate(wtcpue, list(year=year), mean), ylab=s.reg, type="l"); abline(v=1986)}, by="s.reg"]


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
# beta.var.time.lcbd <- divData[,
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

# beta.var.time.lcbd.max <- beta.var.time.lcbd[, list(year=year[which.max(year.lcbd)]), by=c("s.reg","stratum")]
# dev.new(width=3, height=6)
# par(mfrow=c(5,2), mar=c(1.5,1.25,0.5,0.5), oma=c(0.5,0.5,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
# beta.var.time.lcbd.max[,plot(table(year)), by="s.reg"]



# ===========================================================
# = Which fish contributed the most to changes in beta div? =
# ===========================================================
# beta.var.time.scbd.expr <- bquote({
# 	castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
# 	beta.div(castExp, nperm=0)$SCBD
# })
#
# beta.var.time.scbd <- divData[,
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


save(beta.var.space, beta.var.time, beta.turn.space, beta.turn.time, file="./trawl/Results/trawl.betaD.RData")







