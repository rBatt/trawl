
# ==================
# = Load Libraries =
# ==================
# library(maps)
library(data.table)
library(vegan)
library(reshape2)


# =============
# = Load Data =
# =============
# Load trawl data
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/divData.RData")


# ===========================
# = Load Scripts/ Functions =
# ===========================
# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))

# Load plottign functions
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))

# Load statistics functions
stat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

# divData2 <- divData[,list(stemp=meanna(stemp), btemp=meanna(btemp), depth=meanna(depth), wtcpue=meanna(wtcpue)), by=c("s.reg","stratum","spp","year","common")]




# ===================
# = Alpha over time =
# ===================
alpha.trend.expr <- bquote({
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
		# castExp <- test[,acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]]
		
		# Add fix needed due to region-wide species padding (some spp were never observed in this stratum, but were obsd for other strata in the same region)
		# for "wc", if a species has NA's, that's the result of the species only being observed by wcann or by wctri, but not both.
		goodSpp0 <- apply(castExp, 2, function(x)all(!is.na(x))&any(!is.na(x) & x>0))
		goodSpp <- names(goodSpp0)[goodSpp0]
		castExp <- castExp[,colnames(castExp)%in%goodSpp]
		
		
		alphaD <- diversity(castExp)
		
		yrs.0 <- as.numeric(names(alphaD))
		dX.yr <- yrs.0 - min(yrs.0)

		dy1 <- as.numeric(alphaD)
		dX <- c(dX.yr)
			
		decay.slope <- lm(dy1~dX)$coef[2]
		decay.slope
		
	}else{
		as.numeric(NA)
	}
})



# =================
# = Current Alpha =
# =================
alpha.now.expr <- bquote({
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
		
		# Add fix needed due to region-wide species padding (some spp were never observed in this stratum, but were obsd for other strata in the same region)
		# for "wc", if a species has NA's, that's the result of the species only being observed by wcann or by wctri, but not both.
		goodSpp0 <- apply(castExp, 2, function(x)all(!is.na(x))&any(!is.na(x) & x>0))
		goodSpp <- names(goodSpp0)[goodSpp0]
		castExp <- castExp[,colnames(castExp)%in%goodSpp]
		
		alphaD <- diversity(castExp)

		yrs.0 <- as.numeric(names(alphaD))
		dX.yr <- yrs.0 - min(yrs.0)

		dy1 <- as.numeric(alphaD)
		dX <- c(dX.yr)
		
		now.pred <- rev(as.numeric(predict(lm(dy1~dX))))[1]
		if(now.pred<=0){
			rev(dy1)[1]
		}else{
			now.pred
		}
		
	}else{
		as.numeric(NA)
	}
})



# ========================
# = Plot Alpha over time =
# ========================
alpha.plot.expr <- bquote({
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
		
		# Add fix needed due to region-wide species padding (some spp were never observed in this stratum, but were obsd for other strata in the same region)
		# for "wc", if a species has NA's, that's the result of the species only being observed by wcann or by wctri, but not both.
		goodSpp0 <- apply(castExp, 2, function(x)all(!is.na(x))&any(!is.na(x) & x>0))
		goodSpp <- names(goodSpp0)[goodSpp0]
		castExp <- castExp[,colnames(castExp)%in%goodSpp]
		
		alphaD <- diversity(castExp)
		
		yrs.0 <- as.numeric(names(alphaD))
		yrs.1 <- min(yrs.0)
		dX.yr <- yrs.0 - yrs.1

		dy1 <- as.numeric(alphaD)
		dX <- c(dX.yr)
		
			
		alpha.mod <- lm(dy1~yrs.0)
		am.ylim <- range(c(predict(alpha.mod), dy1), na.rm=TRUE)
		
		par(mar=c(2.5, 2.5, 0.1, 0.5), ps=8, cex=1, mgp=c(1.5, 0.25, 0), tcl=-0.25, family="Times")
		plot(yrs.0, dy1, pch=20, ylim=am.ylim)
		abline(alpha.mod, col="red")
		points(tail(yrs.0,1), tail(predict(alpha.mod),1), pch=19, col="blue")
		print(tail(predict(alpha.mod),1))
			
		}else{
			as.numeric(NA)
		}
})

# dev.new()
# divData[s.reg=="ai"&stratum=="513",
# 	j={
# 		eval(alpha.plot.expr)
# 	}
# ]



alphaD <- divData[,
	j={
		list(lon=mean(lon), lat=mean(lat), alpha.trend=eval(alpha.trend.expr), alpha.now=eval(alpha.now.expr))
	}, 
	
	by=c("s.reg","stratum")
]


save(alphaD, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/alphaD.RData")


