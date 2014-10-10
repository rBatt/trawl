
library(data.table)
library(plyr)

load("~/Documents/School&Work/pinskyPost/trawl/Results/forPlotGutsTurnover.RData")


# ==========================
# = Beta temporal turnover =
# ==========================
beta.turn.time.expr <- bquote({
	# print(paste(s.reg, stratum))
	# print(.SD)
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]
		d.helli00 <- beta.div(castExp, nperm=0, save.D=TRUE)
		d.helli0 <- d.helli00$D
		d.helli <- c(d.helli0)

		
		dX.yr <- dist(as.numeric(attributes(d.helli0)$Labels), method="euclidean")
		
		good.y1 <- d.helli>0 # figure out which indices would throw error if took log
		dy1 <- log(sqrt(2)-d.helli[good.y1])
		dX <- c(dX.yr)[good.y1]
		decay.slope <- lm(dy1~dX)$coef[2]
		
		
		par(mfrow=c(2,1), mar=c(1.75,1.75,0.5,0.5), oma=c(0.1,0.1,1.5,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
		mod <- lm(dy1~dX)
		mod.coef <- as.numeric(mod$coef)
		add.lines <- list(x=dX, y=as.numeric(predict(mod)))
		
		unX.mod <- -exp(as.numeric(predict(mod))) + sqrt(2)
		add.lines2 <- list(x=dX[order(dX)], y=unX.mod[order(dX)])
		
		betaDvar <- d.helli00[[1]][2]
		meanDist <- mean(d.helli[good.y1])
		
		plot(dX, d.helli[good.y1], main=paste(s.reg, stratum), ylim=c(0, sqrt(2)), xlab="", ylab=bquote(Hellinger~Distance~(Delta*y)))
		lines(add.lines2, col="red")
		
		plot(dX, dy1, ylim=c(log(0.1), log(sqrt(2))), xlab=bquote(Delta*x~(years)), ylab=bquote(log[e](sqrt(2)~-~Delta*y)))
		lines(add.lines, col="red")
		
		mtext(paste("intercept =", round(mod.coef[1],2), "  ", "slope =", round(mod.coef[2],2)), outer=TRUE, adj=0, line=0.25)
		mtext(paste("meanDist =", round(meanDist,2), "  ", "betaDvar =", round(betaDvar,2)), outer=TRUE, adj=0, line=0.75)
		
		
		
		
		
		decay.slope
		}else{
			as.numeric(NA)
		}

})


pdf("~/Desktop/test.pdf", width=3.5, height=6)
# beta.turn.time <- divData[,list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr)), by=c("s.reg","stratum")]
beta.turn.time <- divData[,
	j={
		list(lon=mean(lon), lat=mean(lat), turn.time=eval(beta.turn.time.expr))
	}, 
	
	by=c("s.reg","stratum")
]
dev.off()






# test <- divData[s.reg=="neus"&stratum=="1100"]
# blah <- test[,acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp, fill=0)[,-1]]
# test.bd1 <- beta.div(blah, nperm=0)[[1]][2]
# d.helli0 <- beta.div(blah, nperm=0, save.D=TRUE)$D
# d.helli <- c(d.helli0)

# ==============================================
# = Heuristic figure for how/ why to transform =
# ==============================================
dev.new(width=3.5, height=3.5)
par(mfrow=c(2,2), mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.65,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
tvec.x <- seq(0, 30, length.out=50)
# (1-dy) = exp(x)
# -dy = exp(x) - 1
# dy = -exp(x) + 1
# so to simulate dy in the case where the maximum is sqrt(2) (not 1):
tvec <- -exp(0 + tvec.x*-0.05) + sqrt(2)
tvec.noise <- -exp(0 + tvec.x*-0.05 + rnorm(length(tvec), sd=0.1)) + sqrt(2)

plot(tvec.x, sqrt(2)-tvec.noise, ylab=bquote(sqrt(2)-Delta*y), xlab=bquote(Delta*X))
lines(tvec.x, sqrt(2)-tvec, type="l", lwd=3)

plot(tvec.x, tvec.noise, ylab=bquote(Delta*y), xlab=bquote(Delta*X))
lines(tvec.x, tvec, type="l", lwd=3)

plot((tvec.x), log(sqrt(2)-tvec.noise), ylab=bquote(log[e](sqrt(2)-Delta*y)), xlab=bquote(Delta*X))
lines((tvec.x), log(sqrt(2)-tvec), type="l", lwd=3)

plot((tvec.x), log(tvec.noise), ylab=bquote(log[e](Delta*y)), xlab=bquote(Delta*X))
lines((tvec.x), log(tvec), type="l", lwd=3)
points(mean(range(tvec.x)), mean(range(log(tvec.noise))), pch=4, cex=10, col="red")