# =====================================================
# = Quick regressions for beta div before lab meeting =
# =====================================================


# ==========================
# = Load packages and data =
# ==========================
library(data.table)


load("~/Documents/School&Work/pinskyPost/trawl/Results/trawl.betaD.RData")
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/divData.RData")

# Load Data functions
dat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(dat.location, list.files(dat.location), sep="/"), source, .GlobalEnv))


# ============================================================
# = Quick merge for data organization; combine turn with var =
# ============================================================
beta.temporal <- merge(beta.var.time[,list(s.reg,stratum,lon,lat,var.time)], beta.turn.time[,list(s.reg,stratum,lon,lat,turn.time)], by=c("s.reg","stratum","lon","lat"))

# dev.new(width=3.5, height=3.5)
# par(mfrow=c(1,1), mar=c(1.75,1.75,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
# beta.temporal[,plot((var.time), (turn.time), xlab=bquote(Temporal~Variance), ylab=bquote(log[e](Temporal~Turnover)))]


# ====================================================================
# = temporal turnover and variance vs. latitude (pooled all regions) =
# ====================================================================
dev.new(width=3.5, height=5)
# png(file="~/Desktop/presFigs/tempTurnVar_vs_lat_all.png", width=3.5, height=6, res=150, units="in")
par(mfrow=c(2,1), mar=c(1.75,1.75,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.temporal[,plot((lat), (turn.time), xlab=bquote(latitude), ylab=bquote(log[e](Temporal~Turnover)))]
beta.temporal[,plot((lat), (var.time), xlab=bquote(latitude), ylab=bquote(Temporal~Variance))]
# dev.off()


# =======================================================================
# = Does the temporal turnover of each stratum correlate with latitude? =
# =======================================================================
dev.new(width=4, height=7)
# png(file="~/Desktop/presFigs/tempTurn_vs_lat_byReg.png", width=4, height=7, res=150, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.25,0.5,0.5), oma=c(0.5,0.5,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.temporal[,
	{
		plot((lat), (turn.time), xlab="", ylab="", main=s.reg)
		t.mod.pval <- summary(lm(turn.time~lat))$coef[2,4]
		t.lwd.logic <- t.mod.pval < 0.05
		t.dash.logic <- t.mod.pval < 0.005
		if(t.lwd.logic){
			abline(lm(turn.time~lat), col="blue", lwd=4)
		}else{
			abline(lm(turn.time~lat), col="blue", lwd=0.5)
		}
		if(t.dash.logic){
			abline(lm(turn.time~lat), col="white", lty="dashed", lwd=2)
		}
		
	}, 
	by="s.reg"
]
mtext(bquote(log[e](Temporal~Turnover)), side=2, outer=TRUE, line=-0.5)
mtext(bquote(latitude), side=1, outer=TRUE, line=-0.5)

# dev.off()





# =======================================================================
# = Does the temporal variance of each stratum correlate with latitude? =
# =======================================================================
dev.new(width=4, height=7)
# png(file="~/Desktop/presFigs/tempVar_vs_lat_byReg.png", width=4, height=7, res=150, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.25,0.5,0.5), oma=c(0.5,0.5,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
beta.temporal[,
	{
		plot((lat), (var.time), xlab="", ylab="", main=s.reg)
		t.mod.pval <- summary(lm(var.time~lat))$coef[2,4]
		t.lwd.logic <- t.mod.pval < 0.05
		t.dash.logic <- t.mod.pval < 0.005
		if(t.lwd.logic){
			abline(lm(var.time~lat), col="blue", lwd=4)
		}else{
			abline(lm(var.time~lat), col="blue", lwd=0.5)
		}
		if(t.dash.logic){
			abline(lm(var.time~lat), col="white", lty="dashed", lwd=2)
		}
		
	}, 
	by="s.reg"
]
mtext(bquote(Temporal~Variance), side=2, outer=TRUE, line=-0.5)
mtext(bquote(latitude), side=1, outer=TRUE, line=-0.5)

# dev.off()




# ==================
# = Eigenvalue EWS =
# ==================
# the multivariate index steve uses, I think
spp.mat000 <- divData[,list(wtcpue=meanna(wtcpue)), by=c("s.reg","stratum","spp","common","year")]

spp.lambda <- spp.mat000[,
	{
		casted <- acast(melt(.SD, id.vars=c("stratum","spp"), measure.vars=c("wtcpue")), stratum~spp, fill=0)[,-1]
		casted <- decostand(casted, "hellinger")
		# lambda <- tryCatch(max(abs(eigen(cov(casted))$values)), error=function(cond){as.numeric(NA)})
		lambda <- tryCatch(max(abs(eigen(cov(casted))$values)), error=function(cond){as.numeric(NA)})
		lambda
	},
	
	by=c("s.reg","year")
	
]
setnames(spp.lambda, "V1", "lambda")

# plot the dominant eigenvalue
dev.new(width=4, height=7)
# png(file="~/Desktop/presFigs/maxEigen.png", width=4, height=7, res=150, units="in")
par(mfrow=c(5,2), mar=c(1.5,1.25,0.5,0.5), oma=c(0.5,0.5,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
spp.lambda[,plot(year, lambda, type="l", xlab="", ylab="", main=s.reg), by=c("s.reg")]

mtext(bquote(Year), side=1, outer=TRUE, line=-0.5)
mtext(bquote(max~eigenvalue), side=2, outer=TRUE, line=-0.5)
# dev.off()