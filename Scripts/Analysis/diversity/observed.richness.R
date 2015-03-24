
# =================
# = Load Packages =
# =================
library(data.table)
library(fields)
library(beanplot)
library(RColorBrewer)

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
trawl2.veri <- trawl2[(correctSpp)&!is.na(common)&is.species(spp),]
trawl2.veri <- trawl2.veri[s.reg!="wcann"]
trawl2.veri[s.reg=="wcann" | s.reg=="wctri", s.reg:="wc"]



# =======================
# = Region Names (Long) =
# =======================
regKey <- c("ai"="Aleutian Islands", "ebs"="Eastern Bering Sea", "gmex"="Gulf of Mexico", "goa"="Gulf of Alaska", "neus"="Northeast US", "newf"="Newfoundland", "sa"="US South Atlantic", "sgulf"="S. Gulf of St. Lawrence", "shelf"="Scotian Shelf", "wc"="West Coast")


# =====================
# = Observed Richness =
# =====================

rich.obs <- trawl2.veri[,list(obs.rich.all=lu(spp), obs.rich.chor=lu(spp[!is.na(phylum)&phylum=="Chordata"])), by=c("s.reg","stratum","year")]

rich.obs.LLs <- t(simplify2array(strsplit(rich.obs[,stratum], " ")))
rich.obs.lon <- as.numeric(rich.obs.LLs[,1])
rich.obs.lat <- as.numeric(rich.obs.LLs[,2])
rich.obs[,c("lon","lat"):=list(rich.obs.lon, rich.obs.lat)]

setkey(rich.obs, s.reg, year, stratum)


rich.obs.mu <- rich.obs[,list(stratum=unique(stratum), lon=unique(lon), lat=unique(lat), obs.rich.all=mean(obs.rich.all, na.rm=TRUE), obs.rich.chor=mean(obs.rich.chor, na.rm=TRUE)), by=c("s.reg","stratum")]

# ==============================
# = Observed Richness Map: All =
# ==============================

png(height=4, width=rich.obs.mu[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/avg_richness_observed_all.png", res=200, units="in")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
rich.obs.mu[,z.col.all:=heat.cols[cut(obs.rich.all, 256)]]
rich.obs.mu[,plot(lon, lat, col=z.col.all, pch=21, cex=1, type="n")]
invisible(rich.obs.mu[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
rich.obs.mu[,points(lon, lat, bg=z.col.all, pch=21, cex=1)] # add points


# Key
rich.obs.mu[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
rich.obs.mu[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
rich.obs.mu[,text(-167, y=seq(30,40, length.out=4), round(seq(min(obs.rich.all, na.rm=TRUE), max(obs.rich.all, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
rich.obs.mu[,text(-162.5, 41.5, bquote(Mean~Observed~Richness~(number~spp)))] # add label for key


dev.off()




# ==============================
# = Observed Richness Map: Chordata =
# ==============================

png(height=4, width=rich.obs.mu[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/avg_richness_observed_chordata.png", res=200, units="in")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
rich.obs.mu[,z.col.chor:=heat.cols[cut(obs.rich.chor, 256)]]
rich.obs.mu[,plot(lon, lat, col=z.col.chor, pch=21, cex=1, type="n")]
invisible(rich.obs.mu[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
rich.obs.mu[,points(lon, lat, bg=z.col.chor, pch=21, cex=1)] # add points


# Key
rich.obs.mu[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
rich.obs.mu[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
rich.obs.mu[,text(-167, y=seq(30,40, length.out=4), round(seq(min(obs.rich.chor, na.rm=TRUE), max(obs.rich.chor, na.rm=TRUE), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
rich.obs.mu[,text(-162.5, 41.5, bquote(Mean~Observed~Chordate~Richness~(number~spp)))] # add label for key


dev.off()


# ======================================
# = Observed Richness Time Series: All =
# ======================================

auto.dim <- rich.obs[,auto.mfrow(lu(s.reg), tall=F)]
# dev.new(width=4, height=(auto.dim[1]/auto.dim[2])*4)
# dev.new(width=(auto.dim[2]/auto.dim[1])*4, height=4)
png(width=(auto.dim[2]/auto.dim[1])*4, height=4, file="./trawl/Figures/Diversity/strat_rich_time_observed_all.png", units="in", res=200)
par(mfrow=auto.dim, mar=c(1.5,1,1.0,0.1), oma=c(0.5,1,1.5,0.1), cex=1, tcl=-0.1, mgp=c(1,0.1,0), ps=8)

MyGray2 <- rgb(t(col2rgb("black")), alpha=100, maxColorValue=255)

for(i in 1:length(rich.obs[,unique(s.reg)])){
	
	t.sreg <- rich.obs[,unique(s.reg)[i]]
	t.r <- rich.obs[s.reg==t.sreg]
	
	maxR <- t.r[,max(obs.rich.all)]
	minR <- t.r[,min(obs.rich.all)]
	maxY <- t.r[,max(year)]
	minY <- t.r[,min(year)]
	
	u.strat <- t.r[,unique(stratum)]
	t.r[
		stratum==u.strat[1],
		plot(year, obs.rich.all, ylim=c(minR, maxR), xlim=c(minY,maxY), main=regKey[as.character(unique(s.reg))], type="l", col=MyGray2, xlab="", ylab="")
	]
	for(j in 2:length(u.strat)){
		t.r[stratum==u.strat[j],lines(year,obs.rich.all, col=MyGray2)]
	}
	t.r[,mean(obs.rich.all), by="year"][,lines(year,V1, col="red",lwd=2)]
	
}
mtext("# Species", side=2, line=0, outer=TRUE, cex=1.25)
# abline(v=2004, lty="dotted", lwd=2) # only need if still using wcann, which I no longer am
mtext(bquote(underline(Observed~~Richness)), side=3, line=0.25, outer=TRUE, cex=1.5)

dev.off()



# ============================================
# = Observed Richness Time Series: Chordates =
# ============================================
auto.dim <- rich.obs[,auto.mfrow(lu(s.reg), tall=F)]
# dev.new(width=4, height=(auto.dim[1]/auto.dim[2])*4)
# dev.new(width=(auto.dim[2]/auto.dim[1])*4, height=4)
png(width=(auto.dim[2]/auto.dim[1])*4, height=4, file="./trawl/Figures/Diversity/strat_rich_time_observed_chordata.png", units="in", res=200)
par(mfrow=auto.dim, mar=c(1.5,1,1.0,0.1), oma=c(0.5,1,1.5,0.1), cex=1, tcl=-0.1, mgp=c(1,0.1,0), ps=8)

MyGray2 <- rgb(t(col2rgb("black")), alpha=100, maxColorValue=255)

for(i in 1:length(rich.obs[,unique(s.reg)])){
	
	t.sreg <- rich.obs[,unique(s.reg)[i]]
	t.r <- rich.obs[s.reg==t.sreg]
	
	maxR <- t.r[,max(obs.rich.chor)]
	minR <- t.r[,min(obs.rich.chor)]
	maxY <- t.r[,max(year)]
	minY <- t.r[,min(year)]
	
	u.strat <- t.r[,unique(stratum)]
	t.r[
		stratum==u.strat[1],
		plot(year, obs.rich.chor, ylim=c(minR, maxR), xlim=c(minY,maxY), main=regKey[as.character(unique(s.reg))], type="l", col=MyGray2, xlab="", ylab="")
	]
	for(j in 2:length(u.strat)){
		t.r[stratum==u.strat[j],lines(year,obs.rich.chor, col=MyGray2)]
	}
	t.r[,mean(obs.rich.chor), by="year"][,lines(year,V1, col="red",lwd=2)]
	
}
mtext("# Species", side=2, line=0, outer=TRUE, cex=1.25)
# abline(v=2004, lty="dotted", lwd=2)
mtext(bquote(underline(Observed~~Chordata~~Richness)), side=3, line=0.25, outer=TRUE, cex=1.5)
dev.off()


# =====================================================
# = Observed Chordate Richness vs. Temperature        =
# =====================================================
auto.dim <- rich.obs[,auto.mfrow(lu(s.reg), tall=F)]
rich.ts <- rich.obs[,mean(obs.rich.chor), by=c("year", "s.reg")] # timeseries of mean richness for each region
setnames(rich.ts, old="V1", new="obs.rich.chor")

temps = copy(trawl2.veri)
temps[,c('K', 'spp', 'wtcpue', 'correctSpp', 'common', 'taxLvl', 'phylum'):=NULL] # remove columns we don't need
temps <- unique(temps)
temps.mu = temps[,list(mu.btemp = mean(btemp, na.rm=TRUE), mu.stemp = mean(stemp, na.rm=TRUE)), by=c('year','s.reg', 'region')]  # mean temp by year in each region

rich.ts = merge(rich.ts, temps.mu, by=c('s.reg', 'year'))
cols = colorRampPalette(brewer.pal(9, 'YlGnBu')[2:9])(rich.obs[,lu(year)]) # colors, one for each year
names(cols) = rich.ts[,sort(unique(year))]

# quartz(width=(auto.dim[2]/auto.dim[1])*4, height=4)
png(width=(auto.dim[2]/auto.dim[1])*4, height=4, file="./trawl/Figures/BioClimate/rich_by_year_vs_btemp_observed_chordata.png", units="in", res=200)
par(mfrow=auto.dim, mai=c(0.5,0.5,0.3,0.1), cex=1, tcl=-0.1, mgp=c(1,0.2,0), ps=8, las=1)

rich.ts[,plot(mu.btemp, obs.rich.chor, main=regKey[as.character(unique(s.reg))], xlab='Bottom temperature (°C)', ylab='Richness', pch=16, col=cols[as.character(year)]), by='s.reg']

dev.off()


# =========================================================================
# = Change in Observed Chordate Richness vs. Change in Temperature        =
# =========================================================================
rich.ts <- rich.obs[,mean(obs.rich.chor), by=c("year", "s.reg")] # timeseries of mean richness for each region
setnames(rich.ts, old="V1", new="obs.rich.chor")

temps = copy(trawl2.veri)
temps[,c('K', 'spp', 'wtcpue', 'correctSpp', 'common', 'taxLvl', 'phylum'):=NULL] # remove columns we don't need
temps <- unique(temps)
temps.mu = temps[,list(mu.btemp = mean(btemp, na.rm=TRUE), mu.stemp = mean(stemp, na.rm=TRUE)), by=c('year','s.reg', 'region')]  # mean temp by year in each region

rich.ts = merge(rich.ts, temps.mu, by=c('s.reg', 'year'))
rich.slope = rich.ts[,list(obs.rich.chor.slope = coef(lm(obs.rich.chor~year))[2], btemp.slope = coef(lm(mu.btemp~year))[2]),by=c('s.reg')]

cols = brewer.pal(rich.slope[,lu(s.reg)], 'Set3')

# quartz(width=4, height=4)
png(width=4, height=4, file="./trawl/Figures/BioClimate/richslope_vs_btempslope_observed_chordata.png", units="in", res=200)
par(mai=c(0.6,0.6,0.3,0.1), cex=1, tcl=-0.1, mgp=c(1.8,0.2,0), ps=8, las=1)

rich.slope[,plot(btemp.slope, obs.rich.chor.slope, xlab='Bottom temperature trend (°C/year)', ylab='Richness trend (spp/year)', pch=16, col=cols, cex=2, cex.lab=2, cex.axis=1.5)]

legend('topright', col=cols, legend=regKey[as.character(sort(unique(rich.slope$s.reg)))], pch=16)

dev.off()

