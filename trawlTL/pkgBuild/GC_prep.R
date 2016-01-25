library(trawlData)
library(TeachingDemos)
library(fields)

# ============================
# = Calculate trophic shapes =
# ============================
regs <- c("ebs", "ai", "goa", "wc", "gmex", "sa", "neus", "shelf", "newf")
reg_ts <- list()
for(i in 1:length(regs)){
	reg_ts[[regs[i]]] <- trophic_shape(regs[i], t_res=0.5)
}


# ===================
# = Graphing Params =
# ===================
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.1,0.89,0.15,0.9)
axargs <- list(mgp=c(0.5,0.15,0))



# ================================
# = Trophic Level Trend Richness =
# ================================
# ---- time series of heat map ----
dev.new()
par(mfrow=c(3,3))
for(i in 1:length(reg_ts)){
	par(ps=8, cex=1, mgp=c(0.75, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0))
	image.plot(reg_ts[[i]]$rich_l$x, reg_ts[[i]]$rich_l$y, reg_ts[[i]]$rich_l$z, xlab="", ylab="", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0))
	lines(reg_ts[[i]]$rich_l$x, reg_ts[[i]]$rich_l$mid, col="white")
}


# ---- covariance matrix, pair plots ----
dev.new()
all_z0 <- list()
for(i in 1:length(reg_ts)){
	all_z0[[i]] <- as.data.table(reg_ts[[i]]$rich_l$z)
}
all_z <- rbindlist(all_z0, fill=TRUE)

pairs(all_z)
cor(as.matrix(all_z), use="na.or.complete")


# ===============================
# = Trophic Level Trend Biomass =
# ===============================
dev.new()
par(mfrow=c(3,3))
for(i in 1:length(reg_ts)){
	par(ps=8, cex=1, mgp=c(0.75, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0))
	image.plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$y, reg_ts[[i]]$mass_l$z, xlab="", ylab="", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0))
	lines(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mid, col="white")
}

# ---- correlation in biomass among TL's ----
dev.new()
all_z_mass0 <- list()
for(i in 1:length(reg_ts)){
	all_z_mass0[[i]] <- data.table(reg_ts[[i]]$mass_l$z)
}
all_z_mass <- rbindlist(all_z_mass0, fill=TRUE)
pairs(all_z_mass)


# ---- comparison of species contributing to richness vs mass ----
contrib_0 <- list()
for(i in 1:length(reg_ts)){
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=nrow(reg_ts[[i]]$mass_l$z))
	c_m <- reg_ts[[i]]$mass_l$lcbd
	c_r <- reg_ts[[i]]$rich_l$lcbd
	contrib_0[[i]] <- data.table(reg=regs[i], tg=c(c_m[,tg], c_r[,tg]), metric=c(rep('mass',nrow(c_m)),rep('rich',nrow(c_r))), lcbd=c(c_m[,lcbd], c_r[,lcbd]), lcbd_spp=c(c_m[,lcbd_spp], c_r[,lcbd_spp]))
}

contrib <- rbindlist(contrib_0)


# ---- time series of species needed to get 90% of mass variability (temporally) ----
contrib_ts <- list()
for(i in 1:length(regs)){
	contrib_spp_mass <- contrib[metric=="mass" & reg==regs[i], lcbd_spp]
	contrib_lcbd_mass <- contrib[metric=="mass" & reg==regs[i], lcbd]
	t_X <- copy(reg_ts[[i]]$X)
	setkey(t_X, year, spp)
	t_contrib_ts <- t_X[spp%in%contrib_spp_mass]
	
	skele <- t_X[,CJ(year=unique(year), spp=unique(t_contrib_ts[,spp]))]
	setkey(skele, year, spp)
	
	out <- merge(skele, t_X[,list(year, spp, wtcpue, cntcpue, btemp, depth, m, nObs, r, datetime)], by=c("year","spp"), all.x=TRUE, all.y=FALSE)
		
	setkey(out, spp)
	
	merge_other <- t_contrib_ts[,list(spp, reg, common, trophicLevel, trophicLevel.se, tg)]
	setkey(merge_other, spp)
	merge_other <- unique(merge_other)
	
	out <- merge(out, merge_other, by="spp", all=TRUE)
	
	setcolorder(out, c("reg", "year", "datetime", "spp", "common", "tg", "trophicLevel","trophicLevel.se", "btemp","depth", "nObs", "m", "r", "wtcpue", "cntcpue"))
	
	out[is.na(wtcpue), wtcpue:=0]
	
	contrib_ts[[i]] <- out
	
}

contrib_ts <- rbindlist(contrib_ts)
setkey(contrib_ts, reg, year, spp)

tgs <- c("2.5", "3", "3.5", "4", "4.5")
for(i in 1:length(regs)){
	t_dat <- contrib_ts[reg==regs[i]]
	u_spp <- t_dat[,unique(spp)]
	n <- length(u_spp)
	max_n <- t_dat[,lu(spp), by='tg'][,max(V1)]
	dev.new()
	par(mfcol=c(max_n, length(tgs)), cex=1, mar=c(1,1,1.25,0.1), mgp=c(0.5,0.1,0), tcl=-0.1, ps=6, oma=c(0.1,0.1,0.75,0.1))
	for(k in 1:length(tgs)){
		spp_k <- t_dat[tg==tgs[k], unique(spp)]
		if(!length(spp_k)){
			replicate(max_n, plot(1,1, type='n', xaxt="n", yaxt="n", xlab="", ylab=""))
			next
		}
		n_tg_k <- length(spp_k)
		n_tg_extra <- max_n - n_tg_k
		for(s in 1:n_tg_k){
			asdf_common <- match.tbl(spp_k[s], spp.key[,spp], spp.key[,common], exact=TRUE)[,val]
			aa <- tryCatch(sppImg(spp_k[s], common=asdf_common, line=0), error=function(cond)NULL)
			if(!is.null(aa) | (i==2 & k==4 & s==2)){
				par(new=TRUE)
			}
			t_dat[spp%in%spp_k[s], plot(year, wtcpue, type="o", pch=ifelse(wtcpue==0, 21, 20), lwd=3, col="white")]
			t_dat[spp%in%spp_k[s], points(year, wtcpue, type="o", pch=ifelse(wtcpue==0, 21, 20))]
			if(is.null(aa)){
				mtext(paste(spp_k[s], asdf_common, sep="\n"), side=3, line=0)
			}
		}
		replicate(n_tg_extra, plot(1,1, type='n', xaxt="n", yaxt="n", xlab="", ylab=""))
		
	}
	mtext(regs[i], side=3, line=0, outer=TRUE, font=2, cex=1.2)
	
}


# ===========================================================================
# = What drives the big changes in diversity that are seen in most regions? =
# ===========================================================================
# ---- hypothesis 1: temperature ----
# Minimum, mean, or maximum temperature determine the scope of diversity
dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp, type="o", main=regs[i], xlab="", ylab=""); 
	par(new=TRUE); 
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$rich_l$mid, type="o", col='red', xaxt="n", yaxt="n", xlab="", ylab="")
	axis(side=4, col="red")
}
mtext("year", side=1, line=0, outer=TRUE)
mtext("btemp", side=2, line=0, outer=TRUE)
mtext("richness mid TL", side=4, line=0, outer=TRUE)


# ---- mean tl and temperature ----
dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp, type="o", main=regs[i], xlab="", ylab=""); 
	par(new=TRUE); 
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mid, type="o", col='red', xaxt="n", yaxt="n", xlab="", ylab="")
	axis(side=4, col="red")
}
mtext("year", side=1, line=0, outer=TRUE)
mtext("btemp", side=2, line=0, outer=TRUE)
mtext("mass mid TL", side=4, line=0, outer=TRUE)


dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){
	
	ccf(reg_ts[[i]]$btemp, reg_ts[[i]]$mass_l$mid, na.action=na.pass)

}
mtext("year", side=1, line=0, outer=TRUE)
mtext("btemp", side=2, line=0, outer=TRUE)
mtext("mass mid TL", side=4, line=0, outer=TRUE)


# ---- hypothesis 2: changes in the biomass of key species ----
# start by restricting to species observied every year
# response variable is either diversity in a specific tg, mean diversity anomaly, or sum diversity anomaly
for(i in 1:length(regs)){
	
	reg_ts[[i]]$X[,year:=as.integer(year)]
	
	pers_spp <- reg_ts[[i]]$X[,table(spp)]
	pers_spp <- names(pers_spp)[pers_spp==max(pers_spp)]
	
	imp_spp <- reg_ts[[i]]$mass_l$lcbd[lcbd_spp%in%pers_spp,.SD[which.max(lcbd), list(spp=lcbd_spp)], by="tg"]
	imp_dat <- reg_ts[[i]]$X[spp%in%imp_spp[,spp]]	
	
	skele <- imp_dat[,CJ(year=unique(year), spp=unique(spp))]
	setkey(skele, year, spp)
	
	out <- merge(skele, imp_dat[,list(year, spp, wtcpue, cntcpue, btemp, depth, m, nObs, r, datetime)], by=c("year","spp"), all.x=TRUE, all.y=FALSE)
		
	setkey(out, spp)
	
	merge_other <- imp_dat[,list(spp, reg, common, trophicLevel, trophicLevel.se, tg)]
	setkey(merge_other, spp)
	merge_other <- unique(merge_other)
	
	out <- merge(out, merge_other, by="spp", all=TRUE)
	out[is.na(wtcpue), wtcpue:=0]
	out[,wtcpue:=sqrt(wtcpue)]
	
	imp <- dcast.data.table(out, year~spp, value.var="wtcpue")
	rich.ts <- rowSums(reg_ts[[i]]$rich_l$z)
	
	mat <- cbind(rich.ts, imp)
	
	loop_spp <- names(mat)[-c(1:2)]
	dev.new()
	par(mfrow=rbLib::auto.mfrow(ncol(mat)-1), mar=c(2,2,0.75,0.1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1)
	plot(mat[,year], mat[,rich.ts], type="o", xlab="year", ylab="cross-trophic sum richness anomaly")
	imp_cor <- c()
	for(s in 1:length(loop_spp)){
		x <- mat[,rich.ts] - fitted(lm(mat[,rich.ts] ~ mat[,year]))
		y <- mat[,eval(s2c(loop_spp[s]))[[1]]] - fitted(lm(mat[,eval(s2c(loop_spp[s]))[[1]]] ~ mat[,year]))
		
		
		plot(x, y, ylab=paste('detrended', loop_spp[s]), xlab="detrended total richness anomaly")
		
		imp_cor[s] <- cor(x, y)
		mtext(round(imp_cor[s],2), outer=F, line=-0.1, side=3, adj=0.1, font=2)
	}
	names(imp_cor) <- loop_spp
	
}


# ===========================
# = Start GC Poster Figures =
# ===========================
pretty_reg <- c("ebs"="E. Bering Sea", "ai"="Aleutian Islands", "goa"="Gulf of Alaska", "wc"="West Coast US", "gmex"="Gulf of Mexico", "sa"="Southeast US", "neus"="Northeast US", "shelf"="Scotian Shelf", "newf"="Newfoundland")

pretty_col <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','navy','#a65628','salmon','#999999')
names(pretty_col) <- names(pretty_reg)


# ---- time series of total richness for each region ----
year_lim <- range(unlist(sapply(reg_ts, function(x)x$rich_l$x)))
sum_rich <- lapply(reg_ts, function(x)rowSums(x$rich_l$z))
mid_rich <- lapply(reg_ts, function(x)x$rich_l$mid)
mtl_rich <- lapply(reg_ts, function(x)x$rich_l$mtl)
true_rich <- lapply(reg_ts, function(x)rowSums(x$rich_l$vv))
true_rich <- lapply(true_rich, function(x)x/max(x))


y_adj <- c()
y_adj[1] <- -min(mid_rich[[1]])
mid_rich_adj <- list()
mid_rich_adj[[1]] <- mid_rich[[1]]+y_adj[1]

png(file="~/Documents/School&Work/Presentations/GC2016/Figures/richness_anomaly_series.png", width=5.5, height=10, units="in", res=500)
par(mar=c(2.5,2.75,0.1,0.1), mgp=c(1.5,0.25,0), tcl=-0.25, cex=1, ps=18)

# blank plot for setup
plot(reg_ts[[1]]$rich_l$x, mid_rich_adj[[1]], xlim=year_lim, type="n", ylim=c(0,9), ylab="Richness-Weighted Trophic Level", xlab="Year", yaxt="n", col=pretty_col[1], lwd=3)

for(i in 1:length(regs)){
	if(i != 1){
		y_adj[i] <- -(min(mid_rich[[i]]) - (max(mid_rich_adj[[i-1]]) + 0.0))
		mid_rich_adj[[i]] <- mid_rich[[i]] + y_adj[i]
	}
	
	segments(y0=mid_rich_adj[[i]][1], y1=mid_rich_adj[[i]][1], x0=reg_ts[[i]]$rich_l$x[1], x1=rev(reg_ts[[i]]$rich_l$x)[1], lty="dashed", col="black", lwd=1)
	
	lines(reg_ts[[i]]$rich_l$x, mid_rich_adj[[i]], col=pretty_col[i], lwd=3)
	
	if(min(reg_ts[[i]]$rich_l$x) > 1980){
		text(1966, mid_rich_adj[[i]][1], pretty_reg[regs[i]], cex=0.65, col=pretty_col[i], pos=4, font=2)
	}else{
		y_use <- min(mid_rich_adj[[i]][which(reg_ts[[i]]$rich_l$x <= 1980)]) - 0.1
		text(1966, y_use, pretty_reg[regs[i]], cex=0.65, col=pretty_col[i], pos=4, font=2)
	}
}
dev.off()


# ---- time series of MTL or mass_l$mid for each region ----
year_lim <- range(unlist(sapply(reg_ts, function(x)x$mass_l$x)))
sum_mass <- lapply(reg_ts, function(x)rowSums(x$mass_l$z))
mid_mass <- lapply(reg_ts, function(x)x$mass_l$mid)
mtl_mass <- lapply(reg_ts, function(x)x$mass_l$mtl)


y_adj <- c()
y_adj[1] <- -min(mid_mass[[1]])
mid_mass_adj <- list()
mid_mass_adj[[1]] <- mid_mass[[1]]+y_adj[1]

png(file="~/Documents/School&Work/Presentations/GC2016/Figures/biomass_anomaly_series.png", width=5.5, height=10, units="in", res=500)
par(mar=c(2.5,2.75,0.1,0.1), mgp=c(1.5,0.25,0), tcl=-0.25, cex=1, ps=18)

# blank plot for setup
plot(reg_ts[[1]]$mass_l$x, mid_mass_adj[[1]], xlim=year_lim, type="n", ylim=c(0,11.2), ylab="Biomass-Weighted Trophic Level", xlab="Year", yaxt="n", col=pretty_col[1], lwd=3)

for(i in 1:length(regs)){
	if(i != 1){
		y_adj[i] <- -(min(mid_mass[[i]]) - (max(mid_mass_adj[[i-1]]) + 0.0))
		mid_mass_adj[[i]] <- mid_mass[[i]] + y_adj[i]
	}
	
	segments(y0=mid_mass_adj[[i]][1], y1=mid_mass_adj[[i]][1], x0=reg_ts[[i]]$mass_l$x[1], x1=rev(reg_ts[[i]]$mass_l$x)[1], lty="dashed", col="black", lwd=1)
	
	lines(reg_ts[[i]]$mass_l$x, mid_mass_adj[[i]], col=pretty_col[i], lwd=3)
	
	if(min(reg_ts[[i]]$mass_l$x) > 1980){
		text(1966, mid_mass_adj[[i]][1], pretty_reg[regs[i]], cex=0.65, col=pretty_col[i], pos=4, , font=2)
	}else{
		y_use <- min(mid_mass_adj[[i]][which(reg_ts[[i]]$mass_l$x <= 1980)]) - 0.1
		text(1966, y_use, pretty_reg[regs[i]], cex=0.65, col=pretty_col[i], pos=4, font=2)
	}
}
dev.off()


# ---- correlation between richness and mass anomaly ----
anomaly_mr_0 <- list()

for(i in 1:length(reg_ts)){
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=nrow(reg_ts[[i]]$mass_l$z))
	t_yr <- rep(rownames(reg_ts[[i]]$mass_l$z), each=ncol(reg_ts[[i]]$mass_l$z))
	anomaly_mr_0[[i]] <- data.table(reg=regs[i], tg=t_tg, yr=t_yr, anomaly_m=c(reg_ts[[i]]$mass_l$z), anomaly_r=c(reg_ts[[i]]$rich_l$z))
}
anomaly_mr <- rbindlist(anomaly_mr_0, fill=TRUE)


png(file="~/Documents/School&Work/Presentations/GC2016/Figures/biomass_richness_anomaly_scatter.png", width=5, height=5, units="in", res=500)
par(mar=c(2.8,2.9,0.1,0.1), mgp=c(1.75,0.5,0), tcl=-0.25, ps=18, cex=1)
scatter_reg <- anomaly_mr[,lm(anomaly_m~anomaly_r)]
summary(scatter_reg)
anomaly_mr[,plot(anomaly_r, anomaly_m, type='n', ylab="Biomass Anomaly", xlab="Richness Anomaly")]
anomaly_mr[,abline(scatter_reg, lty="dashed", lwd=2)]
anomaly_mr[,points(anomaly_r, anomaly_m, col=adjustcolor(pretty_col[reg], 0.35), pch=20)]
dev.off()


# ===================================================
# = Venn Diagram Richness and Biomass Contributions =
# ===================================================

# ---- Contributions ----
contrib_0 <- list()
for(i in 1:length(reg_ts)){
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=nrow(reg_ts[[i]]$mass_l$z))
	c_m <- reg_ts[[i]]$mass_l$lcbd
	c_r <- reg_ts[[i]]$rich_l$lcbd
	contrib_0[[i]] <- data.table(reg=regs[i], tg=c(c_m[,tg], c_r[,tg]), metric=c(rep('mass',nrow(c_m)),rep('rich',nrow(c_r))), lcbd=c(c_m[,lcbd], c_r[,lcbd]), lcbd_spp=c(c_m[,lcbd_spp], c_r[,lcbd_spp]))
}

contrib <- rbindlist(contrib_0)


# ---- Venn Diagram ----
contrib_venn <- list(contrib[metric=="mass",paste(reg, lcbd_spp, sep="_")], contrib[metric=="rich",paste(reg, lcbd_spp, sep="_")])
names(contrib_venn) <- c("Biomass", "Richness")

venn.diagram(
	contrib_venn, 
	filename="~/Documents/School&Work/Presentations/GC2016/Figures/contrib_venn.tiff", 
	height=5, width=5, resolution=500, units="in",
	cex=1.5, cat.cex=1.5, main.cex=1.5,
	main.fontface=1,
	main.fontfamily="sans", fontfamily="sans", cat.fontfamily="sans",

	cat.pos=c(200, 180),
	cat.dist=c(0.025,0.015),
	cat.default.pos="outer",
	
	main="Species Comprising 90% of Variability", 
	eulder.d=TRUE, scale=TRUE, 
	total.population=lu(unlist(contrib_venn)), hyper.contrib_venn=FALSE, 
	
	fill=c("blue","red")
)

contrib2 <- contrib[,list(mass_only="mass"%in%metric & !"rich"%in%metric, rich_only="rich"%in%metric & !"mass"%in%metric, both="mass"%in%metric & "rich"%in%metric, neither=!"mass"%in%metric & !"rich"%in%metric, lcbd_mass=.SD[metric=="mass", lcbd], lcbd_rich=.SD[metric=="rich", lcbd]), by=c('reg','tg','lcbd_spp')]

# what's in the venn diagram treats species in different regions as separate
# important to make sure that SppX in reg1 as a mass only and as a rich only in reg2 does't show as 'both'
# but it inflates the counts of unique species
# below makes the correct distinction between regions, but doesn't inflate overall counts
# might be useful to know, and is certainly an important distinction for the venn diagram caption
contrib2[(mass_only), lu(lcbd_spp)] # number of unique species mass only
contrib2[(rich_only), lu(lcbd_spp)] # rich only 
contrib2[(both), lu(lcbd_spp)] # both

# ---- Density plots of % contribution per species ----
# dev.new()
png(file="~/Documents/School&Work/Presentations/GC2016/Figures/contrib_venn.png", width=5, height=3, res=500, units="in")
par(mfrow=c(1,3), mar=c(1.5,1.5,0.5,0.1), mgp=c(1.15,0.15,0), tcl=-0.15, ps=18, cex=1, oma=c(1.4,1.2,0.1,0.1))
contrib2[(rich_only), j={
	dens_r <- density(lcbd_rich, from=0, to=1)
	plot(dens_r$x, dens_r$y, type="l", col="red", xlab="", ylab="", ylim=c(0, max(dens_r$y)), lwd=3)
	polygon(c(dens_r$x, rev(dens_r$x)[1], dens_r$x[1]), c(dens_r$y, 0, 0), border=NA, col=adjustcolor("red",0.25))
}]
contrib2[(both), j={
	dens_r <- density(lcbd_rich, from=0, to=1)
	dens_m <- density(lcbd_mass, from=0, to=1)
	x_lim <- range(c(dens_r$x, dens_m$x))
	y_lim <- c(0, max(c(dens_r$y, dens_m$y)))
	
	plot(dens_r$x, dens_r$y, xlim=x_lim, ylim=y_lim, type="l", col="red", xlab="", ylab="", lwd=3)
	polygon(c(dens_r$x, rev(dens_r$x)[1], dens_r$x[1]), c(dens_r$y, 0, 0), border=NA, col=adjustcolor("red",0.25))
	
	lines(dens_m$x, dens_m$y, col="blue", lwd=3)
	polygon(c(dens_m$x, rev(dens_m$x)[1], dens_m$x[1]), c(dens_m$y, 0, 0), border=NA, col=adjustcolor("blue",0.25))
	
}]
legend("top", legend=c("Richness","Biomass"), text.col=c("red","blue"), bty='n', xjust=0.5)

contrib2[(mass_only), j={
	dens_m <- density(lcbd_mass, from=0, to=1)
	plot(dens_m$x, dens_m$y, type="l", col="blue", xlab="", ylab="", ylim=c(0, max(dens_m$y)), lwd=3)
	polygon(c(dens_m$x, rev(dens_m$x)[1], dens_m$x[1]), c(dens_m$y, 0, 0), border=NA, col=adjustcolor("blue",0.25))
}]
mtext("Contribution of a Species to Variability (%)", side=1, line=0.2, outer=TRUE)
mtext("Density", side=2, line=0, outer=TRUE)
dev.off()


# ---- Pad region-specific time series with 0's ----
for(i in 1:length(regs)){
	t_X <- copy(reg_ts[[i]]$X)
	setkey(t_X, year, spp)
	
	skele <- t_X[,CJ(year=unique(year), spp=unique(spp))]
	setkey(skele, year, spp)
	
	out <- merge(skele, t_X[,list(year, spp, wtcpue, cntcpue, btemp, depth, m, nObs, r, datetime)], by=c("year","spp"), all.x=TRUE, all.y=FALSE)
	setkey(out, spp)
	
	merge_other <- t_X[,list(spp, reg, common, trophicLevel, trophicLevel.se, tg)]
	setkey(merge_other, spp)
	merge_other <- unique(merge_other)
	
	out <- merge(out, merge_other, by="spp", all=TRUE)
	setcolorder(out, c("reg", "year", "datetime", "spp", "common", "tg", "trophicLevel","trophicLevel.se", "btemp","depth", "nObs", "m", "r", "wtcpue", "cntcpue"))
	out[is.na(wtcpue), wtcpue:=0]
	setkey(out, reg, year, spp)
	
	reg_ts[[i]]$X0 <- out
}



# ---- Determine which species colonized in the latter 2/3 of time series ----
for(i in 1:length(regs)){
	t_dat <- reg_ts[[i]]$X0
	u_yr <- t_dat[,unique(year)]
	n_yr <- length(u_yr)
	yr_cutoff <- (diff(range(u_yr))/3 + min(u_yr))
	latter_yrs <- u_yr[u_yr >= yr_cutoff]
	early_yrs <- u_yr[u_yr < yr_cutoff]
	
	absent_early_spp <- t_dat[year%in%early_yrs , all(wtcpue==0), by="spp"][(V1),spp]
	
	contrib2[reg==regs[i] & lcbd_spp%in%absent_early_spp, colonizer:=TRUE]
	contrib2[reg==regs[i] & !lcbd_spp%in%absent_early_spp, colonizer:=FALSE]
	
}


contrib_colonizer_venn <- list(
	Biomass = contrib2[(mass_only)|(both),paste(reg, lcbd_spp, sep="_")], 
	Richness = contrib2[(rich_only)|(both),paste(reg, lcbd_spp, sep="_")], 
	Colonizer = contrib2[(colonizer),paste(reg, lcbd_spp, sep="_")]
)

venn.diagram(
	contrib_colonizer_venn, 
	filename="~/Documents/School&Work/pinskyPost/trawl/trawlTL/contrib_colonizer_venn.tiff", 
	height=5, width=5, resolution=300, units="in",
	main.fontfamily="sans",

	cat.pos=c(0, 0, 0),
	
	main="Important Contributors to Richness and Biomass, Late Colonizers", 
	eulder.d=TRUE, scale=TRUE, 
	
	fill=c("blue","red","green")
) 

contrib2[(colonizer) & (both)]
contrib2[(colonizer) & (both) & !tg%in%c("2.5","3")]


# ===================
# = Test for trends =
# ===================
# ---- Mean trophic level (biomass) ----
mass_mid_trend <- list()
for(i in 1:length(regs)){
	t_kendall <- kendallTrendTest(reg_ts[[i]]$mass_l$mid)[c('p.value','estimate')]
	mass_mid_trend[[i]] <- data.table(reg=regs[i], var="mass_mid", tau=t_kendall[[2]][1], slope=t_kendall[[2]][2], p=t_kendall[[1]])
}
mass_mid_trend <- rbindlist(mass_mid_trend) # significant, positive


# ---- Mean trophic level (richness) ----
rich_mid_trend <- list()
for(i in 1:length(regs)){
	t_kendall <- kendallTrendTest(reg_ts[[i]]$rich_l$mid)[c('p.value','estimate')]
	rich_mid_trend[[i]] <- data.table(reg=regs[i], var="rich_mid", tau=t_kendall[[2]][1], slope=t_kendall[[2]][2], p=t_kendall[[1]])
}
rich_mid_trend <- rbindlist(rich_mid_trend)  # 5 significant, all positive


# ---- observed total richness ----
rich_total_trend <- list()
for(i in 1:length(regs)){
	t_rich <- reg_ts[[i]]$X[,lu(spp), keyby='year'][,V1]
	t_kendall <- kendallTrendTest(t_rich)[c('p.value','estimate')]
	rich_total_trend[[i]] <- data.table(reg=regs[i], var="rich_total", tau=t_kendall[[2]][1], slope=t_kendall[[2]][2], p=t_kendall[[1]])
}
rich_total_trend <- rbindlist(rich_total_trend) # 5 significant, all positive


# ============
# = haul map =
# ============
reg_ll <- lapply(reg_ts, function(x)x$ll)
lims <- lapply(rbindlist(reg_ll), range)
png("~/Documents/School&Work/Presentations/GC2016/Figures/haul_map.png", width=9, height=3, units="in", res=500)
par(mar=c(0,0,0,0), oma=c(0,0,0,0), cex=1)
plot(1,1, xlim=lims[[1]], ylim=lims[[2]], type='n', xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
map(add=TRUE)
for(i in 1:length(regs)){
	td <- as.matrix(reg_ll[[regs[i]]][,list(x=lon, y=lat)])
	td2 <- rbindlist(lapply(replicate(25, td, simplify=FALSE), as.data.table))
	points(lapply(td2, jitter, amount=0.5), pch=20, cex=0.5, col=adjustcolor(pretty_col[regs[i]], 1))
}
dev.off()