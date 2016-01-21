library(trawlData)
library(TeachingDemos)
library(fields)

# ============================
# = Calculate trophic shapes =
# ============================
# regs <- c("ebs", "ai", "goa", "wctri", "wcann", "gmex", "sa", "neus", "shelf", "newf")
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
	# all_z_mass0[[i]] <- data.table(reg=regs[i], yr=reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$z)
	all_z_mass0[[i]] <- data.table(reg_ts[[i]]$mass_l$z)
}
all_z_mass <- rbindlist(all_z_mass0, fill=TRUE)
pairs(all_z_mass)
# pairs(as.list(all_z_mass[reg==regs[i]])[c("3","3.5","4")], type="o")

# ---- compared binned and not binned (mid and MTL) ----
mtl_comp_0 <- list()

for(i in 1:length(reg_ts)){
	mtl_comp_0[[i]] <- data.table(reg=regs[i], mtl=reg_ts[[i]]$mass_l$mtl, mid=reg_ts[[i]]$mass_l$mid)
}
mtl_comp <- rbindlist(mtl_comp_0, fill=TRUE)

dev.new()
par(mfrow=c(3,3))
par(ps=8, cex=1, mgp=c(0.5, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0.5))
for(i in 1:length(reg_ts)){
	temp_df <- as.data.frame(mtl_comp[reg==regs[i]])
	plot(temp_df[,"mtl"], temp_df[,"mid"], main=regs[i], xlab="mtl (not binned)", ylab="mtl (binned)")
	
}

# ---- compare not-binned MTL and trophic shape ----
mtl_shape_0 <- list()

for(i in 1:length(reg_ts)){
	mtl_shape_0[[i]] <- data.table(reg=regs[i], mtl=reg_ts[[i]]$mass_l$mtl, mid=reg_ts[[i]]$mass_l$mid, shape=getShape(skew=reg_ts[[i]]$mass_l$sk, bulge=reg_ts[[i]]$mass_l$bul))
}
mtl_shape <- rbindlist(mtl_shape_0, fill=TRUE)

dev.new()
boxplot(mtl_shape[,mid]~mtl_shape[,shape])


# ==========================================
# = Compare Diversity to Trophic Structure =
# ==========================================

# ---- correlation between richness and mass anomaly ----
anomaly_mr_0 <- list()

for(i in 1:length(reg_ts)){
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=nrow(reg_ts[[i]]$mass_l$z))
	t_yr <- rep(rownames(reg_ts[[i]]$mass_l$z), each=ncol(reg_ts[[i]]$mass_l$z))
	anomaly_mr_0[[i]] <- data.table(reg=regs[i], tg=t_tg, yr=t_yr, anomaly_m=c(reg_ts[[i]]$mass_l$z), anomaly_r=c(reg_ts[[i]]$rich_l$z))
}
anomaly_mr <- rbindlist(anomaly_mr_0, fill=TRUE)

dev.new()
par(mfrow=c(3,3))
par(ps=8, cex=1, mgp=c(0.5, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0.5))
for(i in 1:length(regs)){
	t_r <- anomaly_mr[reg==regs[i],anomaly_r]
	t_m <- anomaly_mr[reg==regs[i],anomaly_m]

	plot(t_r, t_m, xlab="richness anomaly", ylab="mass anomaly", main=regs[i])
	abline(a=0, b=1)
}


# ---- setup for next figures ----
anomaly_shape_0 <- list()
for(i in 1:length(reg_ts)){
	t_nrow <- nrow(reg_ts[[i]]$mass_l$z)
	t_ncol <- ncol(reg_ts[[i]]$mass_l$z)
	
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=t_nrow)
	t_yr <- rep(rownames(reg_ts[[i]]$mass_l$z), each=t_ncol)
	
	bulge_m <- reg_ts[[i]]$mass_l$bul
	skew_m <- reg_ts[[i]]$mass_l$sk
	bulge_r <- reg_ts[[i]]$rich_l$bul
	skew_r <- reg_ts[[i]]$rich_l$sk
	
	t_shape_m <- rep(getShape(skew=skew_m, bulge=bulge_m), each=t_ncol)
	t_shape_r <- rep(getShape(skew=skew_r, bulge=bulge_r), each=t_ncol)
	
	anomaly_shape_0[[i]] <- data.table(
		reg=regs[i], tg=t_tg, yr=t_yr, 
		bulge_m=rep(bulge_m[,"slope"], each=t_ncol), skew_m=rep(skew_m[,"slope"], each=t_ncol), 
		bulge_r=rep(bulge_r[,"slope"], each=t_ncol), skew_r=rep(skew_r[,"slope"], each=t_ncol), 
		shape_m=t_shape_m, shape_r=t_shape_r, 
		anomaly_m=c(reg_ts[[i]]$mass_l$z), anomaly_r=c(reg_ts[[i]]$rich_l$z)
	)
}
anomaly_shape <- rbindlist(anomaly_shape_0, fill=TRUE)


# ---- richness anomaly grouped by (mass) shape ----
dev.new()
boxplot((anomaly_shape[,anomaly_r]) ~ anomaly_shape[,shape_m])
# boxplot(abs(anomaly_shape[,anomaly_r]) ~ anomaly_shape[,shape_m])

dev.new()
boxplot((anomaly_shape[,anomaly_m]) ~ anomaly_shape[,shape_r])

# ---- pair plots of skew and bulge for richness and mass ----
dev.new()
pairs(anomaly_shape[!duplicated(paste(reg,yr)),list(bulge_m, skew_m, bulge_r, skew_r)])
#
# dev.new()
# plot(anomaly_shape[shape_m%in%c("upward_triangle","downward_triangle")][!duplicated(paste(reg,yr)), list(skew_r, skew_m)])

# b_i <- c("upward_triangle","downward_triangle")
# plot(anomaly_shape[shape_m%in%b_i][!duplicated(paste(reg,yr)), list(skew_r, skew_m)])
# plot(anomaly_shape[shape_r%in%b_i][!duplicated(paste(reg,yr)), list(skew_r, skew_m)])
# plot(anomaly_shape[shape_m%in%b_i & shape_r%in%b_i][!duplicated(paste(reg,yr)), list(skew_r, skew_m)])
#
# plot(anomaly_shape[!shape_m%in%b_i][!duplicated(paste(reg,yr)), list(bulge_r, bulge_m)])
# plot(anomaly_shape[!shape_r%in%b_i][!duplicated(paste(reg,yr)), list(bulge_r, bulge_m)])
# plot(anomaly_shape[!shape_m%in%b_i & shape_r%in%b_i][!duplicated(paste(reg,yr)), list(bulge_r, bulge_m)])


# ---- heat map of richness shape and mass shape ----
dev.new()
par(mfrow=c(1,2))
table_shape <- anomaly_shape[,table(shape_m, shape_r)]
# table_shape <- log(anomaly_shape[,table(shape_m, shape_r)] + 0.1)
image.plot(1:5, 1:5, table_shape, axes=F, xlab="Richness Shape", ylab="Mass Shape")
text(1:5, 0.1, gsub("_", "\n", rownames(table_shape)), xpd=TRUE, srt=0)
text(0.35, 1:5, gsub("_", "\n", rownames(table_shape)), xpd=TRUE, srt=90)
t_points(1:5, 1:5, rownames(table_shape), inches=0.3, col="white")

image.plot(1:4, 1:4, table_shape[-4,-4], axes=F, xlab="Richness Shape", ylab="Mass Shape")
text(1:4, 0.1, gsub("_", "\n", rownames(table_shape[-4,-4])), xpd=TRUE, srt=0)
text(0.35, 1:4, gsub("_", "\n", rownames(table_shape[-4,-4])), xpd=TRUE, srt=90)
t_points(1:4, 1:4, rownames(table_shape[-4,-4]), inches=0.3, col="white", lwd=3)
t_points(1:4, 1:4, rownames(table_shape[-4,-4]), inches=0.3, col="black")



# ---- comparison of species contributing to richness vs mass ----
contrib_0 <- list()
for(i in 1:length(reg_ts)){
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=nrow(reg_ts[[i]]$mass_l$z))
	c_m <- reg_ts[[i]]$mass_l$lcbd
	c_r <- reg_ts[[i]]$rich_l$lcbd
	contrib_0[[i]] <- data.table(reg=regs[i], tg=c(c_m[,tg], c_r[,tg]), metric=c(rep('mass',nrow(c_m)),rep('rich',nrow(c_r))), lcbd=c(c_m[,lcbd], c_r[,lcbd]), lcbd_spp=c(c_m[,lcbd_spp], c_r[,lcbd_spp]))
}

contrib <- rbindlist(contrib_0)

nspp_contrib <- contrib[,list(n_spp=lu(lcbd_spp)),by=c('reg', 'tg', 'metric')]

dev.new()
nspp_contrib[,boxplot(n_spp~metric)]


contrib_both <- contrib[, 
	list(
		lcbd_spp=.SD[metric=="mass", lcbd_spp][.SD[metric=="mass", lcbd_spp]%in%.SD[metric=="rich", lcbd_spp]], 
		lcbd_mass= .SD[metric=="mass", lcbd][.SD[metric=="mass", lcbd_spp]%in%.SD[metric=="rich", lcbd_spp]],
		lcbd_rich= .SD[metric=="rich", lcbd][.SD[metric=="rich", lcbd_spp]%in%.SD[metric=="mass", lcbd_spp]]
	), 
	by=c("reg","tg")
]



dev.new()
par(mfrow=c(6,6), cex=1, mar=c(0.1,0.1,0.2,0.1), ps=6)
asdf <- contrib_both[!tg%in%c("2.5", "3", "4.5"),lcbd_spp]
asdf_common <- spp.key[spp%in%asdf | !is.na(spp),list(spp, common)]
setkey(asdf_common, spp, common)
asdf_common <- match.tbl(asdf, spp.key[,spp], spp.key[,common], exact=TRUE)[,val]

for(i in 1:length(asdf)){
	if(i %in% c(7)){
		next
	}
	aa <- sppImg(asdf[i], common=asdf_common[i], line=-0.8)
	if(!is.null(aa)){
		t_reg <- contrib_both[!tg%in%c("2.5", "3", "4.5"),][i,reg]
		par(new=TRUE)
		reg_ts[[t_reg]]$X[spp==asdf[i], plot(year, wtcpue, col="white", lwd=2, type="l", ylab="", xlab="")]
		reg_ts[[t_reg]]$X[spp==asdf[i], lines(year, wtcpue, col="black", lwd=1)]
		reg_ts[[t_reg]]$X[spp==asdf[i] & wtcpue==0, points(year, wtcpue, col="black", lwd=1)]
	}

}





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
	
	# for(k in 1:n){
# 		asdf_common <- match.tbl(u_spp[k], spp.key[,spp], spp.key[,common], exact=TRUE)[,val]
# 		aa <- tryCatch(sppImg(u_spp[k], common=asdf_common, line=-1.5), error=function(cond)NULL)
# 		if(!is.null(aa)){
# 			par(new=TRUE)
# 		}
# 		t_dat[spp%in%u_spp[k], plot(year, wtcpue, type="o", pch=ifelse(is.na(wtcpue), 21, 20))]
# 	}
}


contrib[, .SD[which.max(lcbd)], by=c("reg","tg","metric")]



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


dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp_09, type="o", main=regs[i], xlab="", ylab=""); 
	par(new=TRUE); 
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$rich_l$mid, type="o", col='red', xaxt="n", yaxt="n", xlab="", ylab="")
	axis(side=4, col="red")
}
mtext("year", side=1, line=0, outer=TRUE)
mtext("btemp 0.9 quantile", side=2, line=0, outer=TRUE)
mtext("richness mid TL", side=4, line=0, outer=TRUE)


dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp_01, type="o", main=regs[i], xlab="", ylab=""); 
	par(new=TRUE); 
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$rich_l$mid, type="o", col='red', xaxt="n", yaxt="n", xlab="", ylab="")
	axis(side=4, col="red")
}
mtext("year", side=1, line=0, outer=TRUE)
mtext("btemp 0.1 quantile", side=2, line=0, outer=TRUE)
mtext("richness mid TL", side=4, line=0, outer=TRUE)


dev.new(); 
par(mfrow=c(3,3))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp, type="o"); 
	par(new=TRUE); 
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mid, type="o", col='red')
}



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

	# plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp, type="o", main=regs[i], xlab="", ylab="");
	# par(new=TRUE);
	# plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mid, type="o", col='red', xaxt="n", yaxt="n", xlab="", ylab="")
	# axis(side=4, col="red")
}
mtext("year", side=1, line=0, outer=TRUE)
mtext("btemp", side=2, line=0, outer=TRUE)
mtext("mass mid TL", side=4, line=0, outer=TRUE)


dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp, type="o", main=regs[i], xlab="", ylab="");
	par(new=TRUE);
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mtl, type="o", col='red', xaxt="n", yaxt="n", xlab="", ylab="")
	axis(side=4, col="red")
}


dev.new(); 
par(mfrow=c(3,3), mar=c(1,1,0.5,1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1, oma=c(1,1,0.1,1))
for(i in 1:length(regs)){

	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$btemp, type="o", main=regs[i], xlab="", ylab="");
	par(new=TRUE);
	plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mid, type="o", col='black', xaxt="n", yaxt="n", xlab="", ylab="")
	axis(side=4, col="red")
}


# ---- hypothesis 2: changes in the biomass of key species ----
# start by restricting to species observied every year
# response variable is either diversity in a specific tg, mean diversity anomaly, or sum diversity anomaly
# 


for(i in 1:length(regs)){
	
	reg_ts[[i]]$X[,year:=as.integer(year)]
	
	pers_spp <- reg_ts[[i]]$X[,table(spp)]
	pers_spp <- names(pers_spp)[pers_spp==max(pers_spp)]
	
	imp_spp <- reg_ts[[i]]$mass_l$lcbd[lcbd_spp%in%pers_spp,.SD[which.max(lcbd), list(spp=lcbd_spp)], by="tg"]
	imp_dat <- reg_ts[[i]]$X[spp%in%imp_spp[,spp]]
	# imp_dat <- reg_ts[[i]]$X[spp%in%pers_spp]
	
	
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
	
	# par(mfrow=c(ncol(mat)-1, 1), mar=c(2,2,0.1,0.1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1)
	# plot(rich.ts~., data=mat, ask=F, type='p')
	
	# par(mfrow=c(ncol(mat)-1, 1), mar=c(2,2,0.1,0.1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1)
	loop_spp <- names(mat)[-c(1:2)]
	dev.new()
	par(mfrow=rbLib::auto.mfrow(ncol(mat)-1), mar=c(2,2,0.75,0.1), mgp=c(1,0.1,0), tcl=-0.1, ps=8, cex=1)
	plot(mat[,year], mat[,rich.ts], type="o", xlab="year", ylab="cross-trophic sum richness anomaly")
	imp_cor <- c()
	for(s in 1:length(loop_spp)){
		x <- mat[,rich.ts] - fitted(lm(mat[,rich.ts] ~ mat[,year]))
		y <- mat[,eval(s2c(loop_spp[s]))[[1]]] - fitted(lm(mat[,eval(s2c(loop_spp[s]))[[1]]] ~ mat[,year]))
		
		# plot(mat[,rich.ts], mat[,eval(s2c(loop_spp[s]))[[1]]], ylab=loop_spp[s], xlab="total richness anomaly")
		
		plot(x, y, ylab=paste('detrended', loop_spp[s]), xlab="detrended total richness anomaly")
		

		# imp_cor[s] <- cor(mat[,rich.ts], mat[,eval(s2c(loop_spp[s]))[[1]]])
		imp_cor[s] <- cor(x, y)
		mtext(round(imp_cor[s],2), outer=F, line=-0.1, side=3, adj=0.1, font=2)
	}
	names(imp_cor) <- loop_spp
	
}



# ================
# = Frank EBS Eg =
# ================
# reg_ts[[1]]$X[spp%in%c("Gadus macrocephalus", "Limanda aspera", "Paralithodes camtschaticus")]
#
# reg_ts[[1]]$X[common%in%c("red king crab")]
#
# reg_ts[[1]]$X[spp%in%c("Gadus macrocephalus", "Pleuronectes asper", "Paralithodes camtschatica")]



# ======================================
# = Core Figures after Experimentation =
# ======================================
pretty_reg <- c("ebs"="E. Berring Sea", "ai"="Aleutian Islands", "goa"="Gulf of Alaska", "wc"="West Coast US", "gmex"="Gulf of Mexico", "sa"="Southeast US", "neus"="Northeast US", "shelf"="Scotian Shelf", "newf"="Newfoundland")

# pretty_col <- c('#a6cee3', '#1f78b4', '#b2df8a', '#33a02c', '#fb9a99', '#e31a1c', '#fdbf6f', '#ff7f00', '#cab2d6', '#6a3d9a')
# pretty_col <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf','#999999')
# pretty_col <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','navy','#a65628','#f781bf','#999999')
pretty_col <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','navy','#a65628','salmon','#999999')
names(pretty_col) <- names(pretty_reg)


# ---- time series of total richness for each region ----
year_lim <- range(unlist(sapply(reg_ts, function(x)x$rich_l$x)))
sum_rich <- lapply(reg_ts, function(x)rowSums(x$rich_l$z))
mid_rich <- lapply(reg_ts, function(x)x$rich_l$mid)
mtl_rich <- lapply(reg_ts, function(x)x$rich_l$mtl)
true_rich <- lapply(reg_ts, function(x)rowSums(x$rich_l$vv))
true_rich <- lapply(true_rich, function(x)x/max(x))
# pairs(data.frame(unlist(sum_rich), unlist(mid_rich), unlist(mtl_rich)))

y_adj <- c()
y_adj[1] <- -min(mid_rich[[1]])
mid_rich_adj <- list()
mid_rich_adj[[1]] <- mid_rich[[1]]+y_adj[1]
dev.new(width=4, height=7)
par(mar=c(2,2,0.1,0.1), mgp=c(1,0.1,0), tcl=-0.1, cex=1, ps=14)
plot(reg_ts[[1]]$rich_l$x, mid_rich_adj[[1]], xlim=year_lim, type="l", ylim=c(0,10), ylab="Richness-Weighted Trophic Level", xlab="Year", yaxt="n", col=pretty_col[1])
text(1965, mid_rich_adj[[1]][1]-0.25, pretty_reg[regs[1]], cex=0.65, col=pretty_col[1], pos=4)
for(i in 2:length(regs)){
	y_adj[i] <- -(min(mid_rich[[i]]) - (max(mid_rich_adj[[i-1]]) + 0.0))
	mid_rich_adj[[i]] <- mid_rich[[i]] + y_adj[i]
	lines(reg_ts[[i]]$rich_l$x, mid_rich_adj[[i]], col=pretty_col[i])
	text(1965, mid_rich_adj[[i]][1]-0.3, pretty_reg[regs[i]], cex=0.65, col=pretty_col[i], pos=4)
}


# ---- time series of MTL or mass_l$mid for each region ----
year_lim <- range(unlist(sapply(reg_ts, function(x)x$mass_l$x)))
sum_mass <- lapply(reg_ts, function(x)rowSums(x$mass_l$z))
mid_mass <- lapply(reg_ts, function(x)x$mass_l$mid)
mtl_mass <- lapply(reg_ts, function(x)x$mass_l$mtl)
# pairs(data.frame(unlist(sum_mass), unlist(mid_mass), unlist(mtl_mass)))

y_adj <- c()
y_adj[1] <- -min(mid_mass[[1]])
mid_mass_adj <- list()
mid_mass_adj[[1]] <- mid_mass[[1]]+y_adj[1]
dev.new(width=4, height=7)
par(mar=c(2,2,0.1,0.1), mgp=c(1,0.1,0), tcl=-0.1, cex=1, ps=14)
plot(reg_ts[[1]]$mass_l$x, mid_mass_adj[[1]], xlim=year_lim, type="l", ylim=c(0,11.2), ylab="Biomass-Weighted Trophic Level", xlab="Year", yaxt="n", col=pretty_col[1])
text(1965, mid_mass_adj[[1]][1]-0.25, pretty_reg[regs[1]], cex=0.65, col=pretty_col[1], pos=4)
for(i in 2:length(regs)){
	y_adj[i] <- -(min(mid_mass[[i]]) - (max(mid_mass_adj[[i-1]]) + 0.0))
	mid_mass_adj[[i]] <- mid_mass[[i]] + y_adj[i]
	lines(reg_ts[[i]]$mass_l$x, mid_mass_adj[[i]], col=pretty_col[i])
	text(1965, mid_mass_adj[[i]][1]-0.5, pretty_reg[regs[i]], cex=0.65, col=pretty_col[i], pos=4)
}



# ---- proportion of years with trophic shapes ----
# anomaly_shape_0 <- list()
# for(i in 1:length(reg_ts)){
# 	t_nrow <- nrow(reg_ts[[i]]$mass_l$z)
# 	t_ncol <- ncol(reg_ts[[i]]$mass_l$z)
#
# 	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=t_nrow)
# 	t_yr <- rep(rownames(reg_ts[[i]]$mass_l$z), each=t_ncol)
#
# 	bulge_m <- reg_ts[[i]]$mass_l$bul
# 	skew_m <- reg_ts[[i]]$mass_l$sk
# 	bulge_r <- reg_ts[[i]]$rich_l$bul
# 	skew_r <- reg_ts[[i]]$rich_l$sk
#
# 	t_shape_m <- rep(getShape(skew=skew_m, bulge=bulge_m), each=t_ncol)
# 	t_shape_r <- rep(getShape(skew=skew_r, bulge=bulge_r), each=t_ncol)
#
# 	anomaly_shape_0[[i]] <- data.table(
# 		reg=regs[i], tg=t_tg, yr=t_yr,
# 		bulge_m=rep(bulge_m[,"slope"], each=t_ncol), skew_m=rep(skew_m[,"slope"], each=t_ncol),
# 		bulge_r=rep(bulge_r[,"slope"], each=t_ncol), skew_r=rep(skew_r[,"slope"], each=t_ncol),
# 		shape_m=t_shape_m, shape_r=t_shape_r,
# 		anomaly_m=c(reg_ts[[i]]$mass_l$z), anomaly_r=c(reg_ts[[i]]$rich_l$z)
# 	)
# }
# anomaly_shape <- rbindlist(anomaly_shape_0, fill=TRUE)
#
# shape_tally <- anomaly_shape[,apply(table(yr, shape_m, reg), c(2,3), sum)]
# shape_tally/colSums(shape_tally)
#
# shape_tally_prop <- shape_tally
# shape_tally_prop[] <- NA
# for(i in 1:ncol(shape_tally)){
# 	shape_tally_prop[,i] <- shape_tally[,i]/sum(shape_tally[,i])
# }
# boxplot(t(shape_tally_prop[c("downward_triangle","upward_triangle","diamond","hourglass"),]))

# ---- pair plot of coherence among TL for richness ----


# ---- pair plot of coherence among TL for mass ----


# ---- correlation between richness and mass anomaly ----
anomaly_mr_0 <- list()

for(i in 1:length(reg_ts)){
	t_tg <- rep(colnames(reg_ts[[i]]$mass_l$z), each=nrow(reg_ts[[i]]$mass_l$z))
	t_yr <- rep(rownames(reg_ts[[i]]$mass_l$z), each=ncol(reg_ts[[i]]$mass_l$z))
	anomaly_mr_0[[i]] <- data.table(reg=regs[i], tg=t_tg, yr=t_yr, anomaly_m=c(reg_ts[[i]]$mass_l$z), anomaly_r=c(reg_ts[[i]]$rich_l$z))
}
anomaly_mr <- rbindlist(anomaly_mr_0, fill=TRUE)

# dev.new()
# par(mfrow=c(3,3))
# par(ps=8, cex=1, mgp=c(0.5, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0.5))
# for(i in 1:length(regs)){
# 	t_r <- anomaly_mr[reg==regs[i],anomaly_r]
# 	t_m <- anomaly_mr[reg==regs[i],anomaly_m]
#
# 	plot(t_r, t_m, xlab="richness anomaly", ylab="mass anomaly", main=regs[i])
# 	abline(a=0, b=1)
# }

dev.new(width=4, height=4)
par(mar=c(2,2,0.1,0.1), mgp=c(1,0.1,0), tcl=-0.1, ps=14, cex=1)
anomaly_mr[,plot(anomaly_r, anomaly_m, type='n', ylab="Biomass Anomaly", xlab="Richness Anomaly")]
anomaly_mr[,abline(lm(anomaly_m~anomaly_r), lty="dashed", lwd=2)]
anomaly_mr[,points(anomaly_r, anomaly_m, col=adjustcolor(pretty_col[reg], 0.35), pch=20)]



#
# cor_mr <- anomaly_mr[,j={
#
# 	combos <- CJ(tg_m=unique(tg), tg_r=unique(tg))
#
# 	cor_mr <- c()
# 	for(k in 1:nrow(combos)){
# 		t_x <- .SD[tg==combos[k,tg_m], anomaly_m]
# 		t_y <- .SD[tg==combos[k,tg_r], anomaly_r]
# 		cor_mr[k] <- cor(t_x, t_y)
# 	}
# 	combos[,cor_mr:=cor_mr]
#
# 	combos
#
#
# }, by=c("reg")]
#
# cor_mr[!is.finite(cor_mr), cor_mr:=NA]
# cor_mr[,c("tg_m","tg_r"):=list(as.numeric(tg_m), as.numeric(tg_r))]
# cor_mr[,image.plot(as.matrix(reshape2::dcast(.SD, tg_m~tg_r, value.var="cor_mr")[,-1])), by="reg"]
#
#


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

# nspp_contrib <- contrib[,list(n_spp=lu(lcbd_spp)),by=c('reg', 'tg', 'metric')]

# dev.new()
# nspp_contrib[,boxplot(n_spp~metric)]


# contrib_both <- contrib[,
# 	list(
# 		lcbd_spp=.SD[metric=="mass", lcbd_spp][.SD[metric=="mass", lcbd_spp]%in%.SD[metric=="rich", lcbd_spp]],
# 		lcbd_mass= .SD[metric=="mass", lcbd][.SD[metric=="mass", lcbd_spp]%in%.SD[metric=="rich", lcbd_spp]],
# 		lcbd_rich= .SD[metric=="rich", lcbd][.SD[metric=="rich", lcbd_spp]%in%.SD[metric=="mass", lcbd_spp]]
# 	),
# 	by=c("reg","tg")
# ]

# ---- Venn Diagram ----

contrib_venn <- list(contrib[metric=="mass",paste(reg, lcbd_spp, sep="_")], contrib[metric=="rich",paste(reg, lcbd_spp, sep="_")])
names(contrib_venn) <- c("Biomass", "Richness")

venn.diagram(
	contrib_venn, 
	filename="~/Documents/School&Work/pinskyPost/trawl/trawlTL/contrib_venn.tiff", 
	height=5, width=5, resolution=300, units="in",
	main.fontfamily="sans",

	cat.pos=c(210, 180),
	cat.dist=c(0.02,0.01),
	cat.default.pos="outer",
	
	# main="Number of Species Required to Explain 90% Variability", 
	main="Species Needed to Account for 90% of Temporal Variability", 
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
dev.new()
par(mfrow=c(1,3), mar=c(1.5,1.5,0.5,0.1), mgp=c(1,0.1,0), tcl=-0.1, ps=14, cex=1, oma=c(1,1,0.1,0.1))
contrib2[(rich_only), j={
	dens_r <- density(lcbd_rich, from=0, to=1)
	# dens_m <- density(lcbd_mass, from=0, to=1)
	# x_lim <- range(c(dens_r$x, dens_m$x))
	# y_lim <- range(c(dens_r$y, dens_m$y))
	plot(dens_r$x, dens_r$y, type="l", col="red", xlab="", ylab="", ylim=c(0, max(dens_r$y)))
	polygon(c(dens_r$x, rev(dens_r$x)[1], dens_r$x[1]), c(dens_r$y, 0, 0), border=NA, col=adjustcolor("red",0.25))
	# lines(dens_m$x, dens_m$y, col="blue")
}]
contrib2[(both), j={
	dens_r <- density(lcbd_rich, from=0, to=1)
	dens_m <- density(lcbd_mass, from=0, to=1)
	x_lim <- range(c(dens_r$x, dens_m$x))
	y_lim <- c(0, max(c(dens_r$y, dens_m$y)))
	
	plot(dens_r$x, dens_r$y, xlim=x_lim, ylim=y_lim, type="l", col="red", xlab="", ylab="")
	polygon(c(dens_r$x, rev(dens_r$x)[1], dens_r$x[1]), c(dens_r$y, 0, 0), border=NA, col=adjustcolor("red",0.25))
	
	lines(dens_m$x, dens_m$y, col="blue")
	polygon(c(dens_m$x, rev(dens_m$x)[1], dens_m$x[1]), c(dens_m$y, 0, 0), border=NA, col=adjustcolor("blue",0.25))
	
}]
legend("top", legend=c("Richness","Biomass"), text.col=c("red","blue"), bty='n', xjust=0.5)

contrib2[(mass_only), j={
	# dens_r <- density(lcbd_rich, from=0, to=1)
	dens_m <- density(lcbd_mass, from=0, to=1)
	# x_lim <- range(c(dens_r$x, dens_m$x))
	# y_lim <- range(c(dens_r$y, dens_m$y))
	plot(dens_m$x, dens_m$y, type="l", col="blue", xlab="", ylab="", ylim=c(0, max(dens_m$y)))
	polygon(c(dens_m$x, rev(dens_m$x)[1], dens_m$x[1]), c(dens_m$y, 0, 0), border=NA, col=adjustcolor("blue",0.25))
	# lines(dens_m$x, dens_m$y, col="blue")
}]
mtext("Contribution of a Species to Variability (%)", side=1, line=0, outer=TRUE)
mtext("Density", side=2, line=0, outer=TRUE)



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







