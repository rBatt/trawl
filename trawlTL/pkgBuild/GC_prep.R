# ============================
# = Calculate trophic shapes =
# ============================
regs <- c("ebs", "ai", "goa", "wctri", "wcann", "gmex", "sa", "neus", "shelf", "newf")
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
par(mfrow=c(5,2))
for(i in 1:length(reg_ts)){
	par(ps=8, cex=1, mgp=c(0.75, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0))
	image.plot(reg_ts[[i]]$rich_l$x, reg_ts[[i]]$rich_l$y, reg_ts[[i]]$rich_l$z, xlab="", ylab="", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0))
	lines(reg_ts[[i]]$rich_l$x, reg_ts[[i]]$rich_l$mid, col="white")
}


# ---- covariance matrix, pair plots ----

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
par(mfrow=c(5,2))
for(i in 1:length(reg_ts)){
	par(ps=8, cex=1, mgp=c(0.75, 0.1, 0), tcl=-0.1, mar=c(1.75,1.75,0.5,0))
	image.plot(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$y, reg_ts[[i]]$mass_l$z, xlab="", ylab="", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0))
	lines(reg_ts[[i]]$mass_l$x, reg_ts[[i]]$mass_l$mid, col="white")
}

# ---- compared binned and not binned (mid and MTL) ----
mtl_comp_0 <- list()

for(i in 1:length(reg_ts)){
	mtl_comp_0[[i]] <- data.table(reg=regs[i], mtl=reg_ts[[i]]$mass_l$mtl, mid=reg_ts[[i]]$mass_l$mid)
}
mtl_comp <- rbindlist(mtl_comp_0, fill=TRUE)

par(mfrow=c(5,2))
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

boxplot(mtl_shape[,mid]~mtl_shape[,shape])


# ========
# = comp =
# ========


