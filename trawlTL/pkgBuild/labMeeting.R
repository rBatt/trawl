

# ============================
# = Calculate trophic shapes =
# ============================
regs <- c("ebs", "ai", "goa", "wctri", "wcann", "gmex", "sa", "neus", "shelf", "newf")
reg_ts <- list()
for(i in 1:length(regs)){
	reg_ts[[regs[i]]] <- trophic_shape(regs[i], t_res=0.5)
}


# =======================
# = Plot trophic shapes =
# =======================

# ---- Just time series of mean with shapes ----
# weighted by mass
png("trophic_shape_lines.png", width=11, height=4, res=250, units="in")
par(mfrow=rbLib::auto.mfrow(length(regs)), mar=c(1.5, 1.25, 0.25, 0.25), mgp=c(1,0.1,0), tcl=-0.1, cex=1, ps=8, oma=c(1,1,0.25,0.1))
for(i in 1:length(regs)){
	mass_l <- reg_ts[[regs[i]]]$mass_l
	plot(x=mass_l$x, y=mass_l$mid, lwd=2, type="l", xlab="", ylab="")
	
	sh <- getShape(skew=mass_l$sk, bulge=mass_l$bul)
	inch <- c(0.18, 0.1)[as.integer(sh=="rectangle")+1]
	t_points(mass_l$x, mass_l$mid, shape=sh, inches=inch, col="black", lwd=3)
	t_points(mass_l$x, mass_l$mid, shape=sh, inches=inch, col="white", lwd=0.5)
	
	mtext(regs[i], side=3, adj=0, line=-0.1, font=2)
}
dev.off()

# richness (not standardized)
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.1,0.89,0.15,0.9)
axargs <- list(mgp=c(0.5,0.15,0))

png("trophic_richness_absolute.png", width=11, height=4, res=250, units="in")
par(mfrow=rbLib::auto.mfrow(length(regs)), oma=c(1,1,0.25,0.1))
for(i in 1:length(regs)){
	rich_l <- reg_ts[[regs[i]]]$rich_l
	
	par(mar=c(2, 2, 0.5, 0.5), mgp=c(1,0.1,0), tcl=-0.1, cex=1, ps=8)
	image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$vv, xlab="", ylab="", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0), tcl=-0.15)

	mtext(regs[i], side=3, adj=0, line=-0.1, font=2)
}
dev.off()


# richness (standardized)
smplt <- c(0.9,0.92, 0.2,0.8)
bgplt <- c(0.1,0.89,0.15,0.9)
axargs <- list(mgp=c(0.5,0.15,0))

png("trophic_richness_scaled.png", width=11, height=4, res=250, units="in")
par(mfrow=rbLib::auto.mfrow(length(regs)), oma=c(1,1,0.25,0.1))
for(i in 1:length(regs)){
	rich_l <- reg_ts[[regs[i]]]$rich_l
	
	par(mar=c(2, 2, 0.5, 0.5), mgp=c(1,0.1,0), tcl=-0.1, cex=1, ps=8)
	image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$z, xlab="", ylab="", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0), tcl=-0.15)

	mtext(regs[i], side=3, adj=0, line=-0.1, font=2)
}
dev.off()


#
# par(mfrow=rbLib::auto.mfrow(length(regs)))
# smplt <- c(0.9,0.92, 0.2,0.8)
# bgplt <- c(0.05,0.89,0.15,0.95)
# axargs <- list(mgp=c(0.5,0.15,0))
# for(i in 1:length(regs)){
# 	reg_ts <- trophic_shape(regs[i], t_res=0.5)
#
# 	mass_l <- reg_ts$mass_l
# 	rich_l <- reg_ts$rich_l
#
# 	library(fields)
# 	# dev.new()
# 	image.plot(x=mass_l$x, y=mass_l$y, z=mass_l$z, xlab="year", ylab="trophic level", main=regs[i], smallplot=smplt, bigplot=bgplt, axis.args=axargs, mgp=c(0.75,0.25,0), tcl=-0.15)
# 	lines(x=mass_l$x, y=mass_l$mid, col="white", lwd=2)
#
# 	sh <- getShape(skew=mass_l$sk, bulge=mass_l$bul)
# 	inch <- c(0.2, 0.1)[as.integer(sh=="rectangle")+1]
# 	t_points(mass_l$x, mass_l$mid, shape=sh, inches=inch, col="black", lwd=3)
# 	t_points(mass_l$x, mass_l$mid, shape=sh, inches=inch, col="white", lwd=0.5)
# }
#
#
#
# image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$z, xlab="year", ylab="trophic level")
# lines(x=rich_l$x, y=rich_l$mid, col="white", lwd=2)



reg_ts[[i]]$mass_l