#' Calculate the trophic shape of a food web
#' 
#' Yap
#' 
#' @param reg character of trawl region
#' @param t_res trophic level resolution used in binning
#' 
#' @examples
# neus_ts <- trophic_shape("neus", t_res=0.5)
#
# mass_l <- neus_ts$mass_l
# rich_l <- neus_ts$rich_l

# sum1_z <- scale_sum1(mass_l$z)
# h <- sum1_z[1,] + mass_l$x[1]
# barplot(h, horiz=T, space=0, border=NA, xlim=c(mass_l$x[1], max(h)))
# lines(y=1:7, x=sum1_z[1,] , type="s")

# library(fields)
# image.plot(x=mass_l$x, y=mass_l$y, z=mass_l$z, xlab="year", ylab="trophic level")
# lines(x=mass_l$x, y=mass_l$mid, col="white", lwd=2)
#
# sh <- getShape(skew=mass_l$sk, bulge=mass_l$bul)
# inch <- c(0.2, 0.1)[as.integer(sh=="rectangle")+1]
# t_points(mass_l$x, mass_l$mid, shape=sh, inches=inch, col="black", lwd=3)
# t_points(mass_l$x, mass_l$mid, shape=sh, inches=inch, col="white", lwd=0.5)
#
# image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$z, xlab="year", ylab="trophic level")
# lines(x=rich_l$x, y=rich_l$mid, col="white", lwd=2)


trophic_shape <- function(reg, t_res=0.5){

	
	
	if(reg == "wc"){
		X1 <- trawlTrim("wctri", c.add=c("trophicLevel","trophicLevel.se","sex"))#[year!=2004]
		# X2 <- trawlTrim("wcann", c.add=c("trophicLevel","trophicLevel.se","sex"))
		# X <- rbind(X1, X2, fill=TRUE)
		X <- X1
		X <- X[,reg:="wc"][!is.na(trophicLevel) & is.finite(wtcpue)]
	}else{
		X <- trawlTrim(reg, c.add=c("trophicLevel","trophicLevel.se","sex"))
	}
	
	if(reg=="newf"){
		X <- X[year>=1995]
	}
	
	if(reg=="sa"){
		X <- X[!spp%in%"Stomolophus meleagris"] # cannonball jelly not consistently identified for wtcpue
	}
	

	X[,stratum:=ll2strat(lon, lat)]
	strat_table <- X[,colSums(table(year, stratum)>0)]
	goodStrat2 <- X[,names(strat_table)[strat_table>=(lu(year)-(0.2*lu(year)))]]
	X[,keep_strat:=(stratum%in%goodStrat2)]	
	X <- X[(keep_strat)]
	
	X <- X[!is.na(trophicLevel)]
	
	X[,lon:=roundGrid(lon)]
	X[,lat:=roundGrid(lat)]
	
	ll <- X[!duplicated(stratum), list(lon, lat)]
	
	
	
	X_btemp <- X[,list(year, haulid, btemp)]
	setkey(X_btemp, year, haulid)
	X_btemp[,btemp:=fill.mean(btemp), by=c("haulid","year")]
	X_btemp[,btemp:=fill.mean(btemp), by=c("year")]
	
	X_btemp <- unique(X_btemp)
	btemp <- X_btemp[,mean(btemp, na.rm=TRUE), by=c("year")][,V1]
	btemp_09 <- X_btemp[,quantile(btemp, 0.9, na.rm=TRUE), by=c("year")][,V1]
	btemp_01 <- X_btemp[,quantile(btemp, 0.1, na.rm=TRUE), by=c("year")][,V1]
	
	# plot(btemp, ylim=range(c(btemp, btemp_09, btemp_01), na.rm=TRUE), type="o")
	# lines(btemp_09, type="o")
	# lines(btemp_01, type="o")
	
	
	if(reg=="neus"){
		Xa <- trawlAgg(X, bio_lvl="sex", space_lvl="haulid", time_lvl="haulid", bioFun=meanna, envFun=meanna, metaCols=c("reg","datetime","year","common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		Xa[,time_lvl:=NULL]
		
		Xa <- trawlAgg(X, bio_lvl="spp", space_lvl="haulid", time_lvl="haulid", bioFun=sumna, envFun=meanna, metaCols=c("reg","datetime","year","common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		Xa[,time_lvl:=NULL]
		#
		# Xa_strat <- trawlAgg(Xa, bio_lvl="spp", space_lvl="stratum", time_lvl="year", bioFun=meanna, envFun=meanna, metaCols=c("reg","common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		# setnames(Xa, "time_lvl", "year")
		
		Xa <- trawlAgg(Xa, bio_lvl="spp", space_lvl="reg", time_lvl="year", bioFun=meanna, envFun=meanna, metaCols=c("common","trophicLevel","trophicLevel.se", "lon", "lat"), meta.action="unique1")
		
		setnames(Xa, "time_lvl", "year")
		
	}else{
		Xa <- trawlAgg(X, bio_lvl="spp", space_lvl="haulid", time_lvl="haulid", bioFun=sumna, envFun=meanna, metaCols=c("reg","datetime","year","common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		Xa[,time_lvl:=NULL]
		#
		# Xa_strat <- trawlAgg(Xa, bio_lvl="spp", space_lvl="stratum", time_lvl="year", bioFun=meanna, envFun=meanna, metaCols=c("reg","common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		# setnames(Xa, "time_lvl", "year")
		
		Xa <- trawlAgg(X, bio_lvl="spp", space_lvl="reg", time_lvl="year", bioFun=meanna, envFun=meanna, metaCols=c("common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		setnames(Xa, "time_lvl", "year")
	}


	setkey(Xa, reg, year, spp, common)

	round_tl <- Xa[,floor(trophicLevel/t_res)*t_res]
	cut_seq <- seq(min(round_tl), max(round_tl), by=t_res)
	Xa[,tg:=as.character(cut(trophicLevel, breaks=cut_seq, labels=cut_seq[-1], include.lowest=TRUE))]
	Xa[is.na(tg) & trophicLevel>=max(cut_seq), tg:=as.character(max(cut_seq))]
	Xa[is.na(tg) & trophicLevel<=min(cut_seq), tg:=as.character(min(cut_seq))]
	Xa[,m:=wtcpue]
	# Xa[,m:=cntcpue]
	Xa[,nObs:=nAgg]
	Xa[,nAgg:=NULL]
	Xa[,r:=1]
	Xa[,datetime:=as.POSIXct(year, format="%Y")]
	
	
	lcbd <- function(X, prop_out=1){
		ss <- apply(X, 2, var)
		l <- sort(ss/sum(ss), decreasing=TRUE)
		# l <- sort(beta.div(X)$SCBD)
		
		out_ind <- cumsum(l) < prop_out
		out_ind[which(!out_ind)[1]] <- TRUE
		
		list(lcbd=l[out_ind], lcbd_spp=names(l[out_ind]))
	}
	
	mass_X <-  Xa[,reshape2::acast(.SD, year~spp, value.var="wtcpue", fill=0)]
	# test <- lcbd(mass_X)[[1]]
	# test <- test[order(names(test))]
	# test2 <- beta.div(mass_X)$SCBD
	# test2 <- test2[order(names(test2))]
	# test2 <- test2[names(test2)%in%names(test)]
	lcbd_mass <- Xa[,lcbd(reshape2::acast(.SD, year~spp, value.var="wtcpue", fill=0), prop_out=0.9), by=c("tg")]
	
	
	lcbd_rich <- Xa[, j = {
		t_x <- reshape2::acast(.SD, year~spp, value.var="r", fill=0)
		# t_x[] <- pmin(1, ceiling(t_x))
		lcbd(t_x, prop_out=0.9)
	}, 
	, by=c("tg")
	]
	
	
	
	# Xs <- Xa[tg=="4",reshape2::acast(.SD, year~spp, value.var="cntcpue", fill=0)]
	# ss <- apply(Xs, 2, var)
	# lcbd <- ss/sum(ss)
	# sort(lcbd)
	#
	# plot(as.integer(rownames(Xs)), Xs[,1], type='l', ylim=range(Xs))
	# for(i in 2:ncol(Xs)){
	# 	lines(as.integer(rownames(Xs)), Xs[,i], col=i+1)
	# }
	
	mtl_mass <- Xa[, list(mtl=sumna((wtcpue/sumna(wtcpue))*trophicLevel)),by=c("reg","year")][,mtl]
	mtl_rich <- Xa[, list(mtl=sumna((1/sumna(lu(spp)))*trophicLevel)),by=c("reg","year")][,mtl]
	
	
	Xa2 <- trawlAgg(Xa, bio_lvl="tg", time_lvl="year", space_lvl="reg", bioCols=c("nObs","r","m"), envCols=c("stemp","btemp","depth"), bioFun=sum, envFun=mean, metaCols="trophicLevel.se", meta.action="FUN", metaFun=function(x, ...)sum(x^2, ...))
	setnames(Xa2, "time_lvl", "year")
	setkey(Xa2, reg, year, tg)

	cast_tl <- function(xa, value.var){
		vv <- reshape2::acast(xa, year~tg, value.var=value.var, fill=0)
		scale2 <- function(x){
			x <- x - mean(x, na.rm=TRUE)
			sd_x <- sd(x, na.rm=TRUE)
			if(sd_x == 0 | !is.finite(sd_x)){sd_x <- 1}
			x <- x/sd_x
			x
		}
		vv2 <- apply(vv, 2, scale2)

		dimnames(vv2) <- dimnames(vv)
		x <- as.numeric(rownames(vv2))
		y <- as.numeric(colnames(vv2))
		z <- vv2

		mid <- c()
		# scale_sum1 <- function(x){
	# 		zero_min <- (x - min(x))
	# 		zero_min/sum(zero_min)
	# 	}
		for(i in 1:length(x)){
			# mid[i] <- weighted.mean(y, w=scale_sum1(z[i,]))
			mid[i] <- t_mean(z[i,], y)
			
			if(i == 1){
				sk <- t_skew(z[i,], y)
				bul <- t_bulge(z[i,], y)
			}else{
				sk <- rbind(sk, t_skew(z[i,], y))
				bul <- rbind(bul, t_bulge(z[i,], y))
			}
		}
	
		return(list(vv=vv, mid=mid, sk=sk, bul=bul, x=x, y=y, z=z))
	}

	mass_l <- cast_tl(Xa2, "m")
	mass_l$mtl <- mtl_mass
	mass_l$lcbd <- lcbd_mass
	# image.plot(x=mass_l$x, y=mass_l$y, z=mass_l$z, xlab="year", ylab="trophic level")
	# lines(x=mass_l$x, y=mass_l$mid, col="white", lwd=2)

	rich_l <- cast_tl(Xa2, "r")
	rich_l$mtl <- mtl_rich
	rich_l$lcbd <- lcbd_rich
	# image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$z, xlab="year", ylab="trophic level")
	# lines(x=rich_l$x, y=rich_l$mid, col="white", lwd=2)
	
	return(list(mass_l=mass_l, rich_l=rich_l, X=Xa, btemp=btemp, btemp_09=btemp_09, btemp_01=btemp_01, ll=ll))
	
}


getShape <- function(skew, bulge){
	skew_0 <- skew[,"slope_p"] > 0.05*2
	skew_p <- skew[,"slope"] > 0 & !skew_0
	skew_n <- skew[,"slope"] < 0 & !skew_0
	
	bulge_0 <- bulge[,"slope_p"] > 0.05*2 & skew_0
	bulge_p <- bulge[,"slope"] > 0 & !bulge_0 & skew_0
	bulge_n <- bulge[,"slope"] < 0 & !bulge_0 & skew_0
	
	shape <- rep(NA, length(skew_0))
	
	shape[skew_p] <- "downward_triangle"
	shape[skew_n] <- "upward_triangle"
	shape[bulge_0] <- "rectangle"
	shape[bulge_p] <- "diamond"
	shape[bulge_n] <- "hourglass"
	
	return(shape)
	
}


