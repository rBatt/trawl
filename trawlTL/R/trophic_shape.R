#' Calculate the trophic shape of a food web
#' 
#' Yap
#' 
#' @param reg character of trawl region
#' @param t_res trophic level resolution used in binning
#' 
#' @examples
neus_ts <- trophic_shape("shelf", t_res=0.5)

mass_l <- neus_ts$mass_l
rich_l <- neus_ts$rich_l

# sum1_z <- scale_sum1(mass_l$z)
# h <- sum1_z[1,] + mass_l$x[1]
# barplot(h, horiz=T, space=0, border=NA, xlim=c(mass_l$x[1], max(h)))
# lines(y=1:7, x=sum1_z[1,] , type="s")

library(fields)
image.plot(x=mass_l$x, y=mass_l$y, z=mass_l$z, xlab="year", ylab="trophic level")
lines(x=mass_l$x, y=mass_l$bulge, col="white", lwd=2)

image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$z, xlab="year", ylab="trophic level")
lines(x=rich_l$x, y=rich_l$bulge, col="white", lwd=2)

trophic_shape <- function(reg, t_res=0.5){

	X <- trawlTrim(reg, c.add=c("trophicLevel","trophicLevel.se","sex"))[!is.na(trophicLevel)]
	
	if(reg=="neus"){
		Xa <- trawlAgg(X, bio_lvl="sex", space_lvl="haulid", time_lvl="haulid", bioFun=meanna, envFun=meanna, metaCols=c("reg","datetime","year","common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		Xa[,time_lvl:=NULL]
		
		Xa <- trawlAgg(Xa, bio_lvl="spp", space_lvl="reg", time_lvl="year", bioFun=sumna, envFun=meanna, metaCols=c("common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		
		setnames(Xa, "time_lvl", "year")
		
	}else{
		Xa <- trawlAgg(X, bio_lvl="spp", space_lvl="reg", time_lvl="year", bioFun=meanna, envFun=meanna, metaCols=c("common","trophicLevel","trophicLevel.se"), meta.action="unique1")
		setnames(Xa, "time_lvl", "year")
	}


	setkey(Xa, reg, year, spp, common)

	round_tl <- Xa[,floor(trophicLevel/t_res)*t_res]
	cut_seq <- seq(min(round_tl), max(round_tl), by=t_res)
	Xa[,tg:=as.character(cut(trophicLevel, breaks=cut_seq, labels=cut_seq[-1], include.lowest=TRUE))]
	Xa[is.na(tg) & trophicLevel>=max(cut_seq), tg:=as.character(max(cut_seq))]
	Xa[,m:=wtcpue]
	Xa[,nObs:=nAgg]
	Xa[,nAgg:=NULL]
	Xa[,r:=1]
	Xa[,datetime:=as.POSIXct(year, format="%Y")]
	
	
	Xa2 <- trawlAgg(Xa, bio_lvl="tg", time_lvl="year", space_lvl="reg", bioCols=c("nObs","r","m"), envCols=c("stemp","btemp","depth"), bioFun=sum, envFun=mean, metaCols="trophicLevel.se", meta.action="FUN", metaFun=function(x, ...)sum(x^2, ...))
	setnames(Xa2, "time_lvl", "year")
	setkey(Xa2, reg, year, tg)

	cast_tl <- function(xa, value.var){
		vv <- reshape2::acast(xa, year~tg, value.var=value.var, fill=0)
		vv2 <- apply(vv, 2, scale)

		dimnames(vv2) <- dimnames(vv)
		x <- as.numeric(rownames(vv2))
		y <- as.numeric(colnames(vv2))
		z <- vv2

		bulge <- c()
		scale_sum1 <- function(x){
			zero_min <- (x - min(x))
			zero_min/sum(zero_min)
		}
		for(i in 1:length(x)){
			bulge[i] <- weighted.mean(y, w=scale_sum1(z[i,]))
		}
	
		return(list(vv=vv, bulge=bulge, x=x, y=y, z=z))
	}

	mass_l <- cast_tl(Xa2, "m")
	# image.plot(x=mass_l$x, y=mass_l$y, z=mass_l$z, xlab="year", ylab="trophic level")
	# lines(x=mass_l$x, y=mass_l$bulge, col="white", lwd=2)

	rich_l <- cast_tl(Xa2, "r")
	# image.plot(x=rich_l$x, y=rich_l$y, z=rich_l$z, xlab="year", ylab="trophic level")
	# lines(x=rich_l$x, y=rich_l$bulge, col="white", lwd=2)
	
	return(list(mass_l=mass_l, rich_l=rich_l))
	
}
