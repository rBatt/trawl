



# ===================
# = Alpha over time =
# ===================

alpha.turn.time.expr <- bquote({
	# print(paste(s.reg, stratum))
	# print(.SD)
	if(lu(year)>3){
		castExp <- acast(melt(.SD, id.vars=c("year","spp"), measure.vars=c("wtcpue")), year~spp)[,-1]
		d.helli <- diversity(castExp)
		
		print(d.helli)

		
		dX.yr <- 1:length(d.helli)
		
		good.y1 <- d.helli>0 # figure out which indices would throw error if took log
		dy1 <- d.helli[good.y1] #log(d.helli[good.y1])
		dX <- c(dX.yr)[good.y1]
		plot(dX,dy1, type="o")
		decay.slope <- lm(dy1~dX)$coef[2]
		decay.slope
		}else{
			as.numeric(NA)
		}

})



# alpha.turn.time <- trawl3[,list(lon=mean(lon), lat=mean(lat), turn.time=eval(alpha.turn.time.expr)), by=c("s.reg","stratum")]
alpha.turn.time <- trawl3[,
	j={
		list(lon=mean(lon), lat=mean(lat), turn.time=eval(alpha.turn.time.expr))
	}, 
	
	by=c("s.reg","stratum")
]
alpha.turn.time <- alpha.turn.time[!is.na(turn.time)&turn.time>0,]
alpha.turn.time[,turn.time:=log(turn.time)]

setkey(alpha.turn.time, s.reg, stratum)



