divData[wtcpue>0,
	list(
		granRich=lu(spp),
		yearMax.numSpp=.SD[,lu(spp), by=c("year","stratum")][,max(V1)],
		yearMax.year=.SD[,lu(spp), by=c("year","stratum")][,year[which.max(V1)]],
		yearMin.numSpp=.SD[,lu(spp), by=c("year","stratum")][,min(V1)],
		yearMax.year=.SD[,lu(spp), by=c("year","stratum")][,year[which.min(V1)]]
	),
	
	by=c("s.reg")
	
]

divData[wtcpue>0,
	# print(.SD[,list(rich=lu(spp)), by=c("year","stratum")][,timeSlope(rich),by=c("stratum")][,summary(V1)]),
	# list(
	# 	trend=.SD[,list(rich=lu(spp)), by=c("year","stratum")][,timeSlope(rich),by=c("stratum")]
	# ),
	
	{
		tRich <- .SD[,list(rich=lu(spp)), by=c("year","stratum")]
		tRich[,year:=as.numeric(as.character(year))]
		setkey(tRich, year, stratum)
		print(s.reg)
		tRich[,region:=s.reg]
		tRich[,
			{
				maxR <- tRich[,max(rich)]
				minR <- tRich[,min(rich)]
				maxY <- tRich[,max(year)]
				minY <- tRich[,min(year)]
				tRich[
					stratum==unique(stratum)[1],
					plot(year, rich, ylim=c(minR, maxR), xlim=c(minY,maxY), main=as.character(unique(region)), type="l")
				]
				for(i in 2:length(unique(stratum))){
					tRich[stratum==unique(stratum)[i],lines(year,rich)]
				}
				tRich[,mean(rich), by="year"][,lines(year,V1, col="red",lwd=2)]
			}
		]

	},
	
	by=c("s.reg")
	
]

