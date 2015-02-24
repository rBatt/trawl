

# =================
# = Load Packages =
# =================
library(data.table)


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



# ==============================
# = Loop to Load All Summaries =
# ==============================
smryDirectory <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/Richness/msomCov/msomCov.smry/"
smryNameID_file <- list.files(smryDirectory)[grepl(".*\\.RData", list.files(smryDirectory))] # summary file names
smryNameID.reg <- regexpr("[a-z]+_[0-9]{4}_[0-9]+?(?=_smry\\.RData$)", smryNameID_file, perl=TRUE) # regular expression to get name to use for new object name
smryNameID <- regmatches(smryNameID_file, smryNameID.reg) # object name for each summary file


# =============
# = Load Data =
# =============
load("./trawl/Data/MSOM/prepd.msom.cov.RData")




# ==========================================
# = Load regions locally, combine globally =
# ==========================================
all.out <- list()
local({
	for(i in 1:length(smryNameID_file)){
		load(paste(smryDirectory, smryNameID_file[i], sep="/")) # load each .RData file
		# assign()
		all.out[[i]] <<- mget("out") # assign to global, or if found in a parent, reassign in that environment
	}
	# print(ls()) # note that ls() default is to current frame, so only see locally-defined variables with ls()
	rm(list=ls()) # thus removing ls() preserves trawl and all variables defined outside of call to local()
})
all.out <- unlist(all.out, F, F)
names(all.out) <- smryNameID


# ===============================
# = Add Identifying Information =
# ===============================
for(i in 1:length(all.out)){
	
	idKey <- prepd.cov.names[,nameID:=paste(s.reg,year,num,sep="_")]
	idNum <- as.numeric(idKey[nameID==smryNameID[i],num])
	
	t.ao <- all.out[[i]]
	if(i == 255){
		t.ao$mean$a1 <- NA
		t.ao$median$a1 <- NA
		t.ao$sd$a1 <- NA
		
		t.ao$mean$a2 <- NA
		t.ao$median$a2 <- NA
		t.ao$sd$a2 <- NA

	}

	idStrats <- dimnames(prepd.cov.dat[[idNum]])[1][[1]]
	idSpp <- dimnames(prepd.cov.dat[[idNum]])[3][[1]]
	lon.lat <- t(simplify2array(strsplit(idStrats, " ")))
	idLon <- as.numeric(lon.lat[,1])
	idLat <- as.numeric(lon.lat[,2])
	
	into.out <- list(stratum=idStrats, spp=idSpp, lon=idLon, lat=idLat)
	
	all.out[[i]]$ID <- into.out
	
	n.spp <- length(t.ao$mean$a3)
	n0s <- n.spp - length(idSpp)
	spp.tot <- length(t.ao$mean$a3)
	all.spp <- c(idSpp, paste0("unobs",1:n0s))
	
	nS <- length(idStrats)
	
	if(i==1){
		rco <- data.table(
			idKey[idNum,],
			stratum=rep(idStrats, n.spp), 
			lon=rep(idLon, n.spp), 
			lat=rep(idLat, n.spp), 
			btemp=rep(prepd.cov1[[idNum]],n.spp), 
			depth=rep(prepd.cov2[[idNum]],n.spp),
			N=t.ao$median$N,
			Nsite=rep(t.ao$median$Nsite,n.spp),
			spp=rep(all.spp, each=nS),
			Z=c(t.ao$median$Z),
			u.a0=rep(t.ao$median$u.a0, each=nS),
			a1=rep(t.ao$median$a1, each=nS),
			a2=rep(t.ao$median$a2, each=nS),
			a3=rep(t.ao$median$a3, each=nS),
			a4=rep(t.ao$median$a4, each=nS)
			
		)
	}else{
		rco <- rbind(rco, data.table(
			idKey[idNum,],
			stratum=rep(idStrats, n.spp), 
			lon=rep(idLon, n.spp), 
			lat=rep(idLat, n.spp), 
			btemp=rep(prepd.cov1[[idNum]],n.spp), 
			depth=rep(prepd.cov2[[idNum]],n.spp),
			N=t.ao$median$N,
			Nsite=rep(t.ao$median$Nsite,n.spp),
			spp=rep(all.spp, each=nS),
			Z=c(t.ao$median$Z),
			u.a0=rep(t.ao$median$u.a0, each=nS),
			a1=rep(t.ao$median$a1, each=nS),
			a2=rep(t.ao$median$a2, each=nS),
			a3=rep(t.ao$median$a3, each=nS),
			a4=rep(t.ao$median$a4, each=nS)
		
		))
	}
	
}

ao.rco <- all.out

rco[,year:=as.numeric(as.character(year))]

rco <- rco[s.reg!="wcann" | (s.reg=="wcann" & year > 2003)]
rco[s.reg=="wcann" | s.reg=="wctri", s.reg:="wc"]

setkey(rco, s.reg, year, stratum, spp)


# ============================
# = Aggregate RCO across spp =
# ============================
# Leaving site and year
rco.sy <- copy(rco)
rco.sy[,c("spp","Z", "u.a0","a1","a2","a3","a4"):=NULL] # these values aren't useful w/o species distinction
setkey(rco.sy, s.reg, year, stratum)
rco.sy <- unique(rco.sy)


# =====================================
# = Aggregate RCO across spp and year =
# =====================================
qSlope <- function(x, y){if(length(x)<2){return(NA_real_)}else{lm(y~x)$coef[2]}}
rco.s <- copy(rco.sy)
rco.s[,nameID:=NULL]
rco.s <- rco.s[,
	list(
		mu.btemp=mean(btemp, na.rm=TRUE),
		slope.btemp=qSlope(year,btemp),
		mu.depth=mean(depth, na.rm=TRUE),
		slope.depth=qSlope(year, depth),
		mu.N=mean(N, na.rm=TRUE),
		slope.N=qSlope(year, N),
		mu.Nsite=mean(Nsite, na.rm=TRUE),
		slope.Nsite=qSlope(year, Nsite)
	),
	by=c("s.reg","stratum","lon","lat")
]






save(rco, file="./trawl/Results/Richness/rco.RData")
save(ao.rco, file="./trawl/Results/Richness/ao.rco.RData")
save(rco.s, file="./trawl/Results/Richness/rco.s.RData")
save(rco.sy, file="./trawl/Results/Richness/rco.sy.RData")

