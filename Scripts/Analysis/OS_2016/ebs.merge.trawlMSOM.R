
# =================
# = Load Packages =
# =================
library(reshape2)
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


# ==============
# = Load Trawl =
# ==============
load("~/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
load("./trawl/Results/Richness/rco.RData") 


# ==================================================
# = function to get trawl data for a single region =
# ==================================================
grab.trawlReg <- function(x, reg){
	out <- x[s.reg==reg]

	out[, lat2:=sapply(strsplit(stratum, " "), function(x){x[2]})]
	out[,lat:=lat2]
	out[,lat:=NULL]

	out[, lon2:=sapply(strsplit(stratum, " "), function(x){x[1]})]
	out[,lon:=lon2]
	out[,lon:=NULL]

	setnames(out, c("lat2","lon2"), c("lat", "lon"))


	out[, btemp:=mean(btemp, na.rm=TRUE), by=c("year", "stratum")]
	out[, stemp:=mean(stemp, na.rm=TRUE), by=c("year", "stratum")]
	out[, depth:=mean(depth, na.rm=TRUE), by=c("year", "stratum")]

	out[,c("correctSpp","taxLvl"):=NULL]
	
	return(out)
	
}

# =================
# = Grab EBS Data =
# =================
ebs <- grab.trawlReg(trawl, "ebs")
ebs <- ebs[!is.na(wtcpue)]
setkey(ebs, s.reg, stratum, year, spp, common)


# =================
# = Grab EBS MSOM =
# =================
# ebs.msom <- rco[s.reg=="ebs"&!grepl("unobs",spp), list(s.reg,stratum,year,spp,depth.msom=depth,btemp.msom=btemp,N,Nsite,Z,u.a0,a1,a2,a3,a4)]
ebs.msom <- rco[s.reg=="ebs"&!grepl("unobs",spp), list(s.reg,stratum,year,spp,N,Nsite,Z,u.a0,a1,a2,a3,a4)]
setkey(ebs.msom, s.reg, stratum, year, spp)


# =======================
# = Merge Data and MSOM =
# =======================
ebs2 <- ebs[ebs.msom]

ebs2[!duplicated(stratum),plot(depth,depth.msom)]
ebs2[!duplicated(stratum),plot(btemp,btemp.msom)]

# ==============
# = Save Merge =
# ==============
ebs.merge.trawlMSOM <- ebs2
save(ebs.merge.trawlMSOM, file="./trawl/Data/OS_2016/ebs.merge.trawlMSOM.RData", compress="xz")
