
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


# =============
# = Load Data =
# =============
load("./trawl/Data/OS_2016/ebs.merge.trawlMSOM.RData")
ebs2 <- ebs.merge.trawlMSOM
rm(list="ebs.merge.trawlMSOM")


# =======================================
# = Write New Functions for this Script =
# =======================================

# function for finding the first or last 1
ce.ever <- function(now, type=c("first","last")){
	type <- match.arg(type)
	ever <- numeric(length(now))
	now.is1 <- now==1
	if(any(now.is1)){
		if(type=="first"){
			ever[which(now.is1)[1]] <- 1
		}else if(type=="last"){
			ever[rev(which(now.is1))[1]] <- 1
		}
		
	}
	return(ever)
}


ce <- function(Z){
	ce <- diff(c(0,Z)) # add 0 to front so that first year counts as a colonization if it's a 1
	
	# local (time and space) colonization/ extinction
	c.now <- c(0,1)[(ce==1)+1]
	e.now <- c(0,1)[(ce==-1)+1]
	
	# global (time only) colonization/ extinction
	# first colonization ever
	# last extinction ever
	c.ever <- ce.ever(c.now, "first")
	e.ever <- ce.ever(e.now, "last")
	
	# first and last observation, ever
	first.obs <- ce.ever(Z, "first")
	last.obs <- ce.ever(Z, "last")
	
	data.table(Z=Z, c.now=c.now, e.now=e.now, c.ever=c.ever, e.ever=e.ever, first.obs=first.obs, last.obs=last.obs)
}
	

# ==========================================
# = Calculate Colonization and Extinctions =
# ==========================================

# first, calculate local
ebs3 <- ebs2[,j={ # spp%in%c("Acantholithodes hispidus","Zaprora silenus")
	ce.out <- ce(Z)
	ce.out[,Z:=NULL]
	data.table(.SD,ce.out)
	
},by=c("stratum","spp")]
setkey(ebs3, spp, year)


# next, calculate regional
ebs4 <- ebs3[,j={
	Z.anywhere <- .SD[,max(Z,na.rm=T),by="year"][,V1]
	ce.out <- ce(Z=Z.anywhere)
	
	setnames(ce.out, names(ce.out), paste0(names(ce.out),".anywhere"))
	data.table(year=unique(year),ce.out)
	
},by=c("spp")]
setkey(ebs4, spp, year)

# last, merge regional into local
ebs5 <- ebs3[ebs4]

# ===================================================
# = Save EBS with colonization/ extinction as 'ebs' =
# ===================================================
ebs <- ebs5
ebs[,c("lon","lat"):=list(as.numeric(lon), as.numeric(lat))]
setorder(ebs, lat, lon, stratum, spp, year)
save(ebs, file="./trawl/Data/OS_2016/ebs.RData", compress="xz")


# =====================================================
# = Briefly Explore Colonization/ Extinction Patterns =
# =====================================================
#+ regionalRich-ts fig.width=4, fig.height=6
#' explore tiem series of richness, colonization, and extinction  
#' note that first year of colonization is just first year of richness  
#' similarly, first year of extinction is just 0  
par(mfrow=c(3,1), mar=c(2,2,0.1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1)

reg.N <- ebs[,.SD[,list(za=as.integer(any(Z.anywhere==1))),by="spp"][,list(reg.N=sum(za))],by="year"]
reg.N[,plot(year,reg.N,type="l")]

reg.col <- ebs[,list(reg.col=max(c.now.anywhere)),by=c("year","spp")][,list(reg.col=sum(reg.col)),by="year"]
reg.col[,plot(.SD, type="l", ylab="regional colonizations")]

reg.ext <- ebs[,list(reg.ext=max(e.now.anywhere)),by=c("year","spp")][,list(reg.ext=sum(reg.ext)),by="year"]
reg.ext[,plot(.SD, type="l", ylab="regional extinctions")]




#+ regionalRich-scatter fig.width=4, fig.height=6
#' explore relationship between colonization and extinction  
#' drop first year of colonization (which is just richness)  
#' also dropping first year of extinction, which is 0  
par(mfrow=c(3,1), mar=c(2,2,0.1,0.1), mgp=c(1,0.15,0), tcl=-0.15, ps=8, cex=1)
ccf(reg.col[-1,reg.col], reg.ext[-1,reg.ext]) # cross correlation function; suggest no lag or colonization before extinction
plot(reg.col[-1,reg.col], reg.ext[-1,reg.ext]) # scatter plot with no lag shift
abline(a=0, b=1)
plot(reg.col[-c(1,lu(year)),reg.col], reg.ext[-c(1,2),reg.ext]) # colonization[t-1] vs extinction[t]
abline(a=0, b=1)

#' no lag, R2 of 0.15
(summary(lm(reg.col[-c(1),reg.col] ~ reg.ext[-c(1),reg.ext])))

#' colonization[t-1], R2 of 0.25
(summary(lm(reg.col[-c(1,lu(year)),reg.col] ~ reg.ext[-c(1,2),reg.ext])))

#' From this it seems that there is pretty good evidence that, at the regional scale, increases in the colonization rate tend to be followed by increases in the extinction rate.


#+ localRich-ts
#' Explore patterns in local richness
loc.N <- ebs[,list(N=sum(Z)),by=c("stratum","year")]
ylim <- loc.N[,range(N)]
xlim <- loc.N[,range(year)]
plot(1,1, type="n", ylim=ylim, xlim=xlim, ylab="", xlab="")
loc.N[,j={
	lines(year, N, col=adjustcolor("black", alpha.f=0.25))
},by="stratum"]

