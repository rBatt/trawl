
# =================
# = Load Packages =
# =================
library(data.table)


# ==================
# = Load Functions =
# ==================
data.location <- "./trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))

stat.location <- "./trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))

plot.location <- "./trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))


# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}



# ================
# = Load Results =
# ================
# First, load data for referencing
load("./trawl/Data/msom.dat.RData") # need msom.dat[[2]] for the region-year key to the rbo output list. Need dimnames(msom.dat[[1]])[2] in order to figure out which strata are in the rows of Z[,,,], and need dimnames(msom.dat[[1]])[3] to get the species (but remember the last 500 are just the augmented 0's)

if("rbo.RData"%in%list.files("./trawl/Results/Richness/")){
	load("./trawl/Results/Richness/rbo.RData")
}else{
	load("./trawl/Results/Richness/richness.basic.out.RData")
	load("./trawl/Results/Richness/last.out.RData")
	
	# NOTE: I screwed up the combine function for the run of richness.basic.R on 06-Feb-2015. I also forgot to run the last region-year combo. So for the first 274 I had to manually edit their combination, and only bothered with the sims.list (everything else is a summary of that anyway). Then when I noticed that I didn't run #275, I ran it by itself, called it last.out, and am adding it on here. I'm saving this as rbo.RData. So that richness.basic.out.RData is super messed up!

	rbo.11 <- richness.basic.out[[11]] # UGH
	rbo.11.1 <- unlist(rbo.11[[1]], F)

	# I got last out manually
	rbo <- c(rbo.11.1, unlist(rbo.11[-1], F), last.out[[11]])
	# save(rbo, file="./trawl/Results/Richness/rbo.RData", compress="xz")
	
}

# ===============================================
# = Need to redo prep.basic to drop same strata =
# ===============================================
prep.basic <- function(t.dat){	
	keep.strat <- apply(t.dat, 1, function(x)!all(is.na(x))) # which strata were never sampled?
	t.dat <- t.dat[keep.strat,,] # remove strata that were never sampled

	# If there are NA's 
	# orig.na.TF <- apply(t.dat, c(1,3), function(x){all(is.na(x))})# sometimes there were NA's in the original data – this implies the species was observed, but perhaps effort wasn't recorded (/ by 0), or some other reason why wtcpue couldn't be recorded.
	# if(any(orig.na.TF)){
		# stop("A species was NA for all reps in a stratum. Please check the following: 1) replace original NA's with 1, 2) if other species have non-NA for this s.reg-year-stratum-K, then this species should have been changed to 0 in expand.data, 3) if all species are NA for all reps in this s.reg-year-stratum, then the stratum should have been dropped at the start of this function")
		# print("found one!")
		# orig.na.ind <- which(orig.na.TF, arr.ind=TRUE) # not easy to use orig.na.TF as subset b/c it doesn't reference all 3 dimensions
		# orig.na.ind.c <- c(orig.na[,1], rep(1,nrow(orig.na)), orig.na[,2]) # we'll just change the first column (K) to not-NA
		# orig.na.2.1 <- matrix(orig.na.ind.c, ncol=3, dimnames=list(rownames(orig.na),c("dim1","dim2","dim3")))
		# t.dat[orig.na.2.1] <- 1 # all non-zero positive values will be turned into 1 in the occurrence model
	# }
	

	return(t.dat)

}

# =========
# = Get N =
# =========
rbo.N0 <- unlist(lapply(rbo, function(x)mode(x$N)), use.names=FALSE)
rbo.N <- msom.dat[[2]][,N:=rbo.N0]
rbo.N[,n.slope:=(lm(N~as.numeric(year))$coeff[2]), by="s.reg"]

# ==========
# = Plot N =
# ==========
# dev.new(height=8, width=4)
png("./trawl/Figures/Diversity/msom.basic.N.ts.png", width=4, height=8, res=200, units="in")
par(mfrow=c(6,2), mar=c(1,1,0.1,0.1), mgp=c(1.5,0.15,0), tcl=-0.15, cex=1, ps=9)
rbo.N[,
	j={
		plot(as.numeric(year), N, type="o", xlab="", ylab="")
		legend("topleft", legend=.BY[[1]], bty="n")
	},
	by=c("s.reg")
]
dev.off()


# =========
# = Get Z =
# =========
# Get the means
get.z.mu <- function(x)rowSums(apply(x$Z, c(2,3), mean))
rbo.Z0 <- lapply(rbo, get.z.mu)

# Format into data.table w/ ID info (stratum, year, region)
z1.msom.dat <- prep.basic(msom.dat[[1]][[1]])
rbo.Z.1.stratNames <- dimnames(z1.msom.dat)[[1]]
rbo.Z.1 <- data.table(msom.dat[[2]][1,], Z=rbo.Z0[[1]], stratum=rbo.Z.1.stratNames)
rbo.Z <- copy(rbo.Z.1)

for(i in 2:length(msom.dat[[1]])){
	t.z.msom.dat <- prep.basic(msom.dat[[1]][[i]])
	t.rbo.Z.stratNames <- dimnames(t.z.msom.dat)[[1]]
	t.rbo.Z <- data.table(msom.dat[[2]][i,], Z=rbo.Z0[[i]], stratum=t.rbo.Z.stratNames)
	rbo.Z <- rbind(rbo.Z, t.rbo.Z)
}

# Split stratum name into lon lat
# mapply(c, strsplit(c("-166.5 54.5", "-167.5 53.5", "-168.5 53.5"), " "))

invisible(rbo.Z[,
	j={
		LL0 <- mapply(c, strsplit(stratum, " "))
		Lons <<- as.numeric(LL0[1,])
		Lats <<- as.numeric(LL0[2,])
		# c("lon","lat") := list(Lons, Lats)
	}
])
rbo.Z[,c("lon","lat") := list(Lons, Lats)]


# =============================================
# = Create Slope of Richness for Each Stratum =
# =============================================
rbo.Z[,z.slope:=(lm(Z~as.numeric(year))$coeff[2]), by=c("s.reg","stratum")]


# =================
# = Plot Map of Z =
# =================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)
rbo.Z[,z.col:=heat.cols[cut(z.slope, 256)]]

# New device
# dev.new(height=4, width=beta.var.time[,map.w(lat,lon,4)])
pdf(height=4, width=rbo.Z[,map.w(lat,lon,4)], file="./trawl/Figures/Diversity/richness_slope_stratum_Z.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)

# Plot
rbo.Z[,plot(lon, lat, col=z.col, pch=21, cex=1, type="n")] # set plot region
invisible(rbo.Z[,map(add=TRUE, fill=TRUE, col="lightgray")]) # add map
rbo.Z[,points(lon, lat, bg=z.col, pch=21, cex=1)] # add points

# Key
rbo.Z[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)] # add colors for key
rbo.Z[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # add tick marks for key
rbo.Z[,text(-167, y=seq(30,40, length.out=4), round(seq(min(z.slope), max(z.slope), length.out=4),2), adj=1, cex=1, col="black")] # add labels for key
rbo.Z[,text(-162.5, 41.5, bquote(Richness~Trend~(spp~per~yr)))] # add label for key

dev.off()


# ========================
# = Save rbo.Z and rbo.N =
# ========================
save(rbo.Z, file="./trawl/Results/Richness/rbo.Z.RData")
save(rbo.N, file="./trawl/Results/Richness/rbo.N.RData")












# test <- do.call(Map, c(c, rbo)) # DON'T DO THIS – IT'LL CRASH OR TAKE FOREVER


