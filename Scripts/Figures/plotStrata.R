
# ==================
# = Load Libraries =
# ==================
library(data.table)
library(maps)

# =============
# = Load Data =
# =============
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")


# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


# =======================
# = Load Plot Functions =
# =======================
plot.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/PlotFunctions"
invisible(sapply(paste(plot.location, list.files(plot.location), sep="/"), source, .GlobalEnv))



# =======================
# = Load Stat Functions =
# =======================
stat.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/StatFunctions"
invisible(sapply(paste(stat.location, list.files(stat.location), sep="/"), source, .GlobalEnv))


# ================================
# = Combine 2 West Coast surveys =
# ================================
trawl[s.reg=="wcann",s.reg:="wc"]
trawl[s.reg=="wctri",s.reg:="wc"]
setkey(trawl, spp, s.reg, year)


# ===========================
# = Set Color Scheme Column =
# ===========================
reg.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(11)
trawl[,col:=reg.cols[as.factor(s.reg)]]


# ==========================
# = Set up lat/lon cushion =
# ==========================
cshn <- 5


# ==============================================================
# = Expressions for plotting sampling locations in each survey =
# ==============================================================
# For multi panel figure
map.reg <- bquote(map(regions=c("USA","Canada", "Mexico"), xlim=range(lon, na.rm=TRUE)+c(-cshn, cshn), ylim=range(lat, na.rm=TRUE)+c(-cshn, cshn), fill=TRUE, col="gray", mar=c(0.25,0.25,0.25,0.25)))
plot.ll <- bquote(points(lon, lat, pch=20, cex=0.5, col=col))

# For 1 panel full map figure
map.full <- bquote(map(regions=c("USA","Canada", "Mexico"), xlim=range(lon, na.rm=TRUE), ylim=range(lat, na.rm=TRUE), fill=TRUE, col="gray", mar=c(0.25,0.25,0.25,0.25), lwd=0.5))
ll.full <- bquote(points(lon, lat, pch=21, cex=0.25, bg=col))
# reg.hull <- bquote(trawl[,polygon(cbind(lon, lat)[chull(lon, lat),]), by="s.reg"])
# reg.hull <- bquote(trawl[,splineHull(list(x=x, y=y)), by="s.reg"])



# ==========================
# = Some helpful functions =
# ==========================
# Length Unique: Convenience function for counting uniques
lu <- function(x) length(unique(x))


# =================
# = Round lon-lat =
# =================
trawl.ull <- trawl[,c("lat","lon"):=list(round(lat,2), round(lon,2))]
# setkey(trawl.ull, lat, lon, s.reg)


# ==============================
# = Plot each stratum, and all =
# ==============================
# dev.new(width=8, height=6)
png(width=8, height=6, file="~/Documents/School&Work/pinskyPost/trawl/Figures/stratHauls_byRegion.png", res=150, units="in")
par(mfrow=c(4,3), mar=c(1,1,0.5,0.5), ps=8, cex=1, family="Times", mgp=c(1, 0.25, 0), tcl=-0.15)


# Each stratum
unique(trawl.ull, by=c("lat", "lon"))[,j={
	eval(map.reg)
	eval(plot.ll)
	t.sd <- .SD
}, by="s.reg"]

# all strata as last panel
unique(trawl.ull, by=c("lat", "lon"))[,j={
	eval(map.reg)
	eval(plot.ll)
}]
dev.off()

# ===============================
# = Whole figure for all strata =
# ===============================
# yrange <- trawl[,range(lat)+c(-cshn,cshn)]
# xrange <- trawl[,range(lon)+c(-cshn,cshn)]
# aspect <- c(cos((mean(yrange) * pi)/180), 1)
# d <- c(diff(xrange), diff(yrange)) * (1 + 2 * 0.01) * aspect

png(height=3, width=trawl.ull[,map.w(lat,lon,3)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/stratHauls_full.png", units="in", res=150)
par(mar=c(1,1,0.5,0.5), ps=8, cex=1, family="Times", mgp=c(1, 0.25, 0), tcl=-0.15)
unique(trawl.ull, by=c("lat", "lon"))[,j={
	eval(map.full)
	eval(plot.ll)
}]

# Add region outlines
# uni.trawl.points <- unique(trawl.ull, by=c("lat", "lon"))
# setnames(uni.trawl.points, c("lat","lon"), c("y","x"))
# uni.trawl.points[,splineHull(list(x=x, y=y), aval=ifelse(unique(s.reg)%in%c("ai","sa","wc"),10,1)), by="s.reg"]
dev.off()



