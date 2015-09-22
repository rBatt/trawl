setwd("~/Documents/Collaborations/Ryan Batt/trawl/Data/temporary")

library(data.table)


load("neus.RData")
# neus$lat <- as.numeric(neus$lat)
# neus$lon <- as.numeric(neus$lon)

#neus[, test:=5]

#neus[,c("test"):=list(5)]
#test <- data.table(blah=1:5)




table(neus$lat)
table(neus$lon)
table(neus$stratum)
# 
# ### sub-stratum
# neus$sub.s <- paste(neus$stratum, neus$K, sep=" ")
# 
# ### Master sub-stratum
# master.sub.s <- aggregate(btemp ~ sub.s + year, data=neus, FUN=mean)
# library(reshape)
# master.sub.s <- rename(master.sub.s, c(btemp = "Avg.btemp.sub.s"))
# 
# ### Master sp
# master.spp <- aggregate(btemp ~ spp, data=neus, FUN=mean)
# 
# #library(reshape)
# master.spp <- rename(master.spp, c(btemp="Avg.btemp.sp"))
# 
# master.space.spp <- merge(master.sub.s, master.spp, all.x=T)
# 
# ### Merge with data to create zeros where sampled but not present
# neus2 <- merge(neus, master.space.spp, all.y=T)
# 
# ### Assign zeros for wtcpue where sampled but not observed
# neus2$wtcpue2 <- ifelse(is.na(neus2$wtcpue), 0, neus2$wtcpue)
# 
# ###
# neus3 <- colsplit(neus2$sub.s, split=" ", names=c("lon2", "lat2", "K2"))
# neus3$sub.s <- neus2$sub.s
# 
# neus4 <- merge(neus2, neus3, by="sub.s")
# 
BSB <- subset(neus, spp == "Centropristis striata")
lob <- subset(neus, spp == "Homarus americanus")
cod <- subset(neus, spp == "Gadus morhua")


# 

### I don't see an easy way of incorporating the biomass estimates
### So instead just do presence absence for now by extracting the rows where not observed
BSB.present <- subset(neus, spp == "Centropristis striata" & wtcpue > 0)
lob.present <- cod <- subset(neus, spp == "Homarus americanus" & wtcpue > 0)
cod.present <- cod <- subset(neus, spp == "Gadus morhua" & wtcpue > 0)



library(hypervolume)

# BSB.space <- subset(BSB, select=c(lat.num, lon.num,year))
# cod.space <- subset(cod, select=c(lat.num, lon.num,year))
# lob.space <- subset(lob, select=c(lat.num, lon.num,year))

BSB.space <- subset(BSB.present, select=c(lat.num, lon.num))
cod.space <- subset(cod.present, select=c(lat.num, lon.num))
lob.space <- subset(lob.present, select=c(lat.num, lon.num))

cod.h <- hypervolume(cod.space, bandwidth=5)
plot(cod.h)

lob.h <- hypervolume(lob.space, bandwidth=0.5)
plot(lob.h, ylim=c(34, 46))

BSB.h <- hypervolume(BSB.space, bandwidth=0.5)
plot(BSB.h, ylim=c(34, 46))

sets <- hypervolume_set(lob.h, BSB.h, check_memory=FALSE)

### 
library(spatstat)
### Lobster
lob.late <- subset(lob, year >= 2000)
lob.late.agg <- aggregate(wtcpue ~ stratum + lat + lon+lat.num + lon.num, data=lob.late, FUN=mean)

lob.early <- subset(lob, year <= 1985)
lob.early.agg <- aggregate(wtcpue ~ stratum + lat + lon+lat.num + lon.num, data=lob.early, FUN=mean)

### Black Sea Bass
BSB.late <- subset(BSB, year >= 2000)
BSB.late.agg <- aggregate(wtcpue ~ stratum + lat + lon +lat.num + lon.num, data=BSB.late, FUN=mean)

BSB.early <- subset(BSB, year <= 1985)
BSB.early.agg <- aggregate(wtcpue ~ stratum + lat + lon+lat.num + lon.num, data=BSB.early, FUN=mean)

### Cod
cod.late <- subset(cod, year >= 2000)
cod.late.agg <- aggregate(wtcpue ~ stratum + lat + lon+lat.num + lon.num, data=cod.late, FUN=mean)

cod.early <- subset(cod, year <= 1985)
cod.early.agg <- aggregate(wtcpue ~ stratum + lat + lon+lat.num + lon.num, data=cod.early, FUN=mean)


### Fraction of biomass in each cell
fcell <- function(agg.data){
  p <- rep(NA, length(agg.data$wtcpue))
  for(i in 1:length(p)){
    p[i] <- agg.data$wtcpue[i] / sum(agg.data$wtcpue)
  }
  return(p)
}

fcod.early <- fcell(cod.early.agg)
fcod.late <- fcell(cod.late.agg)
flob.early <- fcell(lob.early.agg)
flob.late <- fcell(lob.late.agg)
fBSB.early <- fcell(BSB.early.agg)
fBSB.late <- fcell(BSB.late.agg)

schoener <- function(p1, p2){
  over <- rep(NA, length(p1))
  for(i in 1:length(over)){
    over[i] <- abs(p1[i] - p2[i])
  }
  
  SI <- 1- 0.5*sum(over)
}

cod.lob.early <- schoener(fcod.early, flob.early)
cod.lob.late <- schoener(fcod.late, flob.late)
BSB.lob.early <- schoener(fBSB.early, flob.early)
BSB.lob.late <- schoener(fBSB.late, flob.late)




###
lob.late.pattern <- ppp(lob.late.agg$lon.num, lob.late.agg$lat.num, 
                   c(min(lob.late.agg$lon.num), max(lob.late.agg$lon.num)), 
                   c(min(lob.late.agg$lat.num), max(lob.late.agg$lat.num)))

marks(lob.late.pattern) <- lob.late.agg$wtcpue

###
BSB.early.pattern <- ppp(BSB.early.agg$lon.num, BSB.early.agg$lat.num, 
                   c(min(BSB.early.agg$lon.num), max(BSB.early.agg$lon.num)), 
                   c(min(BSB.early.agg$lat.num), max(BSB.early.agg$lat.num)))
marks(BSB.early.pattern) <- BSB.early.agg$wtcpue

BSB.late.pattern <- ppp(BSB.late.agg$lon.num, BSB.late.agg$lat.num, 
                        c(min(BSB.late.agg$lon.num), max(BSB.late.agg$lon.num)), 
                        c(min(BSB.late.agg$lat.num), max(BSB.late.agg$lat.num)))

marks(BSB.late.pattern) <- BSB.late.agg$wtcpue

###
cod.early.pattern <- ppp(cod.early.agg$lon.num, cod.early.agg$lat.num, 
                         c(min(cod.early.agg$lon.num), max(cod.early.agg$lon.num)), 
                         c(min(cod.early.agg$lat.num), max(cod.early.agg$lat.num)))
marks(cod.early.pattern) <- cod.early.agg$wtcpue

cod.late.pattern <- ppp(cod.late.agg$lon.num, cod.late.agg$lat.num, 
                        c(min(cod.late.agg$lon.num), max(cod.late.agg$lon.num)), 
                        c(min(cod.late.agg$lat.num), max(cod.late.agg$lat.num)))

marks(cod.late.pattern) <- cod.late.agg$wtcpue

quartz()
par(mfrow=c(3,2))

cod.early.smooth <- Smooth(cod.early.pattern)
cod.late.smooth <- Smooth(cod.late.pattern)
lob.early.smooth <- Smooth(lob.early.pattern)
lob.late.smooth <- Smooth(lob.late.pattern)
BSB.early.smooth <- Smooth(BSB.early.pattern)
BSB.late.smooth <- Smooth(BSB.late.pattern)



### Get rid of lat/lon combos that aren't in sampled grid

clean.grid <- function(test){
  pa <- test[,list(stratum, lon, lat, pa=1)]
  pa <- pa[!duplicated(stratum)]
  setkey(pa, lon, lat)
  pa0 <- copy(pa)
  pa[,stratum:=NULL]
  
  skeleton <- pa[,expand.grid(lon=seq(min(lon),max(lon),by=0.1), lat=seq(min(lat),max(lat),by=0.1))]
  skeleton <- as.data.table(skeleton)
  setkey(skeleton, lon, lat)
  pa <- pa[skeleton]
  
  pa[is.na(pa), pa:=0]
  
  
  pa[paste(roundGrid(lon), roundGrid(lat))%in%pa0[,stratum], pa:=1]
  
  reshape2:::acast(pa, lat~lon)
  return(pa)
}

fine.grid.clean <- clean.grid(neus)




# pa2 <- as.data.frame(pa)
# pa2[is.na(pa2[,"pa"])] <- 0



quartz()
filled.contour(x=cod.early.smooth$xcol, y=cod.early.smooth$yrow, 
               z=t(cod.early.smooth$v), zlim=c(0, max(cod.early.smooth$v)),
               col=terrain.colors(20))
quartz()
filled.contour(x=cod.late.smooth$xcol, y=cod.late.smooth$yrow, 
               z=t(cod.late.smooth$v), zlim=c(0, max(cod.early.smooth$v)),
               col=terrain.colors(20))
quartz()
filled.contour(x=lob.early.smooth$xcol, y=lob.early.smooth$yrow, 
               z=t(lob.early.smooth$v), zlim=c(0, max(lob.late.smooth$v)),
               col=terrain.colors(20))
quartz()
filled.contour(x=lob.late.smooth$xcol, y=lob.late.smooth$yrow, 
               z=t(lob.late.smooth$v), zlim=c(0, max(lob.late.smooth$v)),
               col=terrain.colors(20))

###########
quartz()
par(mfrow=c(3,2))
contour(x=cod.early.smooth$xcol, y=cod.early.smooth$yrow, 
               z=t(cod.early.smooth$v)) 
        #zlim=c(0, max(cod.early.smooth$v)))

contour(x=cod.late.smooth$xcol, y=cod.late.smooth$yrow, 
               z=t(cod.late.smooth$v)) 
       # zlim=c(0, max(cod.early.smooth$v)))

contour(x=lob.early.smooth$xcol, y=lob.early.smooth$yrow, 
        z=t(lob.early.smooth$v)) 
        #zlim=c(0, max(lob.late.smooth$v)))

contour(x=lob.late.smooth$xcol, y=lob.late.smooth$yrow, 
        z=t(lob.late.smooth$v)) 
        #zlim=c(0, max(lob.late.smooth$v)))

contour(x=BSB.early.smooth$xcol, y=BSB.early.smooth$yrow, 
        z=t(BSB.early.smooth$v)) 
        #zlim=c(0, max(BSB.late.smooth$v)))

contour(x=BSB.late.smooth$xcol, y=BSB.late.smooth$yrow, 
        z=t(BSB.late.smooth$v)) 
        #zlim=c(0, max(BSB.late.smooth$v)))

#####
quartz()
par(mfrow=c(3,2))
plot(Smooth(cod.early.pattern), main="Cod Early")
plot(Smooth(cod.late.pattern), main="Cod Late")

plot(Smooth(lob.early.pattern), main="Lobster Early")
plot(Smooth(lob.late.pattern), main="Lobster Late")

plot(Smooth(BSB.early.pattern), main="BSB Early")
plot(Smooth(BSB.late.pattern), main="BSB Late")

### Just use one dimensional density function


ratio.max.cod <- max(cod.early.agg$wtcpue)/max(cod.late.agg$wtcpue)
ratio.max.lob <- max(lob.early.agg$wtcpue)/max(lob.late.agg$wtcpue)
ratio.max.BSB <- max(BSB.early.agg$wtcpue)/max(BSB.late.agg$wtcpue)



### Raw biomass
quartz()
par(mfrow=c(3,2))
symbols(cod.early.agg$lon.num, cod.early.agg$lat.num, circles=cod.early.agg$wtcpue, inches=0.25*ratio.max.cod,
        main="Cod 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(cod.late.agg$lon.num, cod.late.agg$lat.num, circles=cod.late.agg$wtcpue, inches=0.25,
        main="Cod 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(lob.early.agg$lon.num, lob.early.agg$lat.num, circles=lob.early.agg$wtcpue, inches=0.25*ratio.max.lob,
        main="Lobster 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(lob.late.agg$lon.num, lob.late.agg$lat.num, circles=lob.late.agg$wtcpue, inches=0.25,
        main="Lobster 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(BSB.early.agg$lon.num, BSB.early.agg$lat.num, circles=BSB.early.agg$wtcpue, inches=0.25*ratio.max.BSB,
        main="BSB 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(BSB.late.agg$lon.num, BSB.late.agg$lat.num, circles=BSB.late.agg$wtcpue, inches=0.25,
        main="BSB 2000-2013", xlab="Longitude", ylab="Latitude")

### Fraction of biomass in time period
### Doesn't result in any graphical differences because I am forcing the largest value in the set in each time period
### to have a diameter of 0.25"
quartz()
par(mfrow=c(3,2))
symbols(cod.early.agg$lon.num, cod.early.agg$lat.num, circles=fcod.early, inches=0.25,
        main="Cod 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(cod.late.agg$lon.num, cod.late.agg$lat.num, circles=fcod.late, inches=0.25,
        main="Cod 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(lob.early.agg$lon.num, lob.early.agg$lat.num, circles=flob.early, inches=0.25,
        main="Lobster 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(lob.late.agg$lon.num, lob.late.agg$lat.num, circles=flob.late, inches=0.25,
        main="Lobster 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(BSB.early.agg$lon.num, BSB.early.agg$lat.num, circles=fBSB.early, inches=0.25,
        main="BSB 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(BSB.late.agg$lon.num, BSB.late.agg$lat.num, circles=fBSB.late, inches=0.25,
        main="BSB 2000-2013", xlab="Longitude", ylab="Latitude")

