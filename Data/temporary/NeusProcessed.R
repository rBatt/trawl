setwd("~/Documents/Collaborations/Ryan Batt/trawl/Data/temporary")

library(data.table)
library(reshape2)

load("neus.RData")
# neus$lat <- as.numeric(neus$lat)
# neus$lon <- as.numeric(neus$lon)

#neus[, test:=5]

#neus[,c("test"):=list(5)]
#test <- data.table(blah=1:5)




table(neus$lat)
table(neus$lon)
table(neus$stratum)


### Get rid of lat/lon combos that aren't in sampled grid
roundGrid <- function(x, frac=1){
  # if frac is 1, then place in a 1º grid
  # if frac is 0.5, then place in the 0.5º grid
  floor(x/frac)*frac+frac/2
}

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
  
  # reshape2:::acast(pa, lat~lon)
  return(pa)
}

fine.grid.clean <- clean.grid(neus)

fine.grid.clean.matrix <-   reshape2:::acast(fine.grid.clean, lat~lon)
image(fine.grid.clean.matrix)

### Multiply the kernel density matrix by this fine.grid.clean matrix (0 where not in domain, 1 where in domain)



# 
BSB <- subset(neus, spp == "Centropristis striata")
lob <- subset(neus, spp == "Homarus americanus")
cod <- subset(neus, spp == "Gadus morhua")




### Within hypervolume
### I don't see an easy way of incorporating the biomass estimates
### So instead just do presence absence for now by extracting the rows where not observed
BSB.present <- subset(neus, spp == "Centropristis striata" & wtcpue > 0)
lob.present <- subset(neus, spp == "Homarus americanus" & wtcpue > 0)
cod.present <- subset(neus, spp == "Gadus morhua" & wtcpue > 0)

BSB.present.early <- subset(neus, spp == "Centropristis striata" & wtcpue > 0 & year <= 1985)
lob.present.early <- subset(neus, spp == "Homarus americanus" & wtcpue > 0 & year <= 1985)
cod.present.early <- subset(neus, spp == "Gadus morhua" & wtcpue > 0 & year <= 1985)

BSB.present.late <- subset(neus, spp == "Centropristis striata" & wtcpue > 0 & year >= 2000)
lob.present.late <- subset(neus, spp == "Homarus americanus" & wtcpue > 0 & year >= 2000)
cod.present.late <- subset(neus, spp == "Gadus morhua" & wtcpue > 0 & year >= 2000)

library(hypervolume)

# BSB.space <- subset(BSB, select=c(lat.num, lon.num,year))
# cod.space <- subset(cod, select=c(lat.num, lon.num,year))
# lob.space <- subset(lob, select=c(lat.num, lon.num,year))

BSB.space.early <- subset(BSB.present.early, select=c(lat, lon))
cod.space.early <- subset(cod.present.early, select=c(lat, lon))
lob.space.early <- subset(lob.present.early, select=c(lat, lon))

BSB.space.late <- subset(BSB.present.late, select=c(lat, lon))
cod.space.late <- subset(cod.present.late, select=c(lat, lon))
lob.space.late <- subset(lob.present.late, select=c(lat, lon))

cod.h.early <- hypervolume(cod.space.early, bandwidth=5)
cod.h.late <- hypervolume(cod.space.late, bandwidth=5)

plot(cod.h.early)

lob.h.early <- hypervolume(lob.space.early, bandwidth=10)
lob.h.late <- hypervolume(lob.space.late, bandwidth=10)

BSB.h.early <- hypervolume(BSB.space.early, bandwidth=10)
BSB.h.late <- hypervolume(BSB.space.late, bandwidth=10)

quartz()
plot(lob.h.early, ylim=c(34, 46))
quartz()
plot(lob.h.late, ylim=c(34, 46))

BSB.h <- hypervolume(BSB.space, bandwidth=0.5)
plot(BSB.h, ylim=c(34, 46))

sets <- hypervolume_set(lob.h, BSB.h, check_memory=FALSE)


####################
### Use spatial statistics package to do 2d spatial estimation with weights of wtcpue

### 
library(spatstat)
### Lobster
lob.late <- subset(lob, year >= 2000)
lob.late.agg <- aggregate(wtcpue ~ stratum + lat + lon, data=lob.late, FUN=mean)

lob.early <- subset(lob, year <= 1985)
lob.early.agg <- aggregate(wtcpue ~ stratum + lat + lon, data=lob.early, FUN=mean)

### Black Sea Bass
BSB.late <- subset(BSB, year >= 2000)
BSB.late.agg <- aggregate(wtcpue ~ stratum + lat + lon , data=BSB.late, FUN=mean)

BSB.early <- subset(BSB, year <= 1985)
BSB.early.agg <- aggregate(wtcpue ~ stratum + lat + lon, data=BSB.early, FUN=mean)

### Cod
cod.late <- subset(cod, year >= 2000)
cod.late.agg <- aggregate(wtcpue ~ stratum + lat + lon, data=cod.late, FUN=mean)

cod.early <- subset(cod, year <= 1985)
cod.early.agg <- aggregate(wtcpue ~ stratum + lat + lon, data=cod.early, FUN=mean)


###############
### Merge with clean grid to get matrix of values
cod.late.agg.grid <- merge(cod.late.agg, fine.grid.clean, all.y=T)
cod.late.agg.grid$wtcpue2 <- ifelse(is.na(cod.late.agg.grid$wtcpue),0,cod.late.agg.grid$wtcpue)

### This works but can't have NA in the Smoothing function so do the merge after do the Smooth



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


### Kernel density estimation
###
win=owin(range(neus$lon),range(neus$lat))

lob.early.pattern <- ppp(lob.early.agg$lon, lob.early.agg$lat, window=win)

test <- density(lob.early.pattern, weights=lob.early.pattern$wtcpue, at="points")
test <- density(lob.early.pattern, weights=lob.early.pattern$wtcpue, at="pixels")


test.df <- data.frame(lon=test$xcol, lat=test$yrow, kd=test$v)
test2 <- merge(test.df, fine.grid.clean)

# lob.early.pattern <- ppp(lob.early.agg$lon, lob.early.agg$lat, 
#                          c(min(neus$lon), max(neus$lon)), 
#                          c(min(neus$lat), max(neus$lat)))

marks(lob.early.pattern) <- lob.early.agg$wtcpue



###
lob.late.pattern <- ppp(lob.late.agg$lon, lob.late.agg$lat, 
                   c(min(lob.late.agg$lon), max(lob.late.agg$lon)), 
                   c(min(lob.late.agg$lat), max(lob.late.agg$lat)))

marks(lob.late.pattern) <- lob.late.agg$wtcpue

###
BSB.early.pattern <- ppp(BSB.early.agg$lon, BSB.early.agg$lat, 
                   c(min(BSB.early.agg$lon), max(BSB.early.agg$lon)), 
                   c(min(BSB.early.agg$lat), max(BSB.early.agg$lat)))
marks(BSB.early.pattern) <- BSB.early.agg$wtcpue

BSB.late.pattern <- ppp(BSB.late.agg$lon, BSB.late.agg$lat, 
                        c(min(BSB.late.agg$lon), max(BSB.late.agg$lon)), 
                        c(min(BSB.late.agg$lat), max(BSB.late.agg$lat)))

marks(BSB.late.pattern) <- BSB.late.agg$wtcpue

###
cod.early.pattern <- ppp(cod.early.agg$lon, cod.early.agg$lat, 
                         c(min(cod.early.agg$lon), max(cod.early.agg$lon)), 
                         c(min(cod.early.agg$lat), max(cod.early.agg$lat)))
marks(cod.early.pattern) <- cod.early.agg$wtcpue

cod.late.pattern <- ppp(cod.late.agg$lon, cod.late.agg$lat, 
                        c(min(cod.late.agg$lon), max(cod.late.agg$lon)), 
                        c(min(cod.late.agg$lat), max(cod.late.agg$lat)))

marks(cod.late.pattern) <- cod.late.agg$wtcpue

cod.late.pattern.grid <- ppp(cod.late.agg.grid$lon, cod.late.agg.grid$lat, 
                        c(min(cod.late.agg.grid$lon), max(cod.late.agg.grid$lon)), 
                        c(min(cod.late.agg.grid$lat), max(cod.late.agg.grid$lat)))

marks(cod.late.pattern.grid) <- cod.late.agg.grid$wtcpue2

### Save results of kernel smoothing

cod.early.smooth <- Smooth(cod.early.pattern, at="points")
cod.late.smooth <- Smooth(cod.late.pattern, at="points")
lob.early.smooth <- Smooth(lob.early.pattern, at="points")
lob.late.smooth <- Smooth(lob.late.pattern, at="points")
BSB.early.smooth <- Smooth(BSB.early.pattern, at="points")
BSB.late.smooth <- Smooth(BSB.late.pattern, at="points")

ratio.max.cod.smooth <- max(cod.early.smooth)/max(cod.late.smooth)
ratio.max.lob.smooth <- max(lob.early.smooth)/max(lob.late.smooth)
ratio.max.BSB.smooth <- max(BSB.early.smooth)/max(BSB.late.smooth)

### 
cod.early.smooth.df <- data.frame(lon=cod.early.pattern$x, lat=cod.early.pattern$y, kd=cod.early.smooth)

### Merge with clean grid to get matrix of values
cod.early.agg.grid <- merge(cod.early.smooth.df, fine.grid.clean, all.y=T)

### At all grid points
cod.early.smooth.grid <- Smooth(cod.early.pattern, at="pixels")
cod.late.smooth.grid <- Smooth(cod.late.pattern.grid, at="points")
cod.late.smooth.grid.df <- data.frame(lon=cod.late.pattern.grid$x, lat=cod.late.pattern.grid$y,
                                      kd=cod.late.smooth.grid)

### Merge with pa to get retained ones
cod.late.smooth.grid.df2 <- merge(cod.late.smooth.grid.df, fine.grid.clean, all.y=T)


lob.early.smooth.grid <- Smooth(lob.early.pattern, at="pixels")
lob.late.smooth.grid <- Smooth(lob.late.pattern, at="pixels")
BSB.early.smooth.grid <- Smooth(BSB.early.pattern, at="pixels")
BSB.late.smooth.grid <- Smooth(BSB.late.pattern, at="pixels")

round(cod.early.smooth.grid$xcol, 1)


#library(fields)
#out<- as.image( cod.early.agg$lat, x= cod.early.agg$lon, nx=101, ny=91) 

### From smoothed estimates
quartz()
par(mfrow=c(3,2))
symbols(cod.early.agg$lon, cod.early.agg$lat, circles=cod.early.smooth, inches=0.1*ratio.max.cod.smooth,
        main="Cod 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(cod.late.agg$lon, cod.late.agg$lat, circles=cod.late.smooth, inches=0.1,
        main="Cod 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(lob.early.agg$lon, lob.early.agg$lat, circles=lob.early.smooth, inches=0.1*ratio.max.lob.smooth,
        main="Lobster 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(lob.late.agg$lon, lob.late.agg$lat, circles=lob.late.smooth, inches=0.1,
        main="Lobster 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(BSB.early.agg$lon, BSB.early.agg$lat, circles=BSB.early.smooth, inches=0.1*ratio.max.BSB.smooth,
        main="BSB 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(BSB.late.agg$lon, BSB.late.agg$lat, circles=BSB.late.smooth, inches=0.1,
        main="BSB 2000-2013", xlab="Longitude", ylab="Latitude")




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



ratio.max.cod <- max(cod.early.agg$wtcpue)/max(cod.late.agg$wtcpue)
ratio.max.lob <- max(lob.early.agg$wtcpue)/max(lob.late.agg$wtcpue)
ratio.max.BSB <- max(BSB.early.agg$wtcpue)/max(BSB.late.agg$wtcpue)



### Raw biomass
quartz()
par(mfrow=c(3,2))
symbols(cod.early.agg$lon, cod.early.agg$lat, circles=cod.early.agg$wtcpue, inches=0.25*ratio.max.cod,
        main="Cod 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(cod.late.agg$lon, cod.late.agg$lat, circles=cod.late.agg$wtcpue, inches=0.25,
        main="Cod 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(lob.early.agg$lon, lob.early.agg$lat, circles=lob.early.agg$wtcpue, inches=0.25*ratio.max.lob,
        main="Lobster 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(lob.late.agg$lon, lob.late.agg$lat, circles=lob.late.agg$wtcpue, inches=0.25,
        main="Lobster 2000-2013", xlab="Longitude", ylab="Latitude")
symbols(BSB.early.agg$lon, BSB.early.agg$lat, circles=BSB.early.agg$wtcpue, inches=0.25*ratio.max.BSB,
        main="BSB 1968-1985", xlab="Longitude", ylab="Latitude")
symbols(BSB.late.agg$lon, BSB.late.agg$lat, circles=BSB.late.agg$wtcpue, inches=0.25,
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

