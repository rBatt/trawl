setwd("~/Documents/Collaborations/Ryan Batt/Detectability")

fish <- read.csv("EBS Spp ID Confidence Fish.csv")

library(reshape)

fish.m <- melt(fish, id.vars=c("Taxon"))

year <- colsplit(fish.m$variable, split="X", names=c(NA, "Year"))

fish.m$Year <- year$Year


### inverts
invert <- read.csv("EBS Spp ID Confidence Inverts.csv")
invert.m <- melt(invert, id.vars=c("Taxon"))

year.i <- colsplit(invert.m$variable, split="X", names=c(NA, "Year"))

invert.m$Year <- year.i$Year

write.csv(fish.m, "EBSFishConfidenceProc.csv")
write.csv(invert.m, "EBSInvertConfidenceProc.csv")

