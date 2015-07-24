
library(RColorBrewer)
library(data.table)

# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# Read in data set, making sure that was use class==character, not factor
trophicTrawl <- read.csv("./trawl/Data/trophicTrawl/trophicTrawl.csv", stringsAsFactors=FALSE)

# subset to our favorite region for testing
tt.ai.index <- trophicTrawl[,"s.reg"] == "ai" # want aleutian islands for our test region
tt.ai <- trophicTrawl[tt.ai.index,] # perform the subset


# check for consistency
# table(tt.ai[,c("year","trophicGroup")]) # count combinations of year and trophicGroup (subset the tt.ai data set, then table looks at the unique combinations of year and trophicGroup, and returns a 1 where it finds that combination, and a 0 where the combination isn't found).

# ================================================
# = Make Graph of trophicGroup biomass over time =
# ================================================
# the column indicating the total biomass found for a given trophicGroup in a region in a year is "wtcpue.sum"
tgs <- sort(unique(tt.ai[,"trophicGroup"])) # unique trophic groups
# tgs <- c(tgs[length(tgs)], tgs[1:(length(tgs)-1)]) # a way to write code if we wanted "unknown" (which happens to be the last element after sorting) to be first, instead of last
# tgs <- c(tgs[tgs=="unknown"], tgs[tgs!="unknown"]) # a better way of coding the previous rearrangment
yrs <- unique(tt.ai[,"year"]) # unique years

# let's plot the biomass of the first trohpicGroup over time
# dev.new(width=3.5, height=3.5) # doesn't work in R studio
par(mar=c(2.25,2.25,0.1,0.1), ps=10, mgp=c(1.15, 0.25, 0), tcl=-0.15)
ylims <- range(sqrt(tt.ai[,"wtcpue.sum"]), na.rm=TRUE)
cols <- brewer.pal(n=length(tgs), name="Paired")

tg1.index <- tt.ai[,"trophicGroup"] == tgs[1]
plot(x=tt.ai[tg1.index,"year"], y=sqrt(tt.ai[tg1.index,"wtcpue.sum"]), type="o", ylab="mass", xlab="year", ylim=ylims, col=cols[1], pch=20)


# loop through trophic groups, plotting a line for each one
for(i in 2:length(tgs)){
	t.tgi <- tt.ai[,"trophicGroup"] == tgs[i]
	lines(x=tt.ai[t.tgi,"year"], y=sqrt(tt.ai[t.tgi,"wtcpue.sum"]), type="o", col=cols[i], pch=20)	
}
legend("topleft",legend=tgs, text.col=cols, ncol=2, cex=0.85) # add a legend, the inset argument adjusts the relative position of the legend, given the "topleft" starting location.

# legend("topleft",legend=tgs, text.col=cols, ncol=2, inset=c(-0.075,-0.025), cex=0.85) # add a legend, the inset argument adjusts the relative position of the legend, given the "topleft" starting location. The appearence will change depending on the size of the graphical device, which I set via dev.new, and which doesn't work in RStudio. So I'm commenting this line out.


# Plot shape over time
tt.ai.shape <- tt.ai[,c("year","trophicGroup","wtcpue.sum")]
tt.ai.shape[,"wtcpue.rel"] <- NA

# go through each trophicGroup, and turn it's wtcpue value into a value relative to its biggest value
for(i in 1:length(tgs)){
	t.i <- tt.ai.shape[,"trophicGroup"] == tgs[i]
	t.wtcpue <- tt.ai.shape[t.i,"wtcpue.sum"]
	tt.ai.shape[t.i,"wtcpue.rel"] <- tt.ai.shape[t.i,"wtcpue.sum"]/max(t.wtcpue, na.rm=TRUE)
}


# function to automatically determine the panel dimensions given a desired number of panels in a plot
auto.mfrow <- function(x, tall=FALSE){
	stopifnot(x>0)
	
	if(x==1L){return(c(1,1))}
	
	ran <- 2:max(floor((x-1)/2),1)
	ran2 <- pmax(ceiling(x/(ran)),1)
	rem <- abs(x - ran2*ran)
	
	score <- abs(sqrt(x)-(ran)) + abs(sqrt(x)-(ran2)) + rem
	mf1 <- ran[which.min(score)]
	mf2 <- ran2[which.min(score)]
	
	if(!tall){
		return(sort(c(mf1, mf2)))
	}else{
		return(sort(c(mf1, mf2), TRUE))
	}	
}



par(mfrow=auto.mfrow(length(yrs)))
for(i in 1:length(yrs)){
	yr.i <- tt.ai.shape[,"year"] == yrs[i]
	t.dat <- tt.ai.shape[yr.i,]
	
	barplot(t.dat[,"wtcpue.rel"], horiz=TRUE, xlim=c(-1,1))
	barplot(-t.dat[,"wtcpue.rel"], horiz=TRUE, xlim=c(-1,1), add=TRUE, names.arg=tgs, las=1)
}


