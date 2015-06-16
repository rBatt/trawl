
library(plyr)
library(reshape)
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

tg1.index <- tt.ai[,"trophicGroup"] == tgs[1]
plot(x=tt.ai[tg1.index,"year"], y=sqrt(tt.ai[tg1.index,"wtcpue.sum"]), type="o", ylab="mass", xlab="year", ylim=ylims)


for(i in 2:length(tgs)){
	print(i)
	t.tgi <- tt.ai[,"trophicGroup"] == tgs[i]
	lines(x=tt.ai[t.tgi,"year"], y=sqrt(tt.ai[t.tgi,"wtcpue.sum"]), type="o", col=i)	
}
legend("topleft",legend=tgs, text.col=1:length(tgs), ncol=2)

for(dog in letters){
	print(dog)
}

# second trophic group


