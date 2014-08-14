
# ================
# = Read in Data =
# ================
goaStrata.raw <- read.table("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/goaStrata.csv", header=TRUE, sep=",")

goa.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/")
goa.files <- goa.files[!goa.files%in%"goaStrata.csv"]
n.goa <- length(goa.files)

for(i in 1:n.goa){ # loop through data files and combine them. Assumes that column headers match
	t.goa.file <- goa.files[i]
	t.goa.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/", t.goa.file, sep="")
	if(i==1){
		goa.raw <- read.table(t.goa.name, header=TRUE, sep=",", quote="")
	}else{
		goa.raw <- rbind(goa.raw, read.table(t.goa.name, header=TRUE, sep=",", quote=""))
	}
	
}
