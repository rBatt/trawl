
# NOTE: using the data.table code took 0.790 seconds, whereas the original code took 5.567 seconds

# ================
# = Read in Data =
# ================
# goaStrata.raw <- read.table("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/goaStrata.csv", header=TRUE, sep=",")
# 
# goa.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/")
# goa.files <- goa.files[!goa.files%in%"goaStrata.csv"]
# n.goa <- length(goa.files)
# 
# for(i in 1:n.goa){ # loop through data files and combine them. Assumes that column headers match
# 	t.goa.file <- goa.files[i]
# 	t.goa.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/", t.goa.file, sep="")
# 	if(i==1){
# 		goa.raw <- read.table(t.goa.name, header=TRUE, sep=",", quote="")
# 	}else{
# 		goa.raw <- rbind(goa.raw, read.table(t.goa.name, header=TRUE, sep=",", quote=""))
# 	}
# 	
# }



goaStrata.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/goaStrata.csv", header=TRUE, sep=",")

goa.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/")
goa.files <- goa.files[!goa.files%in%"goaStrata.csv"]
n.goa <- length(goa.files)

for(i in 1:n.goa){ # loop through data files and combine them. Assumes that column headers match
	t.goa.file <- goa.files[i]
	t.goa.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_GOA/2013-10-17/", t.goa.file, sep="")
	if(i==1){
		# goa.raw <- fread(t.goa.name, header=TRUE, sep=",", quote="")
		goa.raw <- fread(t.goa.name)
		setnames(goa.raw, names(goa.raw), gsub("^\\s* | \\s*$", "", names(goa.raw)))
	}else{
		t.goa.raw <- fread(t.goa.name)
		setnames(t.goa.raw, names(t.goa.raw), gsub("^\\s* | \\s*$", "", names(t.goa.raw)))
		goa.raw <- rbind(goa.raw, t.goa.raw)
	}
	
}


rmWhite(goa.raw) # remove whitespace in the elements of each column
rm9s(goa.raw) # check each column for 9999, and replace with NA



