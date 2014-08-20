
require(bit64)
require(data.table)
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rm9s.R")

# ====================
# = Read in Raw Data =
# ====================
ebsStrata <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/ebsStrata.csv", select=c("StratumCode","Areakm2")) # ntoe that the last row is basically blank, except the 2nd column contains NA and the 3rd column contains the sum area (sum of 3rd column)
setnames(ebsStrata, "StratumCode", "STRATUM")
ebsStrata[,STRATUM:=as.character(STRATUM)]
setkey(ebsStrata, STRATUM)

ebs.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/")
ebs.files <- ebs.files[!ebs.files%in%"ebsStrata.csv"]
n.ebs <- length(ebs.files)

for(i in 1:n.ebs){ # loop through data files and combine them. Assumes that column headers match
	t.ebs.file <- ebs.files[i]
	t.ebs.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/", t.ebs.file, sep="")
	if(i==1){
		ebs.raw <- fread(t.ebs.name)
		setnames(ebs.raw, names(ebs.raw), gsub("^\\s* | \\s*$", "", names(ebs.raw)))
	}else{
		t.ebs.raw <- fread(t.ebs.name)
		setnames(t.ebs.raw, names(t.ebs.raw), gsub("^\\s* | \\s*$", "", names(t.ebs.raw)))
		ebs.raw <- rbind(ebs.raw, t.ebs.raw)
	}
}

rmWhite(ebs.raw) # remove whitespace in the elements of each column
rm9s(ebs.raw) # check each column for 9999, and replace with NA
ebs.raw[,STRATUM:=as.character(STRATUM)]
setkey(ebs.raw, STRATUM)


ebs <- merge(ebs.raw, ebsStrata, all.x=TRUE)


