

# ====================
# = Read in Raw Data =
# ====================
ebsStrata.raw <- read.table("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/ebsStrata.csv", header=TRUE, sep=",")

ebs.files <- list.files("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/")
ebs.files <- ebs.files[!ebs.files%in%"ebsStrata.csv"]
n.ebs <- length(ebs.files)

for(i in 1:n.ebs){ # loop through data files and combine them. Assumes that column headers match
	t.ebs.file <- ebs.files[i]
	t.ebs.name <- paste("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/", t.ebs.file, sep="")
	if(i==1){
		ebs.raw <- read.table(t.ebs.name, header=TRUE, sep=",", quote="")
	}else{
		ebs.raw <- rbind(ebs.raw, read.table(t.ebs.name, header=TRUE, sep=",", quote=""))
	}
	
}
