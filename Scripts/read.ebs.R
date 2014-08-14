

# ====================
# = Read in Raw Data =
# ====================
ebsStrata.raw <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/raw_data/AFSC_EBS/2013-10-17/ebsStrata.csv") # ntoe that the last row is basically blank, except the 2nd column contains NA and the 3rd column contains the sum area (sum of 3rd column)

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
# 
# # system.time({ # time was 6.875
# # for(i in 1:ncol(ebs.raw)){
# # 	set(ebs.raw, i=NULL, j=i, value=gsub("^\\s* | \\s*$", "", ebs.raw[[i]]))
# # }	
# # })
# 
# 
# # system.time({ # time was 6.775
# # Remove trailing and leading whitespaces in the elements of each column
# for(i in 1:ncol(ebs.raw)){
# 	t.name <- names(ebs.raw)[i]
# 	expr <- bquote(.(as.name(t.name)):=gsub("^\\s* | \\s*$", "", .(as.name(t.name))))
# 	ebs.raw[,eval(expr)]
# }	
# # })
# 
# # change -9999's to NA
# for(i in seq_along(ebs.raw)){
# 	set(ebs.raw, i=which(ebs.raw[[i]]==-9999L), j=i, value=as.character(NA))
# 	set(ebs.raw, i=which(ebs.raw[[i]]==-9999.0), j=i, value=as.character(NA))
# }
# 

