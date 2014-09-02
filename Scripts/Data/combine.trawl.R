

library(data.table)
library(rfishbase)
library(plyr)
library(taxize)

source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")


# ============================================
# = Identify individual regions to be loaded =
# ============================================
cleanDirectory <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions"
cleanedRegions <- list.files(cleanDirectory)
cleanNames.reg <- regexpr("[a-z]+[0-9]?(?=\\.RData$)", cleanedRegions, perl=TRUE)
cleanNames <- regmatches(cleanedRegions, cleanNames.reg)

# ==========================================
# = Load regions locally, combine globally =
# ==========================================
local({
	for(i in 1:length(cleanedRegions)){
		load(paste(cleanDirectory, cleanedRegions[i], sep="/")) # load each .RData file
	}
	trawl000 <<- rbindlist(mget(cleanNames)) # assign to global, or if found in a parent, reassign in that environment
	# print(ls()) # note that ls() default is to current frame, so only see locally-defined variables with ls()
	rm(list=ls()) # thus removing ls() preserves trawl and all variables defined outside of call to local()
})

# ============================
# = Convert spp to character =
# ============================
trawl000[,spp:=as.character(spp)]



# ======================
# = Remove bad species =
# ======================
uspp <- unique(trawl000[,spp])
badEgg <- uspp[grepl("[eE][gG]{2}", uspp)]
badFish <- uspp[grepl("(?<![a-z])fish(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
badLarv <- uspp[grepl("(?<![a-z])larv(a[e])?(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
# badJuv <- uspp[grepl("(?<![a-z])juven(ile)?(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
badYoy <- uspp[grepl("(?<![a-z])yoy(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]

badSpp <- unique(c(badEgg, badFish, badLarv, badYoy))

setkey(trawl000, spp)
trawl00 <- trawl000[!.(badSpp),]

# ========================
# = Note redundant names =
# ========================
# "ACANTHOCARPUS ALEXANDRI"
# # and
# "ACATHOCARPUS ALEXADRI" 

# trawl["ACATHOCARPUS ALEXADRI", spp:="ACANTHOCARPUS ALEXANDRI"] # fix a redundant species name due to typo


# ====================================
# = Functions for cleaning spp names =
# ====================================


fixCase <- function(x){
	s <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep="")
	paste(substring(s, 1, 1), tolower(substring(s, 2)), sep="")
}

cullExSpace <- function(x){
	gsub("\\s+", " ", x)
}

cullSp <- function(x){
	# gsub("\\ss[p]{1,2}\\..*", "", x)
	gsub("\\s(s[p]{1,2}|unid)\\..*", "", x)
}

cullParen <- function(x){
	gsub("\\s?\\(.*\\)", "", x)
}

is.species <- function(x){
	sapply(strsplit(x, " "), length) >= 2
}

# =============================
# = Clean and reaggregate spp =
# =============================
trawl00[,spp:=cullParen(cullSp(fixCase(cullExSpace(spp))))]
setkey(trawl00, spp, year, s.reg)

trawl00[,isSpecies:=is.species(spp)] # infer whether the taxa are identified to species or to genus (1 or 2 words)

# trawl2 <- trawl
# trawl2[,c("wtcpue", "cntcpue","depth","btemp","stemp"):=list(wtcpue=sumna(wtcpue), cntcpue=sumna(cntcpue), depth=meanna(depth), btemp=meanna(btemp), stemp=meanna(stemp)), by=c("year","datetime","spp","haulid","stratum","stratumarea","lat","lon","region","s.reg")]
# dim(trawl) # 997913     15
# dim(trawl2) # 997913     15 ... this means that species weren't ID'd in multiple ways during the same haul.


# ================================
# = Use Taxize to clean up names =
# ================================
# eol.key <- "f0822ff32cb0af5fda7e4c9e02c66e47e7848e74"
# getkey("f0822ff32cb0af5fda7e4c9e02c66e47e7848e74", service="eol")

uspp <- trawl00[,unique(spp)]


countN <- function(x){ # count the number of times the letter "n" appears
	sapply(strsplit(x,""), FUN=function(x)length(grep("n",x)))
}

grb.spp1 <- function(x) {
	tryCatch(
		{
			x <- x$results
			x <- x[!duplicated(x[,"matched_name2"]),]
			adjN <- pmax(countN(x$matched_name2) - countN(x$submitted_name), 0)*0.01 # gets bonus match score if the matched name has more n's, because n's appear to be missing a lot
			x$score <- x$score + adjN
			x <- x[max(which.max(x[,"score"]),1),c("submitted_name","matched_name2")]
			if(x[,"matched_name2"]==""){x[,"matched_name2"] <- NA}
			return(x)
		}, 
		error=function(cond){
			tryCatch(
				{
					data.frame(submitted_name=x$results[1, "submitted_name"], matched_name2=as.character(NA))
				},
				error=function(cond){data.frame(submitted_name=NA, matched_name2=NA)}
			)
		}	
	)
}

tax.files <- dir("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy")


# ==============================
# = Grab correct species names =
# ==============================
	# =========================================================
	# = If spp.corr1.RData does not exist, start from scratch =
	# =========================================================
if(!"spp.corr1.RData"%in%tax.files){
	print("Can't find data file, looking up all spp names")
	flush.console()
	# cut(seq_along(uspp), length(uspp)/(2:20)[which.min(length(uspp)%%(2:20))])
	# n.cuts <- length(uspp)/(2:20)[which.min(length(uspp)%%(2:20))] # finds the smallest integer between 2 and 20 of which length(uspp) is a multiple (modulus %% == 0)
	uspp.chunks <- as.character(cut(seq_along(uspp), length(uspp)))
	u.uspp.chunks <- unique(uspp.chunks)
	# start.time <- proc.time()["elapsed"]/60/60
	spp.pb <- txtProgressBar(min=1, max=length(u.new.spp.chunks), style=3)
	for(s in seq_along(u.uspp.chunks)){
		
		t.chunk <- u.uspp.chunks[s]
		t.uspp <- uspp[uspp.chunks==t.chunk]
		t.spp.corr1.names <- gnr_resolve(t.uspp, stripauthority=TRUE, http="get", resolve_once=TRUE)
		t.spp.corr1 <- data.table(grb.spp1(t.spp.corr1.names))
		if(s==1){
			spp.corr1 <- t.spp.corr1
		}else{
			spp.corr1 <- rbind(spp.corr1, t.spp.corr1)	
		}
		setTxtProgressBar(spp.pb, s)
		# t.time <- proc.time()["elapsed"]/60/60
		# t.perc <- formatC(round(s/length(u.uspp.chunks)*100,2), 3)
		# t.elap <- formatC(round(t.time-start.time, 2),3)
		# print(paste("# ", formatC(s, digits=4), " has completed (", t.perc, "%); ", t.elap, " hours elapsed", sep=""))
		# flush.console()
	}
	
	# spp.corr1.names <- gnr_resolve(uspp, stripauthority=TRUE, http="post")
	# save(spp.corr1.names, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.names.RData")
	# spp.corr1 <- ddply(spp.corr1.names, "submitted_name", grb.spp1)
	# spp.corr1 <- data.table(spp.corr1)
	close(spp.pb)
	setnames(spp.corr1, c("submitted_name", "matched_name2"), c("spp", "sppCorr"))
	setkey(spp.corr1, spp)
	save(spp.corr1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
	
	# ======================================================
	# = If spp.corr1.RData exists, only search for new spp =
	# ======================================================
}else{ # in the case where the spp.corr1.RData file has already been found, ....
	print("Found data file of corrected species names")
	flush.console()
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
	new.spp0 <- !uspp%in%spp.corr1[,spp] # are there any species that we haven't searched yet?
	if(any(new.spp0)){ # if so, ...
		print(paste("Looking up spp names for ", sum(new.spp0), " new spp", sep=""))
		flush.console()
		new.spp <- uspp[new.spp0] # define the new.spp as the unique species names that haven't been searched
		
		new.spp.chunks <- as.character(cut(seq_along(new.spp), length(new.spp))) # could just use unique(), but this is here if we want to search for more than 1 new.spp at a time (i.e., several new.spp would be passed to gnr_resolve at once)
		u.new.spp.chunks <- unique(new.spp.chunks) # the unique groups of species that will be saerched
		
		# start.time <- proc.time()["elapsed"]/60/60 # set up timer
		spp.pb <- txtProgressBar(min=1, max=length(u.new.spp.chunks), style=3)
		for(s in seq_along(u.new.spp.chunks)){ # for each group of new species to search (current just each unique species)
		
			t.chunk <- u.new.spp.chunks[s] # temporary spp group name
			t.new.spp <- new.spp[new.spp.chunks==t.chunk] # temporary spp name (could be more than 1 spp, depending on how we used cut())
			t.spp.corr2.names <- gnr_resolve(t.new.spp, stripauthority=TRUE, http="get", resolve_once=TRUE) # search the species w/ taxize package
			t.spp.corr2 <- data.table(grb.spp1(t.spp.corr2.names)) # 
			if(s==1){
				spp.corr2 <- t.spp.corr2
			}else{
				spp.corr2 <- rbind(spp.corr2, t.spp.corr2)	
			}
			setTxtProgressBar(spp.pb, s)
			# t.time <- proc.time()["elapsed"]/60/60
# 			t.perc <- formatC(round(s/length(u.new.spp.chunks)*100,2), 3)
# 			t.elap <- formatC(round(t.time-start.time, 2),3)
# 			print(paste("# ", formatC(s, digits=4), " has completed (", t.perc, "%); ", t.elap, " hours elapsed", sep=""))
# 			flush.console()
			
		}
		close(spp.pb)
		setnames(spp.corr2, c("submitted_name", "matched_name2"), c("spp", "sppCorr"))
		setkey(spp.corr2, spp) # set key for spp.corr2 (should already be set for spp.corr1)
		# spp.corr1 <- spp.corr1[spp.corr2] # do a join ... maybe this should just be an rbind()
		spp.corr1 <- rbind(spp.corr1, spp.corr2)
		
		# Save the new spp.corr1 file, which has been updated with new species
		save(spp.corr1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
	}
}

# setkey(spp.corr1, spp, sppCorr)
# dim(unique(spp.corr1))
# spp.corr1 <- unique(spp.corr1)

# ===========================
# = Search for common names =
# ===========================
u.sppCorr <- spp.corr1[,sppCorr] #spp.corr1[,unique(sppCorr)]
	# ========================================================
	# = If spp.cmmn1.RData doesn't exist, start from scratch =
	# ========================================================
if(!"spp.cmmn1.RData"%in%tax.files){
	print("File of common names not found, searching for all")
	flush.console()
	cmmn.pb <- txtProgressBar(min=1, max=length(u.sppCorr), style=3)
	for(i in 1:length(u.sppCorr)){
		t.spp.cmmn00 <- tryCatch( # first try finding the common name in itis
			{
				sci2comm(u.sppCorr[i], db="itis", ask=FALSE, verbose=FALSE)[[1]]#[1] # sometimes this throws an error
			},
				error=function(cond){
					tryCatch( # if itis throws an error, look in eol
						sci2comm(u.sppCorr[i], db="eol", ask=FALSE, verbose=FALSE)[[1]],#[1], # if can't find in eol or error...
						error=function(cond){NA} # ... return NA
						)
				}
		)
		t.spp.cmmn0 <- t.spp.cmmn00[grepl("[a-zA-Z]", t.spp.cmmn00)][1] # only match common names with english chars
		
		t.spp.cmmn1 <- data.table(sppCorr=u.sppCorr[i], common=t.spp.cmmn0) # turn the common match into a data table w/ sppCorr
		if(i==1){
			spp.cmmn1 <- t.spp.cmmn1 # create the spp.cmmn1 data.table
		}else{
			spp.cmmn1 <- rbind(spp.cmmn1, t.spp.cmmn1) # or accumulate the spp.cmmn1 entries
		}
		setTxtProgressBar(cmmn.pb, i)
	}
	close(cmmn.pb)
	setkey(spp.cmmn1, sppCorr)
	save(spp.cmmn1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")
	
	# =============================================================
	# = If spp.cmmn1.RData does exist, only search for new common =
	# =============================================================
}else{
	print("File of common names found")
	flush.console()
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")
	setkey(spp.cmmn1, sppCorr)
	new.sppCorr0 <- !u.sppCorr%in%spp.cmmn1[,sppCorr] & !is.na(u.sppCorr)
	if(any(new.sppCorr0 )){
		print(paste("Looking up common names for ", sum(new.sppCorr0), " new spp", sep=""))
		flush.console()
		new.sppCorr <- u.sppCorr[new.sppCorr0]
		cmmn.pb <- txtProgressBar(min=1, max=length(new.sppCorr), style=3) # initialize the progress bar
		for(i in 1:length(new.sppCorr)){
			t.spp.cmmn00 <- tryCatch(
				{
					sci2comm(new.sppCorr[i], db="itis", ask=FALSE, verbose=FALSE)[[1]]#[1]
				},
					error=function(cond){
						tryCatch(
							sci2comm(new.sppCorr[i], db="eol", ask=FALSE, verbose=FALSE)[[1]],#[1], 
							error=function(cond){NA}
							)
					}
			)
			t.spp.cmmn0 <- t.spp.cmmn00[grepl("[a-zA-Z]", t.spp.cmmn00)][1] # only match common names with english chars
			t.spp.cmmn2 <- data.table(sppCorr=new.sppCorr[i], common=t.spp.cmmn0)
			if(i==1){
				spp.cmmn2 <- t.spp.cmmn2
			}else{
				spp.cmmn2 <- rbind(spp.cmmn2, t.spp.cmmn2)
			}
			
			setTxtProgressBar(cmmn.pb, i) # update progress bar
		}
		close(cmmn.pb) # close progress bar
		setkey(spp.cmmn2, sppCorr) # set key for new common names
		spp.cmmn1 <- rbind(spp.cmmn1, spp.cmmn2) # bind new and old common names
		save(spp.cmmn1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")
	}
}


# ===========================================
# = Merge sppCorr and common names together =
# ===========================================
setkey(spp.cmmn1, sppCorr)
setkey(spp.corr1, sppCorr)
trawl.newSpp <- spp.corr1[unique(spp.cmmn1)]
# trawl.newSpp[,sum(is.na(sppCorr)&!is.na(spp))] # I get 0
trawl.newSpp[!grepl("[a-zA-Z]", common)|common=="", common:=as.character(NA)] # remove any common names that don't contain english chars

# check for duplicates (arising because of different original "spp" value, but resolved to be same sppCorr & common values)
setkey(trawl.newSpp, sppCorr, common)
# sum(duplicated(trawl.newSpp))
trawl.newSpp <- unique(trawl.newSpp)

# =============================
# = Determine taxonomic level =
# =============================

# t.taxLvl0 <- tryCatch( # first try finding the common name in itis
# 	{
# 		tail(classification("Abisa", db="itis", verbose=FALSE)[[1]][,2], 1)
# 	},
# 		error=function(cond){as.character(NA)}
# )
# "\\n"
# tail(classification("Cetacea", db="itis")[[1]][,2], 1)
# classification("Abisa", db="itis")
sppCorr2 <- trawl.newSpp[,sppCorr]
# formals(get_tsn)$ask <- FALSE
if(!"taxLvl.RData"%in%tax.files){
	print("File of taxonomic levels not found, searching for all")
	flush.console()
	tlvl.pb <- txtProgressBar(min=1, max=length(sppCorr2), style=3)
	for(i in 1:length(sppCorr2)){
		t.taxLvl0 <- tryCatch( # first try finding the common name in itis
			{
				tail(classification(get_tsn(sppCorr2[i], ask=FALSE, verbose=FALSE), verbose=FALSE)[[1]][,2], 1)
			},
				error=function(cond){as.character(NA)}
		)
		# t.taxLvl0 <- t.taxLvl00[grepl("[a-zA-Z]", t.taxLvl00)][1] # only match common names with english chars
		
		t.taxLvl1 <- data.table(sppCorr=sppCorr2[i], taxLvl=t.taxLvl0) # turn the common match into a data table w/ sppCorr
		if(i==1){
			taxLvl1 <- t.taxLvl1 # create the spp.cmmn1 data.table
		}else{
			taxLvl1 <- rbind(taxLvl1, t.taxLvl1) # or accumulate the spp.cmmn1 entries
		}
		setTxtProgressBar(tlvl.pb, i)
	}
	close(tlvl.pb)
	setkey(taxLvl1, sppCorr)
	save(taxLvl1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")
	
	# ==========================================================
	# = If taxLvl.RData does exist, only search for new common =
	# ==========================================================
}else{
	print("File of common names found")
	flush.console()
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")
	setkey(taxLvl1, sppCorr)
	new.sppCorr0 <- !u.sppCorr%in%taxLvl1[,sppCorr] & !is.na(u.sppCorr)
	if(any(new.sppCorr0 )){
		print(paste("Looking up common names for ", sum(new.sppCorr0), " new spp", sep=""))
		flush.console()
		new.sppCorr <- u.sppCorr[new.sppCorr0]
		tlvl.pb <- txtProgressBar(min=1, max=length(new.sppCorr), style=3) # initialize the progress bar
		for(i in 1:length(new.sppCorr)){
			t.taxLvl0 <- tryCatch( # first try finding the common name in itis
				{
					# tail(classification(sppCorr2[i], db="itis", verbose=FALSE)[[1]][,2], 1)
					tail(classification(get_tsn(sppCorr2[i], ask=FALSE, verbose=FALSE), verbose=FALSE)[[1]][,2], 1)
				},
					error=function(cond){as.character(NA)}
			)
			t.taxLvl2 <- data.table(sppCorr=new.sppCorr[i], taxLevel=t.taxLvl0)
			if(i==1){
				taxLvl2 <- t.taxLvl2
			}else{
				taxLvl2 <- rbind(taxLvl2, t.taxLvl2)
			}
			
			setTxtProgressBar(tlvl.pb, i) # update progress bar
		}
		close(tlvl.pb) # close progress bar
		setkey(taxLvl2, sppCorr) # set key for new common names
		taxLvl1 <- rbind(taxLvl1, taxLvl2) # bind new and old common names
		save(taxLvl1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")
	}
}


# ======================================
# = Update species names in trawl data =
# ======================================
setkey(trawl.newSpp, spp)
setkey(trawl00, spp)
trawl0 <- merge(trawl00, trawl.newSpp, all.x=TRUE, by="spp") #trawl[trawl.newSpp]

setkey(trawl0, spp)
trawl0[!is.na(sppCorr),spp:=sppCorr]
trawl0[,correctSpp:=!is.na(sppCorr)]

# ===========================
# = Trim trawl columns down =
# ===========================
trawl0 <- trawl0[,list(region, s.reg, spp, common, year, datetime, stratum, lat, lon, depth, stemp, btemp, wtcpue, cntcpue, isSpecies, correctSpp)]
setkey(trawl0, s.reg, spp, year, stratum)


# ====================
# = Fix date formats =
# ====================
# regular expression patterns
pat2y <- "^(\\d{1,2})(?:\\/)(\\d{1,2})(?:\\/)(\\d{2})(?=\\s)" # for dates like 6/23/07 or 06/5/07 or 06/05/07
pat4y <- "^(\\d{1,2})(?:\\/)(\\d{1,2})(?:\\/)(\\d{4})(?=\\s)" # for dates like 6/23/2007, or 06/23/2007, etc
pat4y.only <- "^(\\d{4})$" # for dates that are just the year, e.g., 2007

trawl0[,datetime:=gsub(pat2y, "20\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/07 for 2007-6-23

trawl0[,datetime:=gsub(pat4y, "\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/2007 for 2007-6-23

trawl0[,datetime:=gsub(pat4y.only, "\\1-01-01", datetime, perl=TRUE)] # e.g., switch out 2007 for 2007-01-01

trawl0[,datetime:=as.POSIXct(datetime, tz="GMT")] # note that the times get truncated


# ========
# = Save =
# ========
setkey(trawl, s.reg, region, year, datetime, spp, stratum, haulid)
save(trawl, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
