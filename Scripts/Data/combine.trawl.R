

library(data.table)
library(rfishbase)
library(plyr)
library(taxize)

# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")

# =========================
# = Memory-saving options =
# =========================
delOldTrawl <- c(FALSE, TRUE)[1]

# =======================
# = Load data functions =
# =======================
data.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions"
invisible(sapply(paste(data.location, list.files(data.location), sep="/"), source, .GlobalEnv))


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

# ==================
# = Save Memory #1 =
# ==================
if(delOldTrawl){
	rm(list="trawl000")
}


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
trawl00[,raw.spp:=spp]
trawl00[,spp:=cullParen(cullSp(fixCase(cullExSpace(raw.spp))))]
setkey(trawl00, spp, year, s.reg)


# ================================================
# = Save a file containing key back to raw names =
# ================================================
# for use with matching to malin's spptaxonomy_2014-09-19.csv
raw.and.spp <- trawl00[,list(raw.spp, spp)]
setkey(raw.and.spp, raw.spp, spp)
raw.and.spp <- unique(raw.and.spp)
save(raw.and.spp, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/raw.and.spp.RData")

trawl00[,isSpecies:=is.species(spp)] # infer whether the taxa are identified to species or to genus (1 or 2 words)

# =============================
# = Watch out for duplicates? =
# =============================
# setkey(trawl00, s.reg, year, spp, stratum, datetime)
# sum(duplicated(trawl00))
#
# setkey(trawl00, s.reg, year, spp, stratum, haulid, datetime)
# sum(duplicated(trawl00))
#
# setkey(trawl00, s.reg, year, spp, stratum, haulid, datetime, raw.spp)
# sum(duplicated(trawl00))

# need the "duplicates" for matching to malin's taxa, i guess


# ================================
# = Use Taxize to clean up names =
# ================================
# eol.key <- "f0822ff32cb0af5fda7e4c9e02c66e47e7848e74"
# getkey("f0822ff32cb0af5fda7e4c9e02c66e47e7848e74", service="eol")

uspp <- trawl00[,unique(spp)]

tax.files <- dir("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy")


# =============================
# = Get Correct Species Names =
# =============================
if("spp.corr1.RData"%in%tax.files){
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")
	newlyChecked.spp <- getSpp(uspp=uspp, oldSpp=spp.corr1)
	spp.corr1 <- newlyChecked.spp
}else{
	newlyChecked.spp <- getSpp(uspp=uspp)
	spp.corr1 <- newlyChecked.spp
}

save(spp.corr1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.corr1.RData")


# ==============================
# = Get "Correct" Common Names =
# ==============================
if("spp.cmmn1.RData"%in%tax.files){
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")
	newlyChecked.cmmn <- getCmmn(u.sppCorr=spp.corr1[,sppCorr], oldCmmn=spp.cmmn1)
	spp.cmmn1 <- newlyChecked.cmmn
}else{
	newlyChecked.cmmn <- getCmmn(uspp=spp.corr1[,sppCorr])
	spp.cmmn1 <- newlyChecked.cmmn
}

save(spp.cmmn1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spp.cmmn1.RData")


# ===========================================
# = Merge sppCorr and common names together =
# ===========================================
setkey(spp.cmmn1, sppCorr)
setkey(spp.corr1, sppCorr)
trawl.newSpp <- spp.corr1[unique(spp.cmmn1)]
# trawl.newSpp[,sum(is.na(sppCorr)&!is.na(spp))] # I get 0 ... not anymore, now I get 24
trawl.newSpp[!grepl("[a-zA-Z]", common)|common=="", common:=as.character(NA)] # remove any common names that don't contain english chars

# check for duplicates (arising because of different original "spp" value, but resolved to be same sppCorr & common values)
setkey(trawl.newSpp, sppCorr, common)
trawl.newSpp <- unique(trawl.newSpp)


# ================================
# = Get Taxonomic Classification =
# ================================

if("taxLvl1.RData"%in%tax.files){
	load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")
	
	taxLvl1 <- getTax(sppCorr2=trawl.newSpp[,sppCorr], oldTax=taxLvl1)

	setkey(taxLvl1, sppCorr)
	taxLvl1 <- unique(taxLvl1)
}else{
	taxLvl1 <- getTax(sppCorr2=trawl.newSpp[,sppCorr])
	
	setkey(taxLvl1, sppCorr)
	taxLvl1 <- unique(taxLvl1)
}

save(taxLvl1, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxLvl1.RData")


# ====================
# = Add Manual Names =
# ====================
manualTax <- fread("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/spptaxonomy_2014-10-09_plusManual_no_spp._RDB.csv")
rmWhite(manualTax) # have to remove leading and trailing white space

setnames(manualTax, c("taxon", "name"), c("spp", "sppCorr"))

manualTax[,spp:=cullParen(cullSp(fixCase(cullExSpace(spp))))]

setkey(manualTax, spp)
manualTax <- unique(manualTax)


tns.in.man <- trawl.newSpp[,spp]%in%manualTax[,spp] # find the trawl names in the manual file
# man.in.tns <- manualTax[,spp]%in%trawl.newSpp[,spp] # find the manual names in the trawl object
trawl.newSpp2 <- rbind(trawl.newSpp[!tns.in.man], manualTax[, list(sppCorr, spp, common)]) # take all of the trawl object names not found in the manual file, then bind it to all of the manual names that were found in the trawl object



# ======================================
# = Update species names in trawl data =
# ======================================
setkey(trawl.newSpp2, spp)
setkey(trawl00, spp)
trawl0 <- merge(trawl00, trawl.newSpp2, all.x=TRUE, by="spp") #trawl[trawl.newSpp]


# ==================
# = Save Memory #2 =
# ==================
if(delOldTrawl){
	rm(list="trawl00")
}

trawl0[!is.na(sppCorr),spp:=sppCorr]
trawl0[,correctSpp:=!is.na(sppCorr)]

setkey(trawl0, sppCorr)
# taxLvl1[,sum(is.na(sppCorr))] # this needs to be 0
if(taxLvl1[,sum(is.na(sppCorr))]==0){
	trawl0 <- merge(trawl0, taxLvl1, all.x=TRUE)
}else{
	print("NA's in sppCorr in taxLvl1 !!")
}



# ===========================
# = Trim trawl columns down =
# ===========================
trawl4 <- trawl0[,list(region, s.reg, phylum, spp, taxLvl, common, year, datetime, haulid, stratum, lat, lon, depth, stemp, btemp, wtcpue, cntcpue, correctSpp)] # this is where I drop all of the other pieces of taxonomic information
setkey(trawl4, s.reg, taxLvl, phylum, spp, year, stratum)

# ==================
# = Save Memory #3 =
# ==================
if(delOldTrawl){
	rm(list="trawl0")
}

trawl4[,depth:=as.numeric(depth)]


# ====================
# = Fix date formats =
# ====================
# regular expression patterns
pat2y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{2})(?=\\s|$)" # for dates like 6/23/07 or 06/5/07 or 06/05/07
pat4y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{4})(?=\\s|$)" # for dates like 6/23/2007, or 06/23/2007, etc
pat4y.only <- "^(\\d{4})$" # for dates that are just the year, e.g., 2007

# test <- c("7/15/10 17:59", "08/03/1997 17:58", "09/01/1987 18:00", "07-17-1991")
# gsub(pat2y, "20\\3-\\1-\\2", test, perl=TRUE)
# gsub(pat4y, "\\3-\\1-\\2", test, perl=TRUE)
# pat.mdy <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])"

trawl4[,datetime:=gsub(pat2y, "20\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/07 for 2007-6-23

trawl4[,datetime:=gsub(pat4y, "\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/2007 for 2007-6-23

trawl4[,datetime:=gsub(pat4y.only, "\\1-01-01", datetime, perl=TRUE)] # e.g., switch out 2007 for 2007-01-01

# trawl[,datetime:=as.POSIXct(datetime, tz="GMT")] # note that the times get truncated # too slow, see below for better solution

# ======================
# = Add POSIX to trawl =
# ======================
trawl.posix <- data.table(datetime=trawl4[,unique(datetime)], datetimeP=as.POSIXct(trawl4[,unique(datetime)], tz="GMT"))
setkey(trawl.posix, datetime)
setkey(trawl4, datetime)
trawl3 <- merge(trawl4, trawl.posix, all.x=TRUE)
trawl3[,datetime:=datetimeP]
trawl3 <- trawl3[,j=names(trawl3)[names(trawl3)!="datetimeP"], with=FALSE]


# =================================================
# = Add Replicates (new version of haulid, kinda) =
# =================================================
# trawl3[,groupID:=as.character(datetime)]
# trawl3[s.reg=="neus",groupID:=as.character(haulid)]
# dev.new(); par(mfrow=c(4,3))
# trawl3[,lu(paste0(roundGrid(lat,1/3),roundGrid(lon,1/3))),by=c("s.reg","year","stratum")][,plot(table(V1), main=as.character(s.reg)), by="s.reg"]
#
# trawl3[,lu(paste0(roundGrid(lat,1/3),roundGrid(lon,1/3))),by=c("s.reg","year","stratum")][,sum(table(V1)), by="s.reg"] # the number of rep–year–strat combinations per region ... yikes.

trawl3[,haulid:=paste0(round(roundGrid(lat,1/3),3),round(roundGrid(lon,1/3),3)), by=c("s.reg","year","stratum")]
trawl3[,K:=as.integer(as.factor(haulid)), by=c("s.reg","year","stratum")]
trawl3[,nK:=max(K), by=c("s.reg","year","stratum")]

# ==================
# = Save Memory #4 =
# ==================
if(delOldTrawl){
	rm(list="trawl4")
}


# ======================================================================
# = In the past, my save object was basically the equivalent of trawl3 =
# ======================================================================

# ================================
# = Aggregate and Pad Trawl Data =
# ================================
# Make sure each species in each stratum (by=s.reg) has a row
# Then aggregate by averaging wtcpue

# First, need to do a bit of aggregating to make sure that duplicates weren't creating when finding the correct species names
# Ha, they were definitely created
# I think the problem (or part of it, at least), may be related to the way in which I build upon the common, tax lvl, and spp name files; in particular, I think that because I've begun to institutte new restrictions on what constitutes a valid common name, that now there are both "corrSpp" TRUE and FALSE for the same original raw species name. The particular case of this that I'm thinking of while writing this is where I strip out any name matches (for common) with foriegn characters. Not entirely sure where this lack of match ends up converting into a corrSpp being F, though
# Anyway, I need to do some condensing here.
# ==================================
# = Prepare Padding by Aggregating =
# ==================================
trawl2 <- trawl3[,
	{
		# just some checks to make sure that weird things don't happen
		if(length(unique(common[correctSpp]))>1){
			print(unique(common[correctSpp]))
			stop("trying to add too many common names – apparent correct match of species name to multiple commons")
		}
		if(length(unique(taxLvl[correctSpp]))>1){
			print(unique(taxLvl[correctSpp]))
			stop("trying to add too many taxLvl – apparent correct match of species name to multiple taxonomic classifications")
		}
		
		# OK, create condensed output list
		list(
			# datetime=as.POSIXct(mean(datetime, na.rm=TRUE), tz="GMT", origin="1970-01-01 00:00.00 GMT"),
			# lat=roundGrid(mean(lat, na.rm=TRUE)),
			# lon=roundGrid(mean(lon, na.rm=TRUE)),
			lat=mean(lat, na.rm=TRUE),
			lon=mean(lon, na.rm=TRUE),
			depth=mean(depth, na.rm=TRUE), 
			stemp=meanna(stemp), 
			btemp=meanna(btemp), 
			wtcpue=meanna(wtcpue), 
			correctSpp=any(correctSpp),
			# common=.SD[correctSpp,unique(common)],
			# taxLvl=.SD[correctSpp,unique(taxLvl)]
			common=unique(common[correctSpp]),
			taxLvl=unique(taxLvl[correctSpp])
			
		) 
	},
	by=c("region", "s.reg", "year", "stratum", "K", "nK", "phylum", "spp")
	 # I wonder if it's faster to include something like "phylum" in the "by" argument, or if I should do somethign more like what I did with common names or taxLvl and just set it as a unique value in the output
] # note that sometimes wtcpue is 0 when cntcpue is non-0, hence why you can have normal numerics for depth and temp even though there is 0 cpue (which would seemingly imply a no-obs, but that's not necessarily true)
setkey(trawl2, s.reg, year, stratum, nK, K, phylum, spp)

# test <- data.table(
# 		"t1"=c(rep("chor",20), rep("other",20))
# 	)

# sum(duplicated(trawl2))


# ==================
# = Save Memory #5 =
# ==================
if(delOldTrawl){
	rm(list="trawl3")
}

# save.image("~/Desktop/quickPickUp_combineTrawl.asdf982734FAK.RData")
# =====================================================
# = Pad species absences (actual observations of 0's) =
# =====================================================
# test <- trawl2[,head(.SD), by=c("s.reg", "year")]
#
# test.m <- test[s.reg=="ai", merge.data.table(x=list(spp=unique(spp)), y=list(year=unique(year), stratum=unique(stratum)), by=NULL)]
#
# test.m2 <- test[,
# 	j={
# 		yr.strat <- .SD[,list(year=year, stratum=stratum)]
# 		setkey(yr.strat, year, stratum)
# 		yr.strat <- unique(yr.strat)
# 		yr.strat[,ysID:=1:nrow(.SD)]
#
# 		res0 <- CJ(spp=unique(spp[!is.na(spp)]), ysID=yr.strat[,ysID])
# 		res <- merge(res0, yr.strat, by="ysID")
# 		res[,ysID:=NULL]
# 		res
#
# 	},
# 	by=c("s.reg")
# ]


allSpp0 <- trawl2[, 
	j={
		yr.strat <- .SD[,list(year=year, stratum=stratum)] # TODO I think it is here that I need to add haulid or DoY so that I can retain hauls and pad
		setkey(yr.strat, year, stratum)
		yr.strat <- unique(yr.strat)
		yr.strat[,ysID:=1:nrow(.SD)]
		
		res0 <- CJ(spp=unique(spp[!is.na(spp)]), ysID=yr.strat[,ysID])
		res <- merge(res0, yr.strat, by="ysID")
		res[,ysID:=NULL]
		res
	
	},
	by=c("s.reg")
]

# Set keys before merge
setkey(trawl2, s.reg, spp, year, stratum)
setkey(allSpp0, s.reg, spp, year, stratum) 


if(sum(duplicated(allSpp0))==0){
	# trawl1.1 <- merge(allSpp0, trawl2, all=TRUE, by=c("s.reg","spp","year","stratum"))
	trawl1.1 <- trawl2[allSpp0, allow.cartesian=TRUE] # MERGE
	setkey(trawl1.1, s.reg, spp, year, stratum)
}else{
	# Hopefully it never enters this, will probably throw an error if it does
	trawl1.1 <- trawl2[allSpp0] # MERGE
	setkey(trawl1.1, s.reg, spp, year, stratum)
}

trawl1.1[,length(unique(spp)), by=c("s.reg","stratum","year")]

# Indicate that all of these rows were actually observed stratum-year combinations (and possibly obs absences of spp)
trawl1.1[,Obsd:=TRUE] # mark all rows as being "observed"
trawl1.1[is.na(wtcpue)&Obsd, wtcpue:=0] # mark observed absences (which are often not recorded, thus NA at this point) as 0's

# Strip down to names that I want to keep
trawl1.1 <- trawl1.1[,list(s.reg, spp, year, stratum, lat, lon, depth, stemp, btemp, wtcpue, Obsd)]


# Rebuild lat, lon, depth, btemp, stemp
trawl1.1[, c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg", "stratum")] 
trawl1.1[, c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]


# ============================================================
# = Pad w/ true missingness (as opposed to observed absence) =
# ============================================================
# Create the data.table that will hold the spp, s.reg, year, stratum such that for a given species in a given place, we can build a complete time series (missing data gaps to be filled in w/ NA's)
# allSpp <- trawl2[,CJ(spp=unique(spp)[!is.na(unique(spp))], year=as.character(unique(year)), stratum=unique(stratum)[!is.na(unique(stratum))]), by="s.reg"] # build combinations
allSpp <- trawl2[,
			CJ( # TODO K can only be defined once I've decided how to define a rep. I might have to pad the species wtcpue to include unique combinations of days (but not times), so that hauls on the same day are first averaged. Need to pad species wtcpue to have 0's for the different hauls (I'm assuming each haul start at least at a different time of day, even if on same day of year) b/c I obviously need to account for 0's in each haul (I need to linearly remove the effect of effort [put effort in the denominator])
				spp=unique(spp)[!is.na(unique(spp))], 
				year=as.character(unique(year)), 
				stratum=unique(stratum)[!is.na(unique(stratum))],
				K=unique(K)[!is.na(unique(K))] # K is a rep, or haul; not calling it haul so it doesn't get confused with original haulid (this is necessary b/c I redefine statum, so it's possible for two hauls to actually be reps when they were originally in different strata)
			 ), 
			 by="s.reg"] # build combinations


# Set keys before merge
setkey(trawl2, s.reg, spp, year, stratum, K)
setkey(allSpp, s.reg, spp, year, stratum, K) 



# ===========================================
# = Merge full time series skeleton w/ data =
# ===========================================
# A lot of confusion due to data.table asserting I was attempting an unintentional merge
# See bug: https://github.com/Rdatatable/data.table/issues/742
# I had 1.9.2 when I first had this bug, then I updated to 1.9.4 (had to install from source b/c still on Mountain Lion on laptop), but still had bug
# solution was to just check for duplicated i
if(sum(duplicated(allSpp))==0){
	# trawl1 <- merge(allSpp, trawl2, all=TRUE, by=c("s.reg","spp","year","stratum"))
	trawl1.2 <- trawl1.1[allSpp, allow.cartesian=TRUE] # MERGE
	setkey(trawl1.2, s.reg, spp, year, stratum, K)
}else{
	# Hopefully it never enters this, will probably throw an error if it does
	trawl1.2 <- trawl1.1[allSpp] # MERGE
	setkey(trawl1, s.reg, spp, year, stratum, K)
}

# Finish indicating which rows were actually "absence" data vs. "no observation" data
trawl1.2[is.na(Obsd), Obsd:=FALSE]

# Strip down to names that I want to keep
trawl1 <- trawl1.2[,list(s.reg, spp, year, stratum, K, lat, lon, depth, stemp, btemp, wtcpue, Obsd)]


# ==================================================
# = Rebuild Taxonomic Classification after Padding =
# ==================================================
# Have to rebuild some of the taxonomic classifications after merging
trawl2.tax <- trawl2[,list(s.reg, spp, taxLvl, common, correctSpp)] # get classifications
setkey(trawl2.tax) # set key in preparation for merge
trawl2.tax <- unique(trawl2.tax)

# Need to make sure we're only using the classifications for the conditions when correctSpp was true
# This is a necessary step – otherwise, there WILL be duplicates
# This created a huge headache for me originally, before I figure out what was going on (still don't fully understand why all of those duplicates exist, although I do have ideas)
trawl2.tax2 <- trawl2.tax[,
	{
		list(
			s.reg=s.reg[correctSpp],
			spp=spp[correctSpp],
			taxLvl=taxLvl[correctSpp],
			common=common[correctSpp],
			correctSpp=correctSpp[correctSpp]
			)
	}
]
setkey(trawl2.tax2) # set key in preparation for merge

trawlK <- merge(trawl1, trawl2.tax2, by=c("s.reg","spp")) # merge trawl data with rebuilt taxonomic classification
setkey(trawlK, s.reg, spp, stratum, year, K) # set key


# Need to rebuild numeric variables (but not CPUE data) after filling in time series w/ NA's
# This has already been done in cases where Obsd is TRUE (done in trawl1.1)
# Subsetting to !(Obsd) avoid redundancy w/ what was already done in trawl1.1 – should speed up slightly :)
trawlK[!(Obsd), c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg", "stratum")]
trawlK[!(Obsd), c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year", "K")]


# Then aggregate across hauls (to be back compat) and generate "trawl"



# ========
# = Save =
# ========
setkey(trawlK, s.reg, spp, year, stratum, K)
save(trawlK, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawlK.RData")
