


library(rfishbase)
library(taxize)
library(plyr)
library(reshape)
library(reshape2)
library(data.table)


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
# actually, i need to keep those different original spp values! otherwise I can't match back to original data set :(
setkey(trawl.newSpp, spp, sppCorr, common)
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

# check to make sure the same sppCorr doesn't match to more than 1 common name in the manual taxonomy
# checkManDupCmmn <- manualTax[,list(nCmmn=lu(common),cmmn=paste0(unique(common))),by="sppCorr"]
# checkManDupCmmn[nCmmn!=1]

# ========================================================================
# = Define intersections of information in automated and manual tax info =
# ========================================================================
trawl.newSpp[,c("defined","share.spp","share.sppCorr"):=list("auto",FALSE,FALSE)]
manualTax[,c("defined","share.spp","share.sppCorr"):=list("manu",FALSE,FALSE)]

shared.spp <- intersect(trawl.newSpp[,spp],manualTax[,spp])
trawl.newSpp[spp%in%shared.spp, share.spp:=TRUE]
manualTax[spp%in%shared.spp, share.spp:=TRUE]

shared.sppCorr <- intersect(trawl.newSpp[,sppCorr],manualTax[,sppCorr])
trawl.newSpp[sppCorr%in%shared.sppCorr, share.sppCorr:=TRUE]
manualTax[sppCorr%in%shared.sppCorr, share.sppCorr:=TRUE]

# ===============================
# = Combine Sources of tax info =
# ===============================
trawl.newSpp2 <- rbind(manualTax[,list(sppCorr, spp, common, defined, share.spp, share.sppCorr)], trawl.newSpp) # combine sources
trawl.newSpp2 <- trawl.newSpp2[!share.spp | (share.spp & defined=="manu"),] # remove auto-defs where manu-defs exist


# ======================================================================
# = Check for multiple common names per sppCorr after combine tax info =
# ======================================================================
# Check to see if need to replace auto-defined commons with manu-defined commons when share.spp==FALSE & share.sppCorr==TRUE and the two sources don't have the same common names
check2DupCmmn <- trawl.newSpp2[,list(nCmmn=lu(common),cmmn=paste0(unique(common))),by="sppCorr"]
if(check2DupCmmn[,any(nCmmn!=1)]){ # don't want to do this automatically b/c it's kinda weird, and I it's currently not necessary
	trawl.newSpp2 <- trawl.newSpp2[(share.sppCorr), common:=.SD[defined=="manu",unique(common)], by="sppCorr"]
}

# Check again, just to make sure it worked
check2DupCmmn2 <- trawl.newSpp2[,list(nCmmn=lu(common),cmmn=paste0(unique(common))),by="sppCorr"]
if(check2DupCmmn2[,any(nCmmn!=1)]){
	stop("Something terrible has happened! Well, just duplicate common names per sppCorr, and my correction for this failed ...")
}else{
	trawl.newSpp2 <- trawl.newSpp2[,list(sppCorr,spp,common)]
}


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
trawl0[,sppCorr:=NULL]

setkey(trawl0, spp)
# taxLvl1[,sum(is.na(sppCorr))] # this needs to be 0
setnames(taxLvl1, "sppCorr", "spp")
if(taxLvl1[,sum(is.na(spp))]==0){
	trawl0 <- merge(trawl0, taxLvl1, all.x=TRUE)
}else{
	print("NA's in sppCorr in taxLvl1 !!")
}


# ===========================
# = Trim trawl columns down =
# ===========================
trawl4 <- trawl0[,list(region, s.reg, phylum, spp, taxLvl, common, year, datetime, haulid, stratum, lat, lon, depth, stemp, btemp, wtcpue, cntcpue, correctSpp)] # this is where I drop all of the other pieces of taxonomic information
setkey(trawl4, s.reg, taxLvl, phylum, spp, year, stratum)
trawl4[,depth:=as.numeric(depth)]


# ==================
# = Save Memory #3 =
# ==================
if(delOldTrawl){
	rm(list="trawl0")
}



# ====================
# = Fix date formats =
# ====================
# regular expression patterns
pat2y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{2})(?=\\s|$)" # for dates like 6/23/07 or 06/5/07 or 06/05/07
pat4y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{4})(?=\\s|$)" # for dates like 6/23/2007, or 06/23/2007, etc
pat4y.only <- "^(\\d{4})$" # for dates that are just the year, e.g., 2007

trawl4[,datetime:=gsub(pat2y, "20\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/07 for 2007-6-23

trawl4[,datetime:=gsub(pat4y, "\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/2007 for 2007-6-23

trawl4[,datetime:=gsub(pat4y.only, "\\1-01-01", datetime, perl=TRUE)] # e.g., switch out 2007 for 2007-01-01


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
trawl3[,haulid:=paste0(round(roundGrid(lat,1/3),3),round(roundGrid(lon,1/3),3)), by=c("s.reg","year","stratum")]
trawl3[,K:=as.integer(as.factor(haulid)), by=c("s.reg","year","stratum")]


# ==================
# = Save Memory #4 =
# ==================
if(delOldTrawl){
	rm(list="trawl4")
}


# ==================================
# = Prepare Padding by Aggregating =
# ==================================
# this could probably be made much faster by using lapply at the end, but that's a little difficult b/c different columns require different functions
trawl2 <- trawl3[, # this aggregates among multiple hauls within the same substratum (K); 
	{
		if(length(unique(common))>1){
			print(unique(common))
			stop("trying to add too many common names – match of species name to multiple commons")
		}
		if(length(unique(taxLvl))>1){
			print(unique(taxLvl))
			stop("trying to add too many taxLvl – match of species name to multiple taxonomic classifications")
		}
		if(length(unique(phylum))>1){
			print(unique(phylum))
			stop("trying to add too many phylum – match of species name to multiple taxonomic classifications")
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
			common=unique(common),
			taxLvl=unique(taxLvl),
			phylum=unique(phylum)	
		) 
	},
	by=c("region", "s.reg", "year", "stratum", "K", "spp")
] # note that sometimes wtcpue is 0 when cntcpue is non-0, hence why you can have normal numerics for depth and temp even though there is 0 cpue (which would seemingly imply a no-obs, but that's not necessarily true)
setkey(trawl2, s.reg, year, stratum, K, phylum, spp)

# ==================
# = Save Memory #5 =
# ==================
if(delOldTrawl){
	rm(list="trawl3")
}


# ===============================================
# = Add missing species, padding w/ 0's or NA's =
# ===============================================
# ============================
# = Add missing combinations =
# ============================
# Make sure all of a region's strata are present in all years
# Make sure all of a region's species are present in each stratum in each year
# If the s.reg-stratum-year was present, but the s.reg-stratum-year-spp was not, fill in w/ 0, NA otherwise
# Aggregate among "K" replicate hauls (substrata)
trawl2[,nK:=NULL]
setkey(trawl2, s.reg, year, stratum, K, spp)
trawl <- expand.data(
	comD = trawl2,
	arr.dim = c("stratum", "year", "spp"), # should uniquely define the keyValue ONLY WHEN combined with fScope (don't accidentally include the gScope value here)
	fillID=c("spp"),
	fillValue=c(0), # values to fill with, for a fillID
	Rule=c("value"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
	keyID=NULL, #c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
	keyValue="wtcpue", # the column whose values would fill the array
	gScope="s.reg", # global scope
	fScope=list("s.reg"), #
	vScope=list(c("s.reg","stratum","year")),
	redID=list(
		c("spp"),
		c("s.reg","year","stratum","spp")
	), 
	redValue=list(
		c("correctSpp","taxLvl","phylum","common"),
		c("lat", "lon", "depth", "stemp", "btemp")
	),
	arrayOut=FALSE, aggFun=meanna, maxOut=Inf
)


# array.filled <- expand.data( # this test the aggregate functionality, data.table output, non-NA fill, 1 fillID
# 	comD = testT.sub,
# 	arr.dim = c("stratum", "year", "spp"), # should uniquely define the keyValue when combined with fScope
# 	fillID=c("spp"),
# 	fillValue=c(0), # values to fill with, for a fillID
# 	Rule=c("value"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=NULL, #c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
# 	keyValue="value", # the column whose values would fill the array
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg"), #
# 	vScope=list(c("s.reg","stratum","year")),
# 	redID=list(c("spp")), redValue=list(c("correctSpp","taxLvl","phylum","common")),
# 	arrayOut=FALSE, aggFun=meanna, maxOut=Inf
# )





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

# =========================================================================================================
# # = # ===================================================================================================
# # = # =============================================================================================
# save.image("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/ctSave.RData")
# load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Results/ctSave.RData")
# # ============================================================================================= =
# # =================================================================================================== =
# # =========================================================================================================

# =========================================
# = # ===================================
# = # =============================
# = # =======================
# = Begin new approach? =
# ======================= =
# ============================= =
# =================================== =
# =========================================

# ==================================
# = Example of how to prepare MSOM =
# ==================================
testT2 <- as.data.table(melt(trawl2, id.vars=c("s.reg","year","stratum","K","correctSpp","taxLvl","phylum","spp","common"), measure.vars=c("wtcpue","stemp","btemp","lat","lon","depth")))

testT.sub <- testT2[variable=="wtcpue" & s.reg=="gmex" & (!is.na(taxLvl)&taxLvl=="Species") & correctSpp==TRUE]
setkey(testT.sub, stratum, K, year, spp)

# create a test community matrix for an msom
testCM <- cast(
	data=testT.sub, 
	formula=stratum~K~year~spp, 
	drop=FALSE, # don't drop missing combinations!
	fill=0, # fill missing combinations with 0's; this is problematic because missing stratum-year-K combinations should result in NA's, whereas missingness due to absent species should be filled with 0's. A solution might be to first recast part of it before creating the huge array. Another option might be to create the array in this manner, then if all elements across the relevant dimensions are 0, then make it an NA.
)




testT.sub2 <- testT.sub#[sample(1:nrow(testT.sub), 1E1)]
setkey(testT.sub2, spp, stratum, year)



# oc[,eval(parse(text="one"))]



# fill$ID[[1]] <- "spp"
# fill$Value[[1]] <- 0
# fill$By[[1]] <- c("s.reg","stratum","year")

# add in extra species to the existing year-stratum combinations, filling with 0's
# make X the desired setup of keyID entries, then fill in with i as the original data set
testT.sub3 <- testT.sub2[,list(value=meanna(value)), by=key(testT.sub2)] # aggregate step: used when not all of the values in keyID are part of 

testT.sub3[,uid1:=.GRP, by=c("stratum","year")]
setkey(testT.sub3, uid1)
# id1.dt <- testT.sub3[,{idset<-CJ(unique(uid1), spp=unique(spp));cbind(.SD[idset[,V1],list(stratum,year)], idset[,.(spp)])}]
id1.dt <- testT.sub3[,{idset<-CJ(uid1=unique(uid1), spp=unique(spp)); setkey(idset,uid1); idset[unique(.SD[,.(uid1, stratum, year)])]}]
setkeyv(id1.dt, cols=c("spp", "stratum", "year"))
setkeyv(testT.sub3, cols=c("spp","stratum","year"))
fill.gaps <- testT.sub3[id1.dt, which=TRUE, allow.cartesian=TRUE]
id1.dt[,c("value"):=testT.sub3[fill.gaps,.(value)]]
id1.dt[is.na(fill.gaps),c("value"):=0]

# Always the final step
id1.dt[,uid1:=NULL]
setkey(id1.dt, stratum, year, spp)
expD <- id1.dt[do.call(CJ, lapply(eval(s2c(arr.dim)), unique))]
setkey(expD, year, stratum, spp)

# expD[sample(1:1E5, 1E2)] # just check to see what random parts of it look like









testT.sub3[CJ(spp=unique(spp), stratum=stratum), which=NA, allow.cartesian=TRUE]

J(CJ(c(1,1,1,2),c(1,2,3)), CJ(c(5,5,6,6),c(1,2,3)))


# then add in any missing year-stratum-species combinations, filling with NA's




testT.sub2[CJ(spp=unique(spp))]
adsf <- testT.sub2[CJ(spp=unique(spp), stratum=unique(stratum), year=unique(year)), allow.cartesian=TRUE]

testCM.dt2a <- 

first <- data.table(cray=sample(letters,4),"one"=c(1,2,3,4), "two"=c(1,3,5,7))
second <- data.table("cray"=sample(letters,35, TRUE))
oc <- CJ(one=first[,(one)], cray=second[[1]])


setkey(first, one, cray)
setkey(second, cray)

first[second, roll=TRUE, rollends=TRUE]


first[oc, which=NA]

tc <- CJ(two=first[,(two)], cray=second[[1]], key="cray")



# testCM[1:3, , 1:2, 1:2]


# If all species are 0, then replace w/ NA
testCM2 <- testCM 
testCM2.0s <- apply(testCM2, c(1,2,3), sum)
dimnames(testCM2.0s) <- NULL
CM.0toNA.ind <- which(testCM2.0s==0, arr.ind=TRUE)

CM.0toNA.ind2 <- matrix(
	c(
		rep(c(CM.0toNA.ind), each=dim(testCM2)[4]), # simply the array indices from the which() line
		rep(1:dim(testCM2)[4], nrow(CM.0toNA.ind)) # adding in a 4th column
	), 
	ncol=4, 
	dimnames=list(NULL,c("dim1","dim2","dim3","dim4"))
)
testCM2[CM.0toNA.ind2] <- NA


test.a.m <- melt(testCM2, varnames=c("stratum","K","year","spp"), value.name="wtcpue")


# testCM2[1:3, , 1:2, 1:2]
# apply(testCM2, c(1,2,3), sum)

# ================================================
# = Create trawl for diverseData using cast melt =
# ================================================

# First, extract the tax info
testTax <- testT2[,list(s.reg,taxLvl,phylum,spp,common)] # save this information for a merge later – currently just inflates factor combinations
setkey(testTax, s.reg, spp)
testTax <- unique(testTax)

# Second, average across K, and cast out environmental/ geographical information (so that it can be removed, like tax info)
# "DD" refers to diverse data
testDD <- dcast.data.table(testT2, s.reg+year+stratum+spp~variable, meanna) # averages across K

# Third, extract the environmental/geographical information (later to be covariates)
testCov <- testDD[,list(s.reg, year, stratum, btemp, lat, lon, depth)]
setkey(testCov, s.reg, year, stratum)
testCov <- unique(testCov)

# Fourth, include all combinations of year-stratum-spp within a region, and only cast wtcpue on RHS so Covariates are dropped
# By including all combinations, missing values will be filled with 0. In a laster step, if all species have 0 for a Y-S-S combo, then change the wtcpue column to NA
testDD2 <- dcast.data.table(testDD, s.reg+year+stratum~spp, value.var="wtcpue", drop=FALSE, fill=NA_real_)


# testDD[ # expand to include all year-stratum-spp combinations
# 	j={
# 		dcast.data.table(.SD, year+stratum+spp~., value.var="wtcpue", drop=FALSE, fill=NA_real_)
# 	},
# 	by=c("s.reg")
# ]








allSpp0 <- trawl2[, 
	j={
		yr.strat <- .SD[,list(year=year, stratum=stratum)]
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

# trawl1.1[,length(unique(spp)), by=c("s.reg","stratum","year","K")]

# Indicate that all of these rows were actually observed stratum-year combinations (and possibly obs absences of spp)
trawl1.1[,Obsd:=TRUE] # mark all rows as being "observed"
trawl1.1[is.na(wtcpue)&Obsd, wtcpue:=0] # mark observed absences (which are often not recorded, thus NA at this point) as 0's

# Strip down to names that I want to keep
trawl1.1 <- trawl1.1[,list(s.reg, year, stratum, lat, lon, depth, stemp, btemp, phylum, spp, wtcpue, Obsd)]


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
				stratum=unique(stratum)[!is.na(unique(stratum))]#,
				# K=unique(K)[!is.na(unique(K))] # K is a rep, or haul; not calling it haul so it doesn't get confused with original haulid (this is necessary b/c I redefine statum, so it's possible for two hauls to actually be reps when they were originally in different strata)
			 ), 
			 by="s.reg"] # build combinations


# ============================================================================
# = # ======================================================================
# = # ================================================================
# = # ==========================================================
# = # ====================================================
# = left off here .... taking up way too much memory =
# ==================================================== =
# ========================================================== =
# ================================================================ =
# ====================================================================== =
# ============================================================================
# Fix classes for merge (addressing some error message about K being integer and stratum being character)
# trawl1.1[,K:=as.character(K)]
trawl1.1[,year:=as.character(year)]
# allSpp[,K:=as.character(K)]

# Set keys before merge
setkey(trawl1.1, s.reg, year, stratum, spp)
setkey(trawl2, s.reg, year, stratum, spp)
setkey(allSpp, s.reg, year, stratum, spp) 



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
	setkey(trawl1.2, s.reg, year, stratum, spp)
}else{
	# Hopefully it never enters this, will probably throw an error if it does
	trawl1.2 <- trawl1.1[allSpp] # MERGE
	setkey(trawl1, s.reg, year, stratum, spp)
}

# Finish indicating which rows were actually "absence" data vs. "no observation" data
trawl1.2[is.na(Obsd), Obsd:=FALSE]

# Strip down to names that I want to keep
trawl1 <- trawl1.2[,list(s.reg, year, stratum, lat, lon, depth, stemp, btemp, phylum, spp, wtcpue, Obsd)]


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

trawl <- merge(trawl1, trawl2.tax2, by=c("s.reg","spp")) # merge trawl data with rebuilt taxonomic classification
setkey(trawl, s.reg, spp, stratum, year) # set key


# Need to rebuild numeric variables (but not CPUE data) after filling in time series w/ NA's
# This has already been done in cases where Obsd is TRUE (done in trawl1.1)
# Subsetting to !(Obsd) avoid redundancy w/ what was already done in trawl1.1 – should speed up slightly :)
trawl[!(Obsd), c("lat","lon","depth"):=list(fill.mean(lat), fill.mean(lon), fill.mean(depth)), by=c("s.reg", "stratum")]
trawl[!(Obsd), c("stemp","btemp"):=list(fill.mean(stemp), fill.mean(btemp)), by=c("s.reg","stratum","year")]


# Then aggregate across hauls (to be back compat) and generate "trawl"



# ========
# = Save =
# ========
setkey(trawl, s.reg, spp, year, stratum)
save(trawl, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
