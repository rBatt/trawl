


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



# =======================================
# = Save Compiled Taxonomic Information =
# =======================================
taxInfo <- trawl0[,list(spp, common, taxLvl, species, genus, family, order, class, superclass, subphylum, phylum, kingdom, raw.spp, isSpecies, correctSpp)]
setkeyv(taxInfo, names(taxInfo))
taxInfo <- unique(taxInfo)
save(taxInfo, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/Taxonomy/taxInfo.RData")


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

# ======================
# = Save the raw trawl =
# ======================
setkey(trawl2, s.reg, year, stratum, K, phylum, spp)
save(trawl2, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl2.RData")

