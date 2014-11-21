
# TODO need to change all the lookup code into functions
# TODO need to incorporate the manually-found species info


library(data.table)
library(rfishbase)
library(plyr)
library(taxize)

# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/rmWhite.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/sumna.R")
# source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/DataFunctions/meanna.R")

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

setnames(manualTax, c("taxon", "name"), c("spp", "sppCorr"))

manualTax[,spp:=cullParen(cullSp(fixCase(cullExSpace(spp))))]

setkey(manualTax, spp)
manualTax <- unique(manualTax)


tns.in.man <- trawl.newSpp[,spp]%in%manualTax[,spp] # find the trawl names in the manual file
man.in.tns <- manualTax[,spp]%in%trawl.newSpp[,spp] # find the manual names in the trawl object
trawl.newSpp2 <- rbind(trawl.newSpp[!tns.in.man], manualTax[man.in.tns, list(sppCorr, spp, common)]) # take all of the trawl object names not found in the manual file, then bind it to all of the manual names that were found in the trawl object



# ======================================
# = Update species names in trawl data =
# ======================================
setkey(trawl.newSpp2, spp)
setkey(trawl00, spp)
trawl0 <- merge(trawl00, trawl.newSpp2, all.x=TRUE, by="spp") #trawl[trawl.newSpp]

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
trawl <- trawl0[,list(region, s.reg, spp, taxLvl, common, year, datetime, stratum, lat, lon, depth, stemp, btemp, wtcpue, cntcpue, correctSpp)]
setkey(trawl, s.reg, taxLvl, spp, year, stratum)


# ====================
# = Fix date formats =
# ====================
# regular expression patterns
pat2y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{2})(?=\\s|$)" # for dates like 6/23/07 or 06/5/07 or 06/05/07
pat4y <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])(\\d{4})(?=\\s|$)" # for dates like 6/23/2007, or 06/23/2007, etc
pat4y.only <- "^(\\d{4})$" # for dates that are just the year, e.g., 2007

test <- c("7/15/10 17:59", "08/03/1997 17:58", "09/01/1987 18:00", "07-17-1991")
gsub(pat2y, "20\\3-\\1-\\2", test, perl=TRUE)
gsub(pat4y, "\\3-\\1-\\2", test, perl=TRUE)
# pat.mdy <- "^(\\d{1,2})(?:[\\/-])(\\d{1,2})(?:[\\/-])"

trawl[,datetime:=gsub(pat2y, "20\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/07 for 2007-6-23

trawl[,datetime:=gsub(pat4y, "\\3-\\1-\\2", datetime, perl=TRUE)] # e.g., switch out 6/23/2007 for 2007-6-23

trawl[,datetime:=gsub(pat4y.only, "\\1-01-01", datetime, perl=TRUE)] # e.g., switch out 2007 for 2007-01-01

# trawl[,datetime:=as.POSIXct(datetime, tz="GMT")] # note that the times get truncated # too slow, see below for better solution

# ======================
# = Add POSIX to trawl =
# ======================
trawl.posix <- data.table(datetime=trawl[,unique(datetime)], datetimeP=as.POSIXct(trawl[,unique(datetime)], tz="GMT"))
setkey(trawl.posix, datetime)
setkey(trawl, datetime)
trawl <- merge(trawl, trawl.posix, all.x=TRUE)
trawl[,datetime:=datetimeP]
trawl <- trawl[,j=names(trawl)[names(trawl)!="datetimeP"], with=FALSE]


# ========
# = Save =
# ========
setkey(trawl, s.reg, taxLvl, spp, year, stratum)
save(trawl, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
