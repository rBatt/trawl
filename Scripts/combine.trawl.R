
library(data.table)
library(rfishbase)

source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/rmWhite.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/sumna.R")
source("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Scripts/meanna.R")


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
	trawl <<- rbindlist(mget(cleanNames)) # assign to global, or if found in a parent, reassign in that environment
	# print(ls()) # note that ls() default is to current frame, so only see locally-defined variables with ls()
	rm(list=ls()) # thus removing ls() preserves trawl and all variables defined outside of call to local()
})

# ============================
# = Convert spp to character =
# ============================
trawl[,spp:=as.character(spp)]

# ======================
# = Remove bad species =
# ======================
uspp <- unique(trawl[,spp])
badEgg <- uspp[grepl("[eE][gG]{2}", uspp)]
badFish <- uspp[grepl("(?<![a-z])fish(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
badLarv <- uspp[grepl("(?<![a-z])larv(a[e])?(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
# badJuv <- uspp[grepl("(?<![a-z])juven(ile)?(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]
badYoy <- uspp[grepl("(?<![a-z])yoy(?![a-z])", uspp, ignore.case=TRUE, perl=TRUE)]

badSpp <- unique(c(badEgg, badFish, badLarv, badYoy))

setkey(trawl, spp)
trawl <- trawl[!.(badSpp),]

# ========================
# = Note redundant names =
# ========================
# "ACANTHOCARPUS ALEXANDRI"
# # and
# "ACATHOCARPUS ALEXADRI" 

trawl["ACATHOCARPUS ALEXADRI", spp:="ACANTHOCARPUS ALEXANDRI"] # fix a redundant species name due to typo


# ====================================
# = Functions for cleaning spp names =
# ====================================


fixCase <- function(x){
	s <- paste(toupper(substring(x, 1, 1)), substring(x, 2), sep="")
	paste(substring(s, 1, 1), tolower(substring(s, 2)), sep="")
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
trawl[,spp:=cullParen(cullSp(fixCase(spp)))]
setkey(trawl, spp, year, s.reg)

trawl[,isSpecies:=is.species(spp)] # infer whether the taxa are identified to species or to genus (1 or 2 words)



# trawl2 <- trawl
# trawl2[,c("wtcpue", "cntcpue","depth","btemp","stemp"):=list(wtcpue=sumna(wtcpue), cntcpue=sumna(cntcpue), depth=meanna(depth), btemp=meanna(btemp), stemp=meanna(stemp)), by=c("year","datetime","spp","haulid","stratum","stratumarea","lat","lon","region","s.reg")]
# dim(trawl) # 997913     15
# dim(trawl2) # 997913     15 ... this means that species weren't ID'd in multiple ways during the same haul.




# ====================
# = Fix date formats =
# ====================
# regular expression patterns
pat2y <- "^(\\d{1,2})(?:\\/)(\\d{1,2})(?:\\/)(\\d{2})(?=\\s)" # for dates like 6/23/07 or 06/5/07 or 06/05/07
pat4y <- "^(\\d{1,2})(?:\\/)(\\d{1,2})(?:\\/)(\\d{4})(?=\\s)" # for dates like 6/23/2007, or 06/23/2007, etc
pat4y.only <- "^(\\d{4})$" # for dates that are just the year, e.g., 2007

trawl[,datetime:=gsub(pat2y, "20\\3-\\1-\\2", datetime, perl=TRUE)] # switch out 6/23/07 for 2007-6-23
# trawl[,unique(datetime)]

trawl[,datetime:=gsub(pat4y, "\\3-\\1-\\2", datetime, perl=TRUE)] # switch out 6/23/2007 for 2007-6-23
# trawl[,unique(datetime)]

trawl[,datetime:=gsub(pat4y.only, "\\1-01-01", datetime, perl=TRUE)] # switch out 2007 for 2007-01-01
# trawl[,unique(datetime)]

trawl[,datetime:=as.POSIXct(datetime, tz="GMT")] # note that the times get truncated
# trawl[,sort(unique(datetime))]



# ========
# = Save =
# ========
setkey(trawl, s.reg, region, year, datetime, spp, stratum, haulid)
save(trawl, file="/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
