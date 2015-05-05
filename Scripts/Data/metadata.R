
library(devtools)
install_github("ropensci/EML", build=FALSE, dependencies=c("DEPENDS", "IMPORTS"))

library(EML)





# ====================
# = Aleutian Islands =
# ====================

load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/cleanedRegions/ai2.RData")
ai.mdData <- ai2[1:10]

ai.md.col.defs <- c(
	"year" = "year of haul", 
	"datetime" = "the day and time of the haul", 
	"spp" = "species scientific name; Genus species", 
	"haulid" = "a unique identifier for the haul; vessel ID - cruise ID - haul number", 
	"stratum" = "the center of the 1 degree grid cell in which the haul was taken; longitude latitude", 
	"stratumarea" = "the area of the statistical stratum (no longer in data set)", 
	"lat" = "latitude of the haul", 
	"lon" = "longitude of the haul, in western hemisphere degrees (for lon > 0, do lon-360)", 
	"depth" = "the maximum depth of the water at the location of the haul", 
	"stemp" = "water temperature at the surface at the location of the haul", 
	"btemp" = "water temperature at the bottom at the location of the haul",
	"wtcpue" = "weight (mass) of the catch", 
	"cntcpue"="number of individuals caught per hectare in the haul", 
	"region" = "the name of this data set, identified by north american sampling region and by the organization doing the sampling",
	"s.reg" = "short region name"
)


# Define custom units
# need numberPerHectare for cntcpue
cntpue.unit <- eml_define_unit("numberPerHectare", unitType="otherUnitType", parentSI="numberPerKilometerSquared", multiplierToSI="0.01", description="number per hectare")

# ai.md.unit.defs <- list(
# 	"year"=c(
# 		unit = "number",
# 		precision = 1
# 		),
# 	"datetime" = c(
# 		format = "MM/DD/YYYY HH:MM"
# 		),
# 	"spp" = "Genus species",
# 	"haulid" = "haul id",
# 	"stratum" = "1 degree lon lat",
# 	"stratumarea"=c(
# 		unit = "kilometer",
# 		precision = 1
# 		),
# 	"lat"=c(
# 		unit = "degree",
# 		precision = 0.5
# 		),
# 	"lon"=c(
# 		unit = "degree",
# 		precision = 0.5
# 		),
# 	"depth"=c(
# 		unit = "meter",
# 		precision = 1
# 		),
# 	"stemp"=c(
# 		unit = "celsius",
# 		precision = 0.1
# 		),
# 	"btemp"=c(
# 		unit = "celsius",
# 		precision = 0.1
# 		),
# 	"wtcpue" =c(
# 		unit = "kilogramsPerHectare",
# 		precision = 0.0001
# 		),
# 	"cntcpue"=c(
# 		unit = "numberPerHectare",
# 		precision = 0.0001
# 		),
# 	"region"="region",
# 	"s.reg"="short region"
#
# )




ai.md.unit.defs <- list(
	"year"=c(
		unit = "number",
		precision = 1
		), 
	"datetime" = c(
		format = "MM/DD/YYYY HH:MM"
		), 
	"spp" = "Genus species", 
	"haulid" = "haul id", 
	"stratum" = "1 degree lon lat", 
	"stratumarea"=c(
		unit = "kilometer",
		precision = 1
		), 
	"lat"=c(
		unit = "degree",
		precision = 0.5
		), 
	"lon"=c(
		unit = "degree",
		precision = 0.5
		), 
	"depth"=c(
		unit = "meter",
		precision = 1
		), 
	"stemp"=c(
		unit = "celsius",
		precision = 0.1
		), 
	"btemp"=c(
		unit = "celsius",
		precision = 0.1
		), 
	"wtcpue" =c(
		unit = "kilogramsPerHectare",
		precision = 0.0001
		),
	"cntcpue"=c(
		unit = "numberPerKilometerSquared", # this is wrong, needs to be per hectare
		precision = 0.01
		), 
	"region"="region", 
	"s.reg"="short region"
	
)





# eml_write(ai.mdData, col.defs=ai.md.col.defs, unit.defs=ai.md.unit.defs, creator="Ryan Batt <battrd@gmail.com>",file="testAI_metaData.xml")


blah <- eml(dat=ai.mdData, title="aiMetaData", col.defs=ai.md.col.defs, unit.defs=ai.md.unit.defs, creator="Ryan Batt <battrd@gmail.com>")

eml_write(blah, title="aiMetaData", col.defs=ai.md.col.defs, unit.defs=ai.md.unit.defs, creator="Ryan Batt <battrd@gmail.com>",file="testAI_metaData.xml")

eml_validate("testAI_metaData.xml")


#
# c("year",
# "datetime",
# "spp",
# "haulid",
# "stratum",
# "stratumarea",
# "lat",
# "lon",
# "depth",
# "stemp",
# "btemp",
# "wtcpue",
# "cntcpue",
# "region",
# "s.reg"
# )







