
update_dependencies <- function(){
	devtools::use_package("raster", type="Depends") # Basis for handling all data sets
	devtools::use_package("trawlData", type="Suggests") # Meets basic requirements for data content and format
	devtools::use_package("rbLib", type="Suggests")
	devtools::use_package("trawlDiversity", type="Suggests")
	devtools::use_package("fields", type="Depends")
}



