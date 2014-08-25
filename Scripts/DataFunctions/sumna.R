sumna <- function(x){
	if(!all(is.na(x))) return(sum(x, na.rm=T))
	if(all(is.na(x))) return(as.numeric(NA))
}