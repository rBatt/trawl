meanna <- function(x){
	if(!all(is.na(x))) return(mean(x, na.rm=T))
	if(all(is.na(x))) return(as.numeric(NA))
}