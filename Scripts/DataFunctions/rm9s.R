

rm9s <- function(x){
	stopifnot(is.data.table(x))
	for(i in seq_along(x)){
		set(x, i=which(x[[i]]==-9999L), j=i, value=as.character(NA))
		set(x, i=which(x[[i]]==-9999.0), j=i, value=as.character(NA))
	}
}
