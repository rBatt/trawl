
# Cut years into 9 chunks (or min(n.yrs))
cy <- function(x){
	lux <- length(unique(x))
	if(lux>1){
		as.character(cut(as.numeric(x), breaks=min(2,lux)))
	}else{
		unique(x)	
	}
}