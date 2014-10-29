sstTimeSlope <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		as.numeric(lm(x~I(0:(length(x)-1)))$coef[2])
	}
}