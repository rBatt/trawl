
auto.mfrow <- function(x, tall=FALSE){
	stopifnot(x>0)
	
	if(x==1L){return(c(1,1))}
	
	ran <- 2:max(floor((x-1)/2),1)
	ran2 <- pmax(ceiling(x/(ran)),1)
	rem <- abs(x - ran2*ran)
	
	score <- abs(sqrt(x)-(ran)) + abs(sqrt(x)-(ran2)) + rem
	mf1 <- ran[which.min(score)]
	mf2 <- ran2[which.min(score)]
	
	if(!tall){
		return(sort(c(mf1, mf2)))
	}else{
		return(sort(c(mf1, mf2), TRUE))
	}	
}
