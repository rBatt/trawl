#' Density Sample
#' 
#' What is the density of each observation in X given the empirical distribution of a sample ref?
#' 
#' @param ref either an object of class density, or a numeric vector representing a random sample that should form the empirical reference distribution
#' @param x samples (quantiles) for which to calculate the density
#' @param relative Default FALSE, else calculation is scalled so that maximum returned is 1
#' 
#' @details
#' Similar to dnorm(), e.g., but instead of assuming the normal distribution, instead assumes an empirical distribution based on density(ref)
#' If ref is of class == "density", then the density of a value in x is calculated based on ref, otherwise, calculated based on density(ref).
#' 
#' @export
dsample <- function(ref,x, relative=FALSE){

	# if(class(ref)!="density"){
	if(!all(c("x","y")%in%names(ref))){
		ref <- density(ref)
	}
	
	# ref.y <- ref$y
	# ref.x <- ref$x
	
	ref.xy <- list(x=ref$x, y=ref$y)
	
	if(relative | max(ref.xy$y)>1){
		if(all(ref.xy$y==0)){
			stop("all probabilities are 0; relative=TRUE cannot be used")
		}else{
			ref.xy$y <- ref.xy$y/max(ref.xy$y)
		}
	}
	
	ds <- approxfun(ref.xy, yleft=0, yright=0)
	
	dsx <- ds(x)
	# if(relative | max(dsx)>1){
# 		if(all(dsx==0)){
# 			return(dsx)
# 		}else{
# 			return(dsx/max(dsx))
# 		}
# 	}else{
# 		return(dsx)
# 	}

	return(dsx)
	
}