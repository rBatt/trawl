
# Density of an empirical distribution

# What is the density of each observation in X given the empirical distribution of a sample ref?
# Similar to dnorm(), e.g., but instead of assuming the normal distribution, instead assumes an empirical distribution based on density(ref)
# If ref is of class == "density", then the density of a value in x is calculated based on ref, otherwise, calculated based on density(ref).

dsample <- function(ref,x, relative=FALSE){

	if(class(ref)!="density"){
		ref <- density(ref)
	}
	
	ref.y <- ref$y
	ref.x <- ref$x
	
	ds <- approxfun(ref)
	
	dsx <- ds(x)
	if(relative){
		return(dsx/max(dsx))
	}else{
		return(dsx)
	}
	
}