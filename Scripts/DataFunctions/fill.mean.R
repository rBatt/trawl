# Fill the NA values of a vector with the mean of the non-NA portion
fill.mean <- function(x){
	nai <- is.na(x)
	x[nai] <- meanna(x)
	x
}



