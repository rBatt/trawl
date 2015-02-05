plot.post <- function(x, fun=NULL, ...){
	
	# Plotting function
	if(!is.null(fun)){
		fun <- match.fun(fun)
	}else{
		fun <- function(x){plot(density(x), ...)}
	}
	
	if(ncol(x)>1){
		fun(apply(x, 2, mode))
	}else{
		fun(x)
	}
}