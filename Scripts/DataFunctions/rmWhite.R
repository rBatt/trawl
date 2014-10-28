
rmWhite <- function(x){
	stopifnot(is.data.table(x))
	nmx <- names(x)
	classes <- sapply(x, class)
	setClass <- classes%in%c("character","numeric","integer","integer64","logical") # leaving out factor b/c can't convert char to factor
	for(i in 1:ncol(x)){
		t.name <- as.name(nmx[i])
		if(setClass[i]){
			expr <- bquote(.(t.name):=as(gsub("^\\s* | \\s*$", "", .(t.name)), Class=classes[i]))
			x[,eval(expr)]
		}else{
			expr <- bquote(.(t.name):=gsub("^\\s* | \\s*$", "", .(t.name)))
			x[,eval(expr)]
		}
		
	}
}
