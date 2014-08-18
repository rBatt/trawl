
rmWhite <- function(x){
	stopifnot(is.data.table(x))
	nmx <- names(x)
	for(i in 1:ncol(x)){
		t.name <- as.name(nmx[i])
		expr <- bquote(.(t.name):=gsub("^\\s* | \\s*$", "", .(t.name)))
		x[,eval(expr)]
	}
}
