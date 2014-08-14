
rmWhite <- function(x){
	stopifnot(is.data.table(x))
	for(i in 1:ncol(x)){
		t.name <- names(x)[i]
		expr <- bquote(.(as.name(t.name)):=gsub("^\\s* | \\s*$", "", .(as.name(t.name))))
		x[,eval(expr)]
	}
}
