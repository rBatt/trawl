

# orginally made in leapfrog.R

# Distances between communities x & y
dist2 <- function(x,y, ...){
	
	nullDim.x <- is.null(dim(x))
	nullDim.y <- is.null(dim(y))
	stopifnot(nullDim.x==nullDim.y)
	nullDim <- nullDim.y & nullDim.x

	# Distance from each sampling unit in community x to each sampling unit in community y
	
	# x and y *must* have the same number of columns, and must be in same order (or have column names)
	
	# Assumes that x and y have same number of sampling units (although, see below)
	# That number of sampling units is n.str
	# if you had n.str.x and n.str.y,
	# change: [1:n.str,(n.str+1):(n.str*2)]
	# to: [1:n.str.x, (n.str.x+1):(n.str.x + n.str.y)]
	
	# add following lines to be general use, I think
	n.str.x <- ifelse(!nullDim.x, nrow(x), length(x))
	n.str.y <- ifelse(!nullDim.y, nrow(y), length(y))
	# then make the aforementioned change to [] subsetting
	
	# as.matrix(beta.div(rbind(x, y), nperm=0, save.D=TRUE)$D)[1:n.str,(n.str+1):(n.str*2)]
	if(nullDim){
		as.matrix(beta.div(c(x, y), nperm=0, save.D=TRUE, ...)$D)[1:n.str.x, (n.str.x+1):(n.str.x + n.str.y)] # untested		
	}else{
		as.matrix(beta.div(rbind(x, y), nperm=0, save.D=TRUE, ...)$D)[1:n.str.x, (n.str.x+1):(n.str.x + n.str.y)]
	}
}
