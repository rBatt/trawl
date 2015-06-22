
# Goal of this function is to figure out which pairs of elements of a matrix (when listed linearly, e.g., a 3x3 matrix has elements 1-9, column-wise) lie a particular orientation to each other. These orientation are supplied via `cdir`, and are listed as the 4 cardinal directions (north west east south) and the 4 primary intercardinal directions (northwest southwest northeast southeast).
cardinal <- function(nr, nc, cdir="north"){ # TODO This is doing a little bit of boundary reflection, kinda, which is not by design and needs to be fixed (what I'm calling a boundary reflection is not actually a boundary reflection, so it isn't right even if I wanted boundary reflection)
	# stopifnot(dim(mat)==2 & ncol(mat)==2)
	stopifnot(nr>=3 & nc>=3)
	
	N <- nr*nc
	mat <- expand.grid(1:N, 1:N)
	colnames(mat) <- c("row", "column")
	mat <- as.matrix(mat)
	
	del <- mat[,2] - mat[,1]
	
	ind <- switch(cdir,
		north = del==1,
		south = del==-1,
		northwest = del==(nr+1),
		southeast = del==(-nr-1),
		northeast = del==(-nr+1),
		southwest = del==(nr-1)
	)
	
	
	# Define cases where the above method will fail due to boundaries. Has some cases that wouldn't be included anyway.
	badN <- (1:N)[(1:N)%%nr==1]
	badW <- (1:nr)
	badS <- (1:N)[(1:N)%%nr==0]
	badE <- (nr*(nc-1)+1):(nr*nc)
	
	out <- mat[ind,] # output refers to coordinates in an adjacency matrix
	
	o2 <- out[,2]
	
	out <- switch(cdir,
			north = out[!o2%in%badN,],
			south = out[!o2%in%badS,],
			northwest = out[!o2%in%c(badN, badW),],
			southeast = out[!o2%in%c(badS, badE),],
			northeast = out[!o2%in%c(badN, badE),],
			southwest = out[!o2%in%c(badS, badW),]
		)
		
	return(out)
	
}