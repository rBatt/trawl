#' Cardinal Directions
#' 
#' Given a 2D grid of specified dimensions (i.e., matrix), returns indices of cells that lie in a cardinal orientation to each focal cell.
#' 
#' @param nr number of rows (must be >=3)
#' @param nc number of columns (must be >=3)
#' @param cdir Direction character. Default "north".
#' 
#' @return A matrix indicating, for each cell in a hypothetical 2D grid, the coordinates of the cell in the same matrix that lie in the specific cardinal direction.
#' 
#' @details
#' Goal of this function is to figure out which pairs of elements of a matrix (when listed linearly, e.g., a 3x3 matrix has elements 1-9, column-wise) lie a particular orientation to each other. These orientation are supplied via `cdir`, and are listed as the 4 cardinal directions (north west east south) and the 4 primary intercardinal directions (northwest southwest northeast southeast).
#' 
#' @export
cardinal <- function(nr, nc, cdir="north"){
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