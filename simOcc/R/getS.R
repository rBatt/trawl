#' Get S
#' 
#' Helper function to get the equivalent of the S object from the guts of sim.spp.proc
#' 
#' @param x a spp object
#' @export
getS <- function(x){
	stopifnot(any(class(x)=="spp"))
	d <- attr(x, "dims")
	convS <- function(x){
		list(
			aperm(array(c(x), dim=c(d["grid.w"],d["grid.h"],d["ns"])), c(2,1,3))
		)
	}
	S0 <- unlist(apply(x, 3, convS), F, F)
	
	s.out <- lapply(S0, brick, xmn=0, xmx=d["grid.w"], ymn=0, ymx=d["grid.h"])
	s.out
}  