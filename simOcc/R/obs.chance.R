#' Observation Chance
#' 
#' # Helper function used to create time-varying chances of observation (detectability) for species
#'
#' @param dim2 number of species; chances/ rng's always differ across \code{dim2} (unless chances supplied with 1 unique)
#' @param dim1 time steps; chances will only differ across \code{dim1} if length of arguments in \code{...} are > 1
#' @param dim3 replicates; chances will be same across \code{dim3}, but \code{dim1} shift by 1 index between each \code{dim3}
#' @param rand.gen a function for random number generation; its first argument must be \code{n}. Used to generate chances when \code{chances} is not specified.
#' @param chances Predetermined probabilities that can optionally be provided for each species. Also, length(chances) must be a multiple of \code{dim2.} If chances is supplied, \code{rand.gen} will not be used.
#' @param ... arguments to be passed to \code{rand.gen}
#' 
#' @details
#' The main use of the function will be when chances is not supplied, and the function performs the random number generation. In a given year, all species will be drawn from the same distribution. That distribution can change among years by supplying vectors to \code{...}. Between replicates, the year that corresponds to a particular vectorized argument in \code{...} rotates. For example, if \code{rand.gen=rnorm}, and \code{mean=c(0:2)} is supplied and \code{dim1=3}, in the first replicate the first time step will have a mean of 0, the second of 1, and the third of 2. The second replicate the first year will have a mean of 2, the second of 0, and the third of 1.
#' 
#' @return
#' An array with observation chances, with first dimension varying time, second by species, and third by replicate.
#' 
#' @export
obs.chance <- function(dim2=ns, dim1=grid.t, dim3=n.obs.reps, rand.gen=rnorm, chances, ...){

	# Create chance to be identified if caught
	# Can also be used to represent a generic
	#  time-varying chance of being detected
	
	# If chances is supplied, then each species will have the same chance in each year, and each replicate
	# species may differ tho

	rand.gen <- match.fun(rand.gen)
	dots <- list(...)
	# stopifnot(all.same(sapply(dots, length)))

	if(missing(chances)){
	
		if(length(dots)>0){
			chances <- matrix(mapply(rand.gen, MoreArgs=list(n=dim2), ...), nrow=dim2)
		}else{
			chances <- matrix(rand.gen(n=dim2,...))
		}
	
		v.rep <- function(...){rep(c(...),each=max(floor(dim1/ncol(chances)),1))}
	
		t.lvls <- unlist(apply(chances, 1, function(x)list(v.rep(x))),F,F)
	
	}else{
		stopifnot(dim2%%length(chances)==0)
		chances <- rep(chances, each=dim2%/%length(chances))
	
		t.lvls <- lapply(chances, c)
	}

	t.noID0 <- lapply(t.lvls, roll.recycle, dim3, dim1)
	t.noID <- aperm(array(simplify2array(t.noID0), dim=c(dim3,dim1,dim2)), c(2, 3, 1))
	t.noID	
}

