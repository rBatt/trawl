#' Simulate Occupancy
#' 
#' Simulate species occupying a 2D environmental grid, and the observation of that occupancy. 
#' 
#' @param ... arguments to be passed to functions \code{\link{sim.env}}, \code{\link{sim.spp.proc}}, \code{\link{obs.spp}}.
#' @param detect.mus,detect.sd numeric vectors specifying the mean(s) and sd(s) of the parent distribution of species detectability on a logit scale. Default of 0 corresponds to 50\% detectability. Form detectabilities that are passed to t.noID \code{\link{obs.spp}}. Also, both can be of variable length, and need not be of same length, but must be multiples of each other, and it is best if lengths are multiples of \code{grid.t}. 
#' @param format.msom character, format for msom? default is to simply output simulation
#' @param n0s if formatting for an msom, how many never-observed (0) species to pad with
#' @param n.obs.reps how many times to repeat the observation process, while holding the true process outcome the same, and holding the observation parameters the same (but perhaps changing which years get which detectability parameters)
#' 
#' @details
#' stuff
#' 
#' @return
#' a list of spp objects
#' 
#' @examples
#' sim_occ(ns=20)
#' 
#' @export
sim_occ <- function(..., detect.mus=0, detect.sd=0.1, format.msom=c("none","jags","stan"), n0s=5, n.obs.reps=1){
	
	# ---- Local functions ----
	local.sim.env <- function(grid.X, ns, alpha_mu, alpha_sd, dynamic, AR1.coef, D.frac, M.frac, persist.bonus, x, n.ss, n.ss.mu, base.chance, t.noID, ...) sim.env(...)
	
	local.sim.spp.proc <- function(grid.w, grid.h, grid.t, X.slope, h.slope, half.val, w.sd, t.sd, x, n.ss, n.ss.mu, base.chance, t.noID, ...) sim.spp.proc(...)
	
	local.obs.spp <- function(grid.X, ns, alpha_mu, alpha_sd, dynamic, AR1.coef, D.frac, M.frac, persist.bonus, grid.w, grid.h, grid.t, X.slope, h.slope, half.val, w.sd, t.sd, ...) obs.spp(...)
	

		
	# ---- Simulate ----
	# Simulate environment
	env <- local.sim.env(...) #sim.env(grid.w=grid.w, grid.h=grid.h, grid.t=grid.t, X.slope=X.slope, h.slope=1, half.val=0, w.sd=1, t.sd=0.1)
	
	# Simulate species true process
	out <- local.sim.spp.proc(grid.X=env, ...)
	dims <- attr(out, "dims")
	grid.t <- dims["grid.t"]
	grid.w <- dims["grid.w"]
	grid.h <- dims["grid.h"]
	ns <- dims["ns"]

	
	format.msom <- match.arg(format.msom)
	
	t.noID <- plogis(obs.chance(dim2=ns, dim1=grid.t, dim3=n.obs.reps, mean=detect.mus, sd=detect.sd))
	
	# print(match.call(local.obs.spp, call("local.obs.spp", x=out, t.noID[,,1], ...)))
	
	# Do observation process
	for(i in 1:n.obs.reps){
		if(i==1){
			out.obs <- local.obs.spp(..., x=out, t.noID=t.noID[,,i])
			formatted <- spp2msom(out.obs)
			new.simDat <- formatted$simDat 
			# simCov <- formatted$simCov
			# simCov.NA <- formatted$simCov.NA
			# simCov.precs <- formatted$simCov.precs
			# simCov.precs.bad <- formatted$simCov.precs.bad
			# sim.cov.names <- formatted$sim.cov.names
	
			big.out.obs <- list(out.obs)
	
			names(new.simDat) <- paste(names(new.simDat),i, sep=".")
			big.simDat <- new.simDat
		}else{
			big.out.obs[[i]] <- local.obs.spp(..., x=out, t.noID=t.noID[,,i])
			new.simDat <- spp2msom(big.out.obs[[i]])$simDat
			names(new.simDat) <- paste(names(new.simDat),i, sep=".")
			big.simDat <- c(big.simDat, new.simDat)
		}
	}
	
	if(format.msom!="none"){
		return(list(big.out.obs=big.out.obs, big.simDat=big.simDat, formatted=formatted))
	}else{
		return(big.out.obs)
	}
	
}