
spp2msom <- function(sim.obs){
	n.substrat <- dim(attributes(sim.obs)$X.obs)[2]
	n.strat <- nrow(sim.obs)
	ns <- attr(sim.obs, "dims")["ns"]
	grid.t <- attr(sim.obs, "dims")["grid.t"]
# 1) Label and sort rows of obs.arr according to stratum-substratum organization
	# Indicate which rows belong to which strata
	stratID0 <- 1:n.strat # stratum ID's (order would fill a matrix b)
	stratID <- c(matrix(stratID0, nrow=attr(sim.obs, "dims")[1], byrow=TRUE)) # reorders to raster convention
	# keyK.rast0 <- gen.grid(stratID, c(attr(sim.obs, "dims")[1:2],1), byrow=F) # create a grid
	# keyK.rast <- values(disaggregate(keyK.rast0, fact=sqrt(n.substrat), method=""))

	# Sort (rows of obs.arr) by stratum ID; in way that is convenient for filling output array
	# obs.arr.sort <- attr(sim.obs, "Z.obs")[order(keyK.rast),,]

# 2) Exploit reorganization of obs.arr to fill output array
	# Create output array (which is input array for MSOM analysis)
	# Dimension 1 of sorted obs.arr (location) fills dims 1&2 out output array (stratum, K)
	# Dimension 2 of sorted obs.arr (spp) fills dim 3 of output array (spp)
	# Dimension 3 of sorted obs.arr (year) corresponds to each element of list of output arrays
	# fill.dims <- c(n.substrat, n.strat, ns) # can't fill byrow=T, so flip row/col and aperm() later
	# a.o0 <- function(x){list(array(c(x), dim=fill.dims))} # function to fill/make arrays
	# output.array0 <- unlist(apply(obs.arr.sort, 3, a.o0), F, F) # split 1st dim into 2, and make 3rd dim a list
	# output.array <- lapply(output.array0, aperm, c(2,1,3)) # rearrange dim to row=stratum & col=K (unflip)
	
	output.array <- unlist(apply(attr(sim.obs,"X.obs"), 4, function(x)list(x)),F,F)


	# Checking
	# plot(subset(attr(sim.obs, "obs")[[12]], 10)) # plot critter 10 in year 12
	# attr(sim.obs, "obs.arr")[1:50,10,12]; obs.arr.sort[1:50,10,12] # first 3+ strata of critter 10 in year 12
	# output.array[[12]][1:4,,10] # first 4 strata of critter 10 in year 12

# 3) Name dimensions (dim) and indices within each dimension (dimnames)
	# list elements are years
	names(output.array) <- paste0("year",1:grid.t)

	# dim should have names c("stratum", "K", "spp")
	strat.names <- as.character(stratID0) # Dimension 1 ("stratum") has integer names
	K.names <- as.character(1:n.substrat) # Dimension 2 ("K") has integer names
	spp.names <- paste0("critter",1:ns) # Dimension 3 ("spp") has contrived character names (e.g., "critter1")
	name.arrays.in.list <- function(x){ # function to apply names within list
		dimnames(x) <- list("stratum"=c(strat.names), "K"=c(K.names), "spp"=c(spp.names))
		x
	}
	simDat <- lapply(output.array, name.arrays.in.list) # give names via lapplying naming function
	simCov <- unlist(apply(values(attr(sim.obs, "grid.X")), 2, list),F,F)
	simCov.NA <- lapply(simCov, function(x){x[] <- NA; x})
	simCov.precs <- lapply(simCov, function(x){x[] <- 1E6; x})
	simCov.precs.bad <- lapply(simCov, function(x){x[] <- 1E-6; x})
	sim.cov.names <- data.frame(s.reg="sim",year=1:grid.t, num=1:length(simDat))
	
	list(simDat=simDat, simCov=simCov, simCov.NA=simCov.NA, simCov.precs=simCov.precs, simCov.precs.bad=simCov.precs.bad, sim.cov.names=sim.cov.names)

}