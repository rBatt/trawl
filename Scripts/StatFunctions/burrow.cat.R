
# ======================
# = Burrows Categories =
# ======================
# Climate trajectory features
# Burrows et al. "Geographical limits to species-range shifts are suggested by climate velocity". 2014. Nature 507:492-495
# Categorize the balance and movement of isotherms in a location based on
# A) the (constant) number of trajectories starting in the cell
# B) the number of trajectories arriving in the cell
# C) the number of trajectories passing through the cell
# The numbers of trajectories (A-C) are supplied as a fraction of total:
# n.start = A/(A+B+C)
# n.end = B/(A+B+C)
# n.ft = C/(A+B+C)
# Note that Burrows did not describe the "balanced" category; they pre-screened for non- or slow-moving trajectories, and estimated trajectories and finer (and more regular) spatio temporal resolution than the "per stratum per year" data that I'm using here. 
# To modify to reflect burrows procedure: Give the function a "velocity" argument, and first categorized velocities at less than 0.4 km/year (20 km / 50 years) as "non-moving", then categorized (velocity>=0.4 & velocity<=2) as "slow-moving", and then apply the %-based categories only if velocities were velocity>2.
burrow.cat <- function(n.start, n.end, n.ft){
	categ <- rep(NA, length(n.start))
	
	source.logic <- n.end==0
	sink.logic <- n.end>0.45 & n.start<0.15
	corridor.logic <- n.ft>0.7 & n.end>0
	divergence.logic <- n.end>n.start & !corridor.logic & !sink.logic
	convergence.logic <- n.start>n.end & !corridor.logic & !source.logic
	balance.logic <- n.start==n.end & !corridor.logic
	
	categ[source.logic] <- "Source"
	categ[sink.logic] <- "Sink"
	categ[corridor.logic] <- "Corridor"
	categ[divergence.logic] <- "Divergence"
	categ[convergence.logic] <- "Convergence"
	categ[balance.logic] <- "None"
	
	categ
}
