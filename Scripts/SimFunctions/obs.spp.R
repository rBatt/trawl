
# Function for observing species




x <- out
obs.spp <- function(x){
	
	stopifnot(any("spp" == class(x)))
	
	
	spp.bio <- attr(x, "spp.bio")
	grid.X <- attr(x, "grid.X")
	S.dens.X <- attr(x, "spp.densX")
	dims <- attr(x, "dims")
	
	# =====================
	# = Observe Substrata =
	# =====================
	# Probabilities for sampling a spot
	# Each grid cell needs to be subdivided
	n.ss <- 9 # number of substrata
	S <- getS(x)
	S.ss <- lapply(S, disaggregate, fact=sqrt(n.ss), method="")

	# Subdivided grid cells have constant probability of being observed
	
	stratum <- setValues(subset(grid.X,1), 1:ncell(grid.X))
	sub.stratum <- disaggregate(stratum, fact=sqrt(n.ss), method="")
	
	
	n.ss.mu <- 100 # average number of substrata observed (total)
	n.ss.obs0 <- min(rpois(1, n.ss.mu), ncell(sub.stratum)) # number of substrata observed this time
	
	all.ss <- 1:ncell(sub.stratum)
	guaran.ss <- sampleStratified(sub.stratum, 1)[,"cell"]
	other.ss <- all.ss[!all.ss%in%guaran.ss]
	
	n.ss.obs <- n.ss.obs0 - length(guaran.ss) # length(guaran.ss) is set to ncell(stratum) right now	
	
	obs.ss.cells <- c(guaran.ss, sample(other.ss, n.ss.obs, F))
	obs.ss <- setValues(sub.stratum, NA)
	obs.ss[obs.ss.cells] <- 1
	
	# Number of substrata sampled per stratum
	table(extract(sub.stratum, obs.ss.cells))
	hist(table(extract(sub.stratum, obs.ss.cells)))
	
	# Plot showing which substrata were sampled
	dev.new(height=3.5, width=2.5+0.95)
	par(mar=c(2,2,0.25,0.1))
	plot(sub.stratum)
	image((obs.ss), add=TRUE, col=adjustcolor("black", 0.25), axes=FALSE, xlab="", ylab="")
	
	
	
	# Probabilities for species
	# Change by species, possibly by time, possibly with dummy covariate
	gear.chance
	gear.dummy # binary, ns-by
	
	
	
	

	
	
	
	
}





















# 1 - (p(1-A) * p(1-B) * p(1-C)) == p(A|B|C)

# A <- 0.25
# B <- 0.25
# C <- 0.25
# p <- c(A,B,C)
#
# all.same <- function(x){
# 	abs(max(x) - min(x)) < 8.881784e-16 # number is (.Machine$double.eps)*4 on my rMBP
# }
#
# n <- 3:1
# pTot <- function(p, n=1){
# 	ln <- length(n)
# 	if(ln>1 | !all.same(p)){
# 		# stopifnot(ln == length(p))
# 		pn <- 1-((1-p)^n)
# 	}else{
# 		pn <- p
# 	}
#
# 	1-prod(1 - pn)
#
# }



