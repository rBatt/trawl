

# Some experimenting to see if I could implement what I've learned about the laplacian matrix and stochastic/ transfer matrices.

# ======================
# = Load Sim Functions =
# ======================
sim.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/SimFunctions"
invisible(sapply(paste(sim.location, list.files(sim.location), sep="/"), source, .GlobalEnv))


# =================
# = Load Packages =
# =================
library(fields)


# ======================================
# = Experimented with Laplacian Matrix =
# ======================================
M <- 10
N <- 5
C0 <- matrix(0, nrow=M, ncol=N)
# C0[(M-1):M, c(1,N)] <- 50
C0[cbind(M:(M-N+1), 1:N)] <- 50
C0[cbind(M:(M-N+1), N:1)] <- 50

C0 <- matrix(C0,ncol=1)

disp <- 0.1 # 10% of the biomass in each vertex will disperse at each time step; i.e., the non-self-connecting edges would be 1/D where D is the degree of the vertex from which an edge is leaving. This would be a directed graph.
# Dinv <- solve(Deg) # inverse of the degree matrix

w <- sqrt(2)/(2*sqrt(2)+1)/2 # just a weighting for diagonal movement vs rook movement (based on the idea that moving between corners is a factor of sqrt(2) greater distance than moving between edges)

Trans00 <- matrix(0, nrow=M*N, ncol=M*N)
Trans00[cardinal(M, N, "north")] <- 1 - w*2
Trans00[cardinal(M, N, "northwest")] <- w
Trans00[cardinal(M, N, "northeast")] <- w

Trans0 <- Trans00
Trans0 <- Trans0*disp

Trans <- Trans0
diag(Trans) <- 1 - colSums(Trans)

dev.new(width=8, height=6)
par(mfrow=c(5,8), mar=c(1,1,0.1,0.1))
image.plot(t(matrix(C0, nrow=M)), zlim=c(0,60), ylim=c(1.05,0))
C.old <- C0
for(i in 1:39){
	
	C.t <- matrix(Trans%*%C.old, nrow=M)
	C.old <- matrix(C.t, ncol=1)
	
	image.plot(t(C.t), zlim=c(0,60), ylim=c(1.05,0))
}

# ==================
# = End Expt w/ LM =
# ==================