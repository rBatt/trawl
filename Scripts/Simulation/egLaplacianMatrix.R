
library(fields)
library(igraph)

# ============================================
# = Wikipedia Example for Laplacian Operator =
# ============================================
# http://en.wikipedia.org/wiki/Laplacian_matrix#Example_of_the_Operator_on_a_Grid

N <- 20
C0 <- matrix(0, nrow=N, ncol=N)
C0[2:5, 2:5] <- 5
C0[10:15, 10:15] <- 10
C0[2:5, 8:13] <- 7
C0 <- matrix(C0,ncol=1)

Adj <- matrix(0, nrow=N*N, ncol=N*N)
Adj[adjacent(as.array(matrix(1, nrow=N, ncol=N)), cells=1:(N*N), direction=8)] <- 1
Deg <- diag(degree(graph.adjacency(Adj), mode="out"))

L <- Deg - Adj
# L <- as.matrix(graph.laplacian(graph.adjacency(Adj), norm=F)) # same as above, but skips calculating the degree matrix and goes straight to the Laplacian matrix
eig.L <- eigen(L)
V <- eig.L$vectors
D <- eig.L$values

C0V <- t(V)%*%C0


dev.new(width=12, height=8)
par(mfrow=c(4,6), mar=c(1,1,0.1,0.1))
for(i in seq(0,5, by=0.25)){
	Phi <- C0V*exp(-D*i) # this is the solution to the differential equation dC/dt = k*lambda*c[i]; thus, to predict one time step ahead (dt=1), simply multiply the previous C by the eigenvalues (lambda)
	Phi <- V%*%Phi
	Phi <- matrix(Phi, nrow=N, ncol=N)
	image.plot(t(Phi), ylim=c(1,0), xlim=c(0,1), zlim=c(0,10))
}

# ======================
# = End Wiki Eg for LO =
# ======================