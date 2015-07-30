hist(sapply(attr(out, "spp.densX"), function(x)mean(x$y))) # density plot from each species' temperature distribution; density plots are across same range of X. species with high mean densities are those that only exist in a narrow region.

# Show how you can narrow a species niche if the 
# parameters of the beta distribution tend to be bigger
# i.e., if alpha and beta parameters are drawn from a 
# uniform distribution that simply covers a larger range,
# then the beta distribution resulting from those parameters will
# have a lower standard deviation, causing species to be more specialist
beta.var <- function(x){prod(x)/(sum(x)^2*(sum(x)+1))}
ab.mu <- c()
bsd <- c()
for(i in 1:20){
	vals <- replicate(5E2, runif(n=2, min=1, max=5+i))
	ab.mu[i] <- mean(apply(vals, 2, mean))
	bsd[i] <- mean(apply(vals, 2, beta.var))
}
plot(ab.mu, sqrt(bsd))


vals.narrowUnif <- replicate(5E2, runif(n=2, min=1, max=3))
hist(apply(vals.narrowUnif, 2, beta.var), xlim=c(0,0.1), col="gray50", ylim=c(0,200))

vals.mediumUnif <- replicate(5E2, runif(n=2, min=1, max=10))
hist(apply(vals.mediumUnif, 2, beta.var), xlim=c(0,0.1), add=TRUE, col=adjustcolor("red",0.5))

vals.wideUnif <- replicate(5E2, runif(n=2, min=1, max=20))
hist(apply(vals.wideUnif, 2, beta.var), xlim=c(0,0.1), add=TRUE, col=adjustcolor("blue",0.5))