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


# ================================================
# = Logistic Regression & dsample() Sanity Check =
# ================================================
# From this I concluded it is a good idea to use the relative=TRUE argument

x <- seq(-10, 10, by=0.1)
prob <- dsample(S.dens.X[[9]], x, relative=TRUE)
prob <- dsample(S.dens.X[[10]], x, relative=TRUE)
prob <- dsample(S.dens.X[[11]], x, relative=TRUE)
prob <- dsample(S.dens.X[[12]], x, relative=TRUE)
n <- length(x)
y <- rbinom(n=n, size=1, prob=prob)

(mod <- (glm(y~x+I(x^2), family="binomial")))
(summary(mod))

pred <- plogis(mod$coef[1]+mod$coef[2]*x+mod$coef[3]*x^2) # manual calculation of prob

plot(x,plogis(predict(mod)), ylim=c(0,1)) # model prediction
lines(x, pred, col="red") # manually calculated predictions
lines(x, prob, col="blue") # "true" values


# ============================================================
# = What would the distribution of regression parameters be? =
# ============================================================
# In the msom, the intercept and 2 parameters of the polynomial are supposed to be from some parent distrubtion. I.e., the species temperature preferences aren't entirely independent. They are all realizations of some parent.
get.cov.params <- function(d){
	# d <- S.dens.X[[2]]
	 # d <- S.dens.X[[5]]
	x <- seq(-10, 10, by=0.01)
	n <- length(x)
	prob <- dsample(d, x, relative=TRUE)
	y <- rbinom(n=n, size=1, prob=prob)
	# y <- rbinom(n=n, size=1, prob=pmin(pmax(prob,1E-3),1-1E-3))
	
	# mod <- suppressWarnings(glm(y~x+I(x^2), family="binomial"))	
	# mod <- (glm(y~x+I(x^2), family="binomial"))
	mod <- tryCatch(
		{c(glm(y~x+I(x^2), family="binomial")$coef,0)}, 
		warning=function(w){
			suppressWarnings({c(glm(y~x+I(x^2), family="binomial")$coef,nchar(w$message))})
		}
	)
	
	names(mod) <- c("u.a0", "a3", "a4", "warning")
	mod <- c(mod, "best.temp"=d$x[which.max(d$y)])
	mod
	
	# plot(x,y,pch=20)
	# lines(x,plogis(predict(mod)))
	# lines(x, prob, col="red")
	# plogis(sum(mod.out * c(1, -3.25, (-3.25)^2))) # probability predicted at x=-3.25
}
# blah <- t(sapply(S.dens.X, get.cov.params))
# apply(blah, 1, function(x)plogis(sum(x[1]+x[5]*x[2]+x[5]^2*x[3])))