
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