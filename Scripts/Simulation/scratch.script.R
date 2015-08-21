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








comp.psi.method <- function(n.ref, samp.label){
	ref <- rnorm(n.ref)
	quants <- seq(-3, 3, by=0.1)
	psi.dnorm <- dnorm(quants)
	psi.dnorm <- psi.dnorm/dnorm(0) #max(psi.dnorm)
	psi.dsample <- dsample(ref, quants, relative=T)
	# plot(psi.dnorm, psi.dsample)
	# cor(psi.dnorm, psi.dsample)
	
	plot(quants,psi.dnorm, cex=0.5, ylab="",xlab="",ylim=c(0,1))
	lines(quants,psi.dsample, col="blue")
	
	if(samp.label){
		mtext(paste0("n=",n.ref), side=3, line=-0.1, adj=0.05, font=2)
	}else{
	}
	
}

# nsamps <- rep(seq(50,100,by=2), each=40)
# comps <- mapply(comp.psi.method, nsamps)
# plot(nsamps, comps)
# boxplot(comps~nsamps)

dev.new(height=7, width=8)
par(mfrow=c(5,5), mar=c(1.5,1.5,0.5,0.1), mgp=c(0.5,0.1,0), tcl=-0.1, ps=6, cex=1)
nsamps <- rep(c(10,40,80,200,1E3), each=5)
samp.label <- !duplicated(nsamps)

invisible(mapply(comp.psi.method, nsamps, samp.label))




# ===============================
# = to go along with rich.cov.R =
# ===============================
	# mus <- fit.cov$BUGSoutput$mean
	# t.range <- range(covs[[1]], na.rm=TRUE)
	# t.grad <- seq(1, 10, by=0.25)
	# for(i in 1:length(mus$a1)){
	# 	resp <- mus$u.a0[i] + mus$a1[i]*t.grad + mus$a1[i]*t.grad
	# 	plot(t.grad, resp)
	# }
	
	# out <- list(mean=fit.cov$BUGSoutput$mean, median=fit.cov$BUGSoutput$median, sd=fit.cov$BUGSoutput$sd)
	# out <- list(mean=fit.cov$BUGSoutput$mean, BUGSoutput=fit.cov$BUGSoutput)

	# library(ggmcmc)
# 	samples.binary <- coda.samples(fit.cov$model, c("w","Z"), 100)
# 	samples.cont <- coda.samples(fit.cov$model, c("u.a0","a3","a4", "psi","v.a0","p"), 100)
# 	samples.binary.Z <- ggs(samples.binary, family="Z")
# 	samples.cont.2 <- ggs(samples.cont)
# 	Z.outcome <- c(t(apply(Xaug1, c(1,3), max, na.rm=TRUE)))
# 	ggmcmc(samples.cont.2, "~/Desktop/test.pdf", width=12, height=12, param_page=5)
# 	Z.roc <- ggs_rocplot(samples.binary.Z, outcome=Z.outcome)
#
# 	sims <- fit.cov$BUGSoutput$sims.matrix
#
# 	# add mu.psi to sims
# 	w.ind <- grepl("w\\[.*", colnames(sims))
# 	psi.ind <- grepl("psi.*", colnames(sims))
# 	mu.psi <- rep(sims[,w.ind],each=grid.w*grid.h)*sims[,psi.ind]
# 	colnames(mu.psi) <- paste0("mu.",colnames(mu.psi))
#
# 	# add mu.p to sims
# 	# TODO Need to finish this – computer crashed when i did something stupid; need to check indices on p and Z to do this calculation correctly; i know the repeating isn't right
# 	Z.ind <- grepl("Z\\[.*", colnames(sims))
# 	p.ind <- grepl("p\\[.*", colnames(sims))
# 	mu.p <- rep(sims[,Z.ind],each=grid.w*grid.h)*sims[,p.ind]
# 	colnames(mu.p) <- paste0("mu.",colnames(mu.p))
#
# 	out.mode <- apply(sims, 2, mode)
# 	out.med <- apply(sims, 2, median)
# 	out.mean <- apply(sims, 2, mean)
#
#
# 	centrals <- data.frame(mode=out.mode, med=out.med, mean=out.mean)
#
#
# 	mu.psi <-
# 	centrals.psi <- cbind(centrals[psi.ind,][1:(ns*grid.w*grid.h),], true.mean=c(psi.true[,,1,1]))
# 	centrals.w <- centrals[w.ind,]
# 	pairs(centrals.psi, panel=function(x,y,...){points(x,y, ...);abline(a=0,b=1)}, pch=20, cex=0.5, col=adjustcolor("black",alpha.f=0.2))
#
# 	table(gsub("\\[.*\\]", "", colnames(sims))) # number of parameters per main parameter
#