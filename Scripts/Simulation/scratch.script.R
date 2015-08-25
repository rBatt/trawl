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



# =======================
# = From sim.spp.proc.R =
# =======================

psiMod <- function(b0, b3, b4, X, n){

	if(missing(X)){
		if(missing(n)){
			n <- 100
		}
		X <- seq(1,30, length.out=n)
		cbind(X=X, psi=plogis(b0+b3*X+b4*X^2))
	}else{
		list(x=X, y=plogis(b0 + b3*X + b4*X^2))
	}
	
}

psi.opt <- function(b1,b2){-b1/(2*b2)}
psi.tol <- function(b2){1/sqrt(-2*b2)}
psi.max <- function(b0,b1,b2){1/(1+exp((b1^2)/(4*b2)-b0))}



# parent means
mu.u.a0 <- 0.5
mua3 <- 0.00
mua4 <- -0.025

# precisions (all species share a precision)
# I'm additionally assuming all of these parameters have the same
# precision, but that might not be true
# (this constraint does not exist in the msom)
tau.u.a0 <- 1/0.5^2
tau.a3 <- 1/0.21^2
tau.a4 <- 1/0.008^2

cov.a0.a3 <- -(0.05^2)
cov.a0.a4 <- -(0.05^2)
cov.a3.a4 <- -(0.01^2)


cov.mat.use <- matrix(
	c(	1/tau.u.a0,	cov.a0.a3,	cov.a0.a4,
		cov.a0.a3,	1/tau.a3,	cov.a3.a4, 
		cov.a0.a4,	cov.a3.a4,	1/tau.a4), 
	ncol=3
)


# species-specific means of logistic regression parameters
u.a0 <- rnorm(ns, mu.u.a0, sqrt(1/tau.u.a0))
a3 <- rnorm(ns, mua3, sqrt(1/tau.a3)) #~ dnorm(0, 0.001)

a4 <- rnorm(ns, mua4, sqrt(1/tau.a4)) #~ dnorm(0, 0.001)
#
# a.out <- mvrnorm(ns, mu=c(mu.u.a0, mua3, mua4), Sigma=cov.mat.use, empirical=TRUE)
# u.a0 <- a.out[,1]
# a3 <- a.out[,2]
# a4 <- a.out[,3]

(cov.mat.obs <- cov(matrix(c(u.a0,a3,a4), ncol=3)))
# (cov.mat.true <- diag(c(1/tau.u.a0, 1/tau.a3, 1/tau.a4)))
cov.mat.use



range.X <- range(values(grid.X))
Xvals <- do.call("seq",c(as.list(range.X),list(length.out=500)))
S.dens.X <- mapply(psiMod, b0=u.a0, b3=a3, b4=a4, MoreArgs=list(X=Xvals), SIMPLIFY=F)
p.suit2 <- simplify2array(mapply(function(...)psiMod(...)$y, u.a0, a3, a4, MoreArgs=list(X=values(subset(grid.X, 1))), SIMPLIFY=F)) # this can now go to colonize() instead of having to rely on dsample(); I tested, and it shouldn't matter much provided that a large enough reference sample size is used.



mua34 <- expand.grid(sort(a3),  sort(a4))
mu.psi.max <- psi.max(mu.u.a0, mua34[,1], mua34[,2])
mu.psi.opt <- psi.opt(mua34[,1], mua34[,2])
# image.plot(matrix(mu.psi.max, nrow=length(a3), byrow=TRUE, dimnames=list(sort(a3), sort(a4))), zlim=0:1)

par(mfrow=c(2,2))

image.plot(x=sort(a3), y=sort(a4), z=matrix(mu.psi.max, nrow=length(unique(mua34[,1])), byrow=F), zlim=0:1)
points(a3,a4, pch=20)
abline(v=c(-0.5, 0.5))
abline(h=-0.04)
image.plot(x=sort(a3), y=sort(a4), z=matrix(mu.psi.opt, nrow=length(unique(mua34[,1])), byrow=F), zlim=c(-15,15))
points(a3,a4, pch=20)
abline(v=c(-0.5, 0.5))
abline(h=-0.04)

# Plot response curves that were just generated:
plot(S.dens.X[[1]], ylim=0:1, type="l", col=adjustcolor("black",alpha.f=0.25))
invisible(sapply(S.dens.X[-1], lines, col=adjustcolor("black",alpha.f=0.25)))
lines(S.dens.X[[1]]$x, apply(simplify2array(lapply(S.dens.X, function(x)x$y)), 1, mean), lwd=3)

a.shapes <- data.frame(u.a0=u.a0, a3=a3, a4=a4,psi.max=psi.max(u.a0, a3, a4), psi.opt=psi.opt(a3, a4), psi.tol=psi.tol(a4))
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

pairs(a.shapes, diag.panel=panel.hist)


# ===========================================================
# = trying to find a good combination of parameters for psi =
# ===========================================================
findGoodPsi <- function(pars){
	Nsim <- 1E3
	
	a0 <- pars[1]
	a1 <- pars[2]
	a2 <- pars[3]
	tau0 <- pars[4]
	tau1 <- pars[5]
	tau2 <- pars[6]
	
	if(any(c(tau0,tau1,tau2)<=0)){
		return(999)
	}
	
	u.a0.o <- rnorm(Nsim, a0, sqrt(1/tau0))
	a1.o <- rnorm(Nsim, a1, sqrt(1/tau1)) #~ dnorm(0, 0.001)
	a2.o <- rnorm(Nsim, a2, sqrt(1/tau2)) #~ dnorm(0, 0.001)
	
	mu.psi.max <- psi.max(u.a0.o, a1.o, a2.o)
	mu.psi.opt <- psi.opt(a1.o, a2.o)
	
	if(sum(mu.psi.opt<(-1) | mu.psi.opt>30)>(0.01 * Nsim)){
		# return((9 * sum(mu.psi.opt<(-1) | mu.psi.opt>30)))
		return(999)
	}
	if(sum(mu.psi.max>=0.95 | mu.psi.max <= 0.25)>(0.01 * Nsim)){
		return(999)
	}
	if(sum(a2.o >=0) > (0.01 * Nsim)){
		return(999)
	}
	# if(!all((c(5:25)%in%round(mu.psi.opt,0)))){
	# 	(c(5:25)%in%round(mu.psi.opt,0))
	# 	return(9999)
	# }
	
	# opt1 <- mu.psi.opt[mu.psi.opt>0]
	# opt2 <- abs(mu.psi.opt[mu.psi.opt<=0])+1E-3
	ryanHappinessIndex <- -var(mu.psi.opt) + -entropy.empirical(mu.psi.opt) - sum(c(5:25)%in%round(mu.psi.opt,0)) + sd(psi.tol(a2.o))^2 # + -sd(mu.psi.max) #-entropy.empirical(mu.psi.opt) + -entropy.empirical(mu.psi.max)
	# ryanHappinessIndex <- -entropy.empirical(opt1) + -entropy.empirical(opt2) + -entropy.empirical(mu.psi.max)
	# + (mean(opt1)-mean(opt2))/2
	return(ryanHappinessIndex)
}
findGoodPsi(c(mu.u.a0, mua3, mua4, tau.u.a0, tau.a3, tau.a4))
happyPars <- optim(c(mu.u.a0, mua3, mua4, tau.u.a0, tau.a3, tau.a4), findGoodPsi, control=list(maxit=1E6))
# c(mu.u.a0, mua3, mua4, tau.u.a0, tau.a3, tau.a4),
domains <- matrix(c(-50,0, -10,10, -10,10, 1E-5,1E4, 1E-1,1E4, 1E-1,1E4), ncol=2, byrow=TRUE)
happyPars <- genoud(findGoodPsi,  6, Domains=domains, pop.size=1E4)$par

speciesPars <- function(pars){
	# species-specific means of logistic regression parameters
	u.a0 <- rnorm(ns, pars[1], sqrt(1/pars[4]))
	a3 <- rnorm(ns, pars[2], sqrt(1/pars[5])) #~ dnorm(0, 0.001)
	a4 <- rnorm(ns, pars[3], sqrt(1/pars[6])) #~ dnorm(0, 0.001)
	
	list(mu.u.a0 = pars[1], mua3 = pars[2], mua4 = pars[3], tau.u.a0 = pars[4], tau.a3 = pars[5], tau.a4 = pars[6], u.a0=u.a0, a3=a3, a4=a4)
}

speciesPars.out <- speciesPars(very.best) #speciesPars(happyPars$par)
mu.u.a0 <- speciesPars.out$mu.u.a0
mua3 <- speciesPars.out$mua3
mua4 <- speciesPars.out$mua4
tau.u.a0 <- speciesPars.out$tau.u.a0
tau.a3 <- speciesPars.out$tau.a3
tau.a4 <- speciesPars.out$tau.a4
u.a0 <- speciesPars.out$u.a0
a3 <- speciesPars.out$a3
a4 <- speciesPars.out$a4


