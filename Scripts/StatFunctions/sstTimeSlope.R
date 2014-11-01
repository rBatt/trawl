sstTimeSlope <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		as.numeric(lm(x~I(0:(length(x)-1)))$coef[2])
	}
}

sstTimeSlope2 <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		lm(x~I(0:(length(x)-1)))$coef[2]
	}
}

sstTimeSlope3 <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		lm(x~I(1:length(x)))$coef[2]
	}
}

rsd <- function(x, ...){
	if(sum(!is.na(x))<3){
		NA
	}else{
		tvec <- seq_along(x)
		cor(x, tvec)*(sd(x, ...)/sd(tvec, ...))
	}
}

rsd2 <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		cor.na <- c("everything", "na.or.complete")[na.rm+1L]
		tvec <- seq_along(x)
		cor(x, tvec, use=cor.na)*(sd(x, na.rm=na.rm)/sd(tvec, na.rm=na.rm))
	}
}

rsd3 <- function(x, na.rm=TRUE){
	if(sum(!is.na(x))<3){
		NA
	}else{
		cor.na <- ifelse(na.rm, "na.or.complete", "everything")
		tvec <- seq_along(x)
		cor(x, tvec, use=cor.na)*(sd(x, na.rm=na.rm)/sd(tvec, na.rm=na.rm))
	}
}

rsd4 <- function(x, na.rm=TRUE){
	nona <- !is.na(x)
	if(sum(nona)<3){
		NA
	}else{
		# cor.na <- c("everything", "na.or.complete")[na.rm+1L]
		tvec <- seq_along(x)[nona]
		x2 <- x[nona]
		cor(x2, tvec)*(sd(x2)/sd(tvec))
		# cor(x, tvec, use=cor.na)*(sd(x, na.rm=na.rm)/sd(tvec, na.rm=na.rm))
	}
}

slopeSolve <- function(x, ...){
	if(sum(!is.na(x))<3){
		NA
	}else{
		# Beta = ((X'X)^-1)(X'Y)
		tvec <- cbind(1,seq_along(x)) # the 1 is a dummy vector for the intercept
		(solve(t(tvec)%*%tvec)%*%t(tvec)%*%x)[2]
	}
}

slopeSolve2 <- function(x, ...){
	if(sum(!is.na(x))<3){
		NA
	}else{
		# Beta = ((X'X)^-1)(X'Y)
		tvec <- cbind(1,seq_along(x)) # the 1 is a dummy vector for the intercept
		ttvec <- t(tvec)
		(solve(ttvec%*%tvec)%*%ttvec%*%x)[2]
	}
}

slopeSolve3 <- function(x, ...){
	nona <- !is.na(x)
	if(sum(nona)<3){
		NA
	}else{
		# Beta = ((X'X)^-1)(X'Y)
		tvec <- cbind(1,seq_along(x))[nona,] # the 1 is a dummy vector for the intercept
		ttvec <- t(tvec)
		(solve(ttvec%*%tvec)%*%ttvec%*%x[nona])[2]
	}
}



hybridSlope <- function(x, ...){
	nona <- !is.na(x)
	if(sum(nona)<3){
		NA
	}else{
		lx <- length(x)
		if(lx>=4E3){
			tvec <- (1:lx)[nona]
			x2 <- x[nona]
			cor(x2, tvec)*(sd(x2)/sd(tvec))
			
		}else{
			tvec <- cbind(1,1:lx)[nona,] # the 1 is a dummy vector for the intercept
			ttvec <- t(tvec)
			(solve(ttvec%*%tvec)%*%ttvec%*%x[nona])[2]
		}
	}
}



t1 <- rnorm(10)
f1 <- function()sstTimeSlope(t1)
f1.2 <- function()sstTimeSlope2(t1)
f1.3 <- function()sstTimeSlope3(t1)
f2 <- function()rsd(t1)
f3 <- function()slopeSolve(t1)
f3.2 <- function()slopeSolve2(t1)

library(microbenchmark)
microbenchmark(f1(), f1.2(), f1.3(), f2(), f3(), f3.2())
# Unit: microseconds
#    expr      min        lq    median       uq       max neval
#    f1() 1297.252 1337.2560 1379.3070 1448.349  2830.449   100
#  f1.2() 1299.273 1336.3785 1374.4590 1429.505  2510.129   100
#  f1.3() 1279.090 1304.4205 1336.5720 1402.000 35315.720   100
#    f2()   99.017  110.7990  116.0825  124.264   193.389   100
#    f3()  108.166  116.7830  121.5085  127.909   213.032   100
#  f3.2()   77.087   83.5565   89.9035   96.572   212.565   100
# OK, so that means that I'm not wasting much time on some of my random preferences. Actually, no time really went to 0:(length(x)-1) vs. 1:length(x), nor to as.numeric()
# Also, the solve() approach is the clear winner. Yay for matrix algebra. But that method won't handle NA's, and is barely faster than f2().

# Why is the lm() version so slow?
Rprof("profile.out")
for(i in 1:2E2) f1()
Rprof(NULL)
summaryRprof("profile.out")
# Yikes, it does a lot of stuff



# OK, test performance on the case there are NA's
set.seed(2)
t2 <- rnorm(10) + 2 + 0.1*seq_len(10)
t2[2] <- NA
z1 <- function()sstTimeSlope(t2)
z1.2 <- function()sstTimeSlope2(t2)
z1.3 <- function()sstTimeSlope3(t2)
z2 <- function()rsd(t2)
z2.2 <- function()rsd2(t2)
z2.3 <- function()rsd3(t2)
z2.4 <- function()rsd4(t2)
z3 <- function()slopeSolve(t2)
z3.2 <- function()slopeSolve2(t2)
z3.3 <- function()slopeSolve3(t2)

z1() # 0.2147202
z1.2() # 0.2147202 (but also with extra name)
z1.3() # 0.2147202 (but with name too)
z2() # NA
z2.2() # 0.2081118 # slightly wrong!
z2.3() # 0.2081118 # slightly wrong!
z2.4() # 0.2147202 # after seeing previous 2 wrong, I made this version. I was surprised previous 2 didn't work.
z3() # NA
z3.2() # NA
z3.3() # 0.2147202 

# so the 3 fastest performers from the previous microbenchmark are now disqualified b/c they can't handle NA's.
# Additionally, I'm apparently getting z3 slightly wrong here ... hmmm
# Let's eliminate them, and see how their revised versions perform:
microbenchmark(z1(), z1.2(), z1.3(), z2.2(), z2.3(), z2.4(), z3.3(), times=500)
# Unit: microseconds
#    expr      min        lq    median        uq       max neval
#    z1() 1327.492 1375.8940 1411.7305 1466.2210 13155.443   500
#  z1.2() 1329.775 1372.6305 1409.3705 1471.2390  9394.128   500
#  z1.3() 1301.406 1344.4265 1381.0880 1443.9895  2752.402   500
#  z2.2()  100.235  109.8365  115.0335  121.6885   231.511   500
#  z2.3()  114.874  123.8245  128.2995  135.0690   225.464   500
#  z2.4()  101.024  108.7990  113.3870  118.5960  7863.758   500
#  z3.3()   84.430   93.7885   99.9940  120.6745   192.005   500
# Each family of method retains its relative ranking: lm-based (z1) is slowest, z2 & z3 are similar to each other and much faster than z1, but the solve-based (z3) slightly outperforms the cor-sd-based (z2).




# One last test: How do these functions scale with the size of the data set? 
# It is nice that z.3.3() was the fastest, but it involves a potentially costly transpose.
# For the HadISST data set, the number of years is only 46, so this probably won't matter too much, as it's relatively short.
do.t3 <- function(size){
	set.seed(2)
	size.vec <- seq_len(size)
	t3 <- rnorm(size) + 2 + 3E-3*size.vec
	t3.na <- sample(x=size.vec, size=trunc(0.1*size), replace=FALSE)
	t3[t3.na] <- NA
	t3
}
t3.lengths <- list(1E1, 10^1.5, 46, 1E2, 10^2.5, 1E3, 10^3.5, 1E4, 10^4.5, 1E5, 10^5.5, 1E6)
t3 <- lapply(t3.lengths, do.t3)

r1 <- function(x)sstTimeSlope(t3[[x]])
r2.4 <- function(x)rsd4(t3[[x]])
r3.3 <- function(x)slopeSolve3(t3[[x]])

microbenchmark(r1(1), r2.4(1), r3.3(1), times=500) # the shortest
# Unit: microseconds
#     expr      min        lq    median        uq      max neval
#    r1(1) 1335.911 1369.6945 1396.6365 1454.5955 4048.594   500
#  r2.4(1)  101.362  109.6145  113.4735  117.1205 1320.741   500
#  r3.3(1)   84.657   90.4185   93.9610   98.4005 1223.347   500

microbenchmark(r1(3), r2.4(3), r3.3(3), times=200) # the length of the trawl time series, which is how long I'm doing HadISST for
# Unit: microseconds
#     expr      min        lq    median       uq       max neval
#    r1(3) 1376.519 1422.6435 1464.1235 1532.916 34825.157   200
#  r2.4(3)  109.116  118.2345  122.6305  128.064   172.361   200
#  r3.3(3)   90.415   97.6890  102.3700  111.709   248.481   200

microbenchmark(r1(12), r2.4(12), r3.3(12), times=5) # for a time series with 1 million observations (notice the difference!)
# Unit: milliseconds
#      expr       min        lq    median        uq       max neval
#    r1(12) 1838.4410 1993.6906 2050.4988 2270.0772 2569.5105     5
#  r2.4(12)  112.6363  112.9746  113.5735  119.8571  146.4474     5
#  r3.3(12)  121.0301  121.1725  124.6759  306.9554  352.4909     5


# OK, now create vector of timings
rO <- matrix(c(unlist(t3.lengths), rep(NA, length(t3)*3)), ncol=4, dimnames=list(NULL, c("size","r1","r2.4","r3.3")))
time.opt <- c(rep(200, 3), rep(100, 4), rep(50, 3), rep(5, 2))
for(i in 1:length(t3)){
	t.t3 <- t3[[i]]
	t.mb <- summary(microbenchmark(r1(i), r2.4(i), r3.3(i), times=time.opt[i], unit="ms"))[,"median"]
	rO[i, 2:4] <- t.mb
}

# dev.new()
png("~/Desktop/timeSlope_benchmarks.png", width=3.5, height=3.5, units="in", res=150)
# dev.new(width=3.5, height=3.5)
par(mar=c(1.75,2,0.1,0.1), ps=10, cex=1, mgp=c(1.25, 0.15, 0), tcl=-0.15, family="Times")
plot(log10(rO[,1]), log10(rO[,2]), ylim=range(log10(rO[,2:4])), type="o", pch=20, xaxt="n", yaxt="n", xlab="", ylab="")
mtext("Number of observations", side=1, line=0.75)
mtext("Computation time (milliseconds)", side=2, line=1.15)
axis(side=1, at=axTicks(1), labels=parse(text=paste(10,axTicks(1),sep="^")))
axis(side=2, at=axTicks(2), labels=parse(text=paste(10,axTicks(2),sep="^")))

lines(log10(rO[,1]), log10(rO[,3]), type="o", pch=20, col="red")
lines(log10(rO[,1]), log10(rO[,4]), type="o", pch=20, col="blue")

legend("topleft", legend=c("r1 (lm-based)","r2.4 (cor/sd-based)","r3.3 (solve-based)"), col=c("black","red","blue"), lty="solid", pch=20)
dev.off()
# As you can see, the cor/sd and solve methods are similar throughout. However, for longer time series (more than ~4000 observations), the cor/sd method begins outperforming the solve-based method.

# It's a close call between r.2.4 and r.3.3.
# r.2.4 is not the fastest at very small time series lengths, but the computation time here is small anyway. Thus, if I had to pick an overall best, it'd be r.2.4  b/c when you'll really notice the difference between it and r.3.3 is when the computation time are huge. So it seems like this is probably the best tool to carry around. On the other hand, slopeSolve is really good for doing a lot of short time series slopes. And this is what I'll be doing w/ the HadISST data set.

# So it seems like the appropriate next step is to see if a hybrid can outperform either on its own.
do.t4 <- function(size){
	set.seed(2)
	size.vec <- seq_len(size)
	t4 <- rnorm(size) + 2 + 3E-3*size.vec
	t4.na <- sample(x=size.vec, size=trunc(0.1*size), replace=FALSE)
	t4[t4.na] <- NA
	t4
}
t4.lengths <- list(1E1, 10^1.5, 46, 1E2, 10^2.5, 1E3, 10^3.5, 1E4, 10^4.5, 1E5, 10^5.5, 1E6)
t4 <- lapply(t4.lengths, do.t4)

h1 <- function(x)sstTimeSlope(t4[[x]])
h2.4 <- function(x)rsd4(t4[[x]])
h3.3 <- function(x)slopeSolve3(t4[[x]])
h4 <- function(x)hybridSlope(t4[[x]])


# OK, now create vector of timings
hO <- matrix(c(unlist(t4.lengths), rep(NA, length(t4)*4)), ncol=5, dimnames=list(NULL, c("size","h1","h2.4","h3.3","h4")))
time.opt <- c(rep(200, 3), rep(100, 4), rep(100, 3), rep(50, 2))
for(i in 1:length(t4)){
	t.t4 <- t4[[i]]
	t.mb <- summary(microbenchmark(h1(i), h2.4(i), h3.3(i), h4(i), times=time.opt[i], unit="ms"))[,"median"]
	hO[i, 2:5] <- t.mb
}

png("~/Desktop/timeSlope_benchmarks_plusHybrid.png", width=3.5, height=3.5, units="in", res=150)
# dev.new(width=3.5, height=3.5)
par(mar=c(1.75,2,0.1,0.1), ps=10, cex=1, mgp=c(1.25, 0.15, 0), tcl=-0.15, family="Times")
# plot(log10(hO[,1]), log10(hO[,2]), ylim=range(log10(hO[,2:5])), type="o", pch=20, xaxt="n", yaxt="n", xlab="", ylab="")
plot(log10(hO[,1]), log10(hO[,3]), ylim=range(log10(hO[,3:5])), type="o", pch=20, xaxt="n", yaxt="n", xlab="", ylab="")
mtext("Number of observations", side=1, line=0.75)
mtext("Computation time (milliseconds)", side=2, line=1.15)
axis(side=1, at=axTicks(1), labels=parse(text=paste(10,axTicks(1),sep="^")))
axis(side=2, at=axTicks(2), labels=parse(text=paste(10,axTicks(2),sep="^")))

# lines(log10(hO[,1]), log10(hO[,3]), type="o", pch=20, col="red")
lines(log10(hO[,1]), log10(hO[,4]), type="o", pch=20, col="red")
lines(log10(hO[,1]), log10(hO[,5]), type="o", pch=20, col="blue")

legend("topleft", legend=c("h2.4 (cor/sd-based)","h3.3 (solve-based)", "h4 (hybrid)"), col=c("black","red","blue"), lty="solid", pch=20)
dev.off()


# ==========================
# = AND THE WINNER IS .... =
# ==========================
# The hybrid!!! Minimal computational overhead associated with the ability to switch between the two models.

# hO
#               size          h1        h2.4        h3.3          h4
#  [1,] 1.000000e+01    1.459970   0.1208720   0.0950805   0.0959915
#  [2,] 3.162278e+01    1.477388   0.1233530   0.0968550   0.0993300
#  [3,] 4.600000e+01    1.411404   0.1174565   0.0958285   0.0977395
#  [4,] 1.000000e+02    1.537510   0.1299140   0.1036260   0.1050785
#  [5,] 3.162278e+02    1.617131   0.1457785   0.1219590   0.1237780
#  [6,] 1.000000e+03    2.196859   0.2263230   0.1940415   0.1945680
#  [7,] 3.162278e+03    3.808483   0.4574160   0.4165905   0.4226075
#  [8,] 1.000000e+04    9.723700   1.1603780   1.4299900   1.1490370
#  [9,] 3.162278e+04   33.059343   3.7131810   4.1516390   3.7280590
# [10,] 1.000000e+05  124.065382  12.4836770  13.4229180  12.5225245
# [11,] 3.162278e+05  508.039382  38.4219430  48.3931015  38.9670955
# [12,] 1.000000e+06 1996.177563 124.4935385 148.1341340 129.0245570












