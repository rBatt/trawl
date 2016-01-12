#' Moments of Trophic Structure
#' 
#' Trophic structure is (or is at least analagous to) a probability distribution. Therefore, key aspects of trophic structure can be summarized through summary statistics that are conceptually similar to statistical moments. The first through third moments (mean, variance, skewness) have interpretations for trophic structure that are similar to those in statistics. The fourth moment of trophic structure, bulge, except that low (negative) bulge extends to indicate bimodality (whereas low skewness is simply a flat distribution).
#' 
#' @param anomaly A weighting factor for each trophic group
#' @param tau The trophic level associated with each trophic group
#' 
#' @details
#' The lengths of \code{anomaly} and \code{tau} should each be equal to the number of trophic groups.
#' 
#' Trophic mean and variance are calculated as a \code{weighted.mean} where the smallest anomaly has been subtracted from all other anomalies. Trophic variance is calculate similarly. Trophic skew is the slope of a regression of \code{anomaly~tau}; trophic bulge is similar, except it mirrors tau about the mean, such that the slope represents average increase as the mean is approached from either extreme.
#' 
#' Non-zero trophic skewness indicates a top- or bottom-heavy trophic structure (positive is top heavy). If trophic skewness is 0, then trophic bulge is needed to describe trohpic shape. A bulge of 0 is a rectangle, a negative bulge is an hourglass, and a positive bulge is a diamond.
#' 
#' @examples
#' N <- 5
#' 
#' # rectangle
#' # Should have no skew, no bulge
#' rec <- rep(1, N)
#' skew(rec, 1:N)
#' #        slope      slope_se      slope_t      slope_p
#' # 5.551115e-17 5.270150e-16 1.053313e-01 9.227607e-01
#' bulge(rec, 1:N)
#' #         slope       slope_se       slope_t       slope_p
#' # -5.551115e-17  1.915317e-16 -2.898275e-01  7.908257e-01
#' 
#' # downward-pointing triangle
#' # Should have positive skew, no bulge
#' dpt <- 1:N
#' skew(dpt, 1:N)
#' #        slope      slope_se      slope_t      slope_p
#' # 1.000000e+00 1.948012e-15 5.133439e+14 0.000000e+00
#' bulge(dpt, 1:N)
#' #         slope       slope_se       slope_t       slope_p
#' # -2.220446e-16  1.091089e+00 -2.035072e-16  1.000000e+00
#' 
#' # upward-pointing triangle (noiser)
#' # Should have negative skew, no bulge
#' upt <- rev(c(1, 2.1, 2.8, 4.2, 4.6))
#' skew(upt, 1:N)
#' #        slope       slope_se       slope_t       slope_p
#' # -0.930000000   0.078102497 -11.907429834   0.001273791
#' bulge(upt, 1:N)
#' #      slope    slope_se    slope_t    slope_p
#' # 0.05000000 1.02498548 0.04878118 0.96415965
#' 
#' # diamond
#' # Should have no skew, positive bulge
#' dia <- c(1,2,3,2,1)
#' skew(dia, 1:N)
#' #        slope      slope_se      slope_t      slope_p
#' # 5.551115e-17 3.055050e-01 1.817029e-16 1.000000e+00
#' bulge(dia, 1:N)
#' #        slope      slope_se      slope_t      slope_p
#' # 1.000000e+00 2.422705e-16 4.127617e+15 0.000000e+00
#' 
#' # hourglass (wasp-waist)
#' # should have no skew, negative bulge
#' hou <- c(3,2,1,2,3)
#' skew(hou, 1:N)
#' #        slope      slope_se      slope_t      slope_p
#' # 1.110223e-16 3.055050e-01 3.634058e-16 1.000000e+00
#' bulge(hou, 1:N)
#' #         slope       slope_se       slope_t       slope_p
#' # -1.000000e+00  6.601594e-16 -1.514786e+15  0.000000e+00
#' 
#' # Benchmark
#' \donttest{
#' library(microbenchmark)
#' f1 <- function() summary(lm(hou~1:N))
#' f2 <- function() skew(hou, 1:N)
#' f3 <- function() bulge(hou, 1:N)
#' }
#' 
#' @export
"trophic_moment"


#' @describeIn t_moments Trophic mean
t_mean <- function(anomaly, tau){
	weighted.mean(tau, anomaly-min(anomaly))
}


#' @describeIn t_moments Trophic variance
t_var <- function(anomaly, tau){
	wt <- anomaly - min(anomaly)
	xm <- weighted.mean(tau, wt)
	sum(wt * (tau - xm)^2)
}


#' @describeIn t_moments Trophic skew
skew <- function(anomaly, tau){
	N <- length(tau)
	X <- unname(cbind(1, tau))
	XpX1 <- solve(t(X)%*%X)
	b <- XpX1%*%t(X)%*%anomaly
	
	r <- (X%*%b - anomaly)
	s2 <- (t(r)%*%r)/(N-2)
	b_se <- sqrt(diag(s2[1]*XpX1))
	
	t_stats <- b[1:2] / b_se
	p_val <- (1 - pt(abs(t_stats), N-2))*2
	
	c('slope'=b[2], 'slope_se'=b_se[2], 'slope_t'=t_stats[2], 'slope_p'=p_val[2])
	
}


#' @describeIn t_moments Trophic bulge
bulge <- function(anomaly, tau){
	N <- length(tau)
	X <- unname(cbind(1, -abs(tau-mean(tau))))
	XpX1 <- solve(t(X)%*%X)
	b <- XpX1%*%t(X)%*%anomaly
	
	r <- (X%*%b - anomaly)
	s2 <- (t(r)%*%r)/(N-2)
	b_se <- sqrt(diag(s2[1]*XpX1))
	
	t_stats <- b[1:2] / b_se
	p_val <- (1 - pt(abs(t_stats), N-2))*2
	
	c('slope'=b[2], 'slope_se'=b_se[2], 'slope_t'=t_stats[2], 'slope_p'=p_val[2])
}




 