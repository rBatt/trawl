
summary.jags <- function(x, conv.names=c(""), doPlot=TRUE, doPanels=TRUE){
	x.sims <- x$BUGSoutput$sims.list
	n.sim <- length(x.sims)
	x.modes <- lapply(x.sims, function(x)get.modes(x))
	
	if(doPlot){
		names.x <- names(x.sims)
		if(doPanels){
			use.mfr <- auto.mfrow(length(x.sims))
			par(mfrow=use.mfr)
		}
		for(i in 1:length(x.sims)){
			t.name <- names.x[i]
			if(t.name%in%conv.names){
				t.x <- anti.logit(x.sims[[i]])
			}else{
				t.x <- x.sims[[i]]
			}
			plot.post(t.x, xlab=t.name, main="")
		}
	}
	
	invisible(c(x.modes, list(list(jags.out=x))))

}