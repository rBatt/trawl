


dev.new.lf <- function(devName="default", fileName=NULL){
	if(!is.null(fileName)){
		
		
		if(grepl("/",fileName)){ # if it looks like the file name includes a directory, 
			png(fileName, width=3.5, height=6.5, res=300, units="in") # then just save it where told
		}else{
			png(paste("~/Desktop/",fileName,sep=""), width=3.5, height=6.5, res=300, units="in") # otherwise put on Desktop
		}
		
		
	}else{
		dev.new(width=3.5, height=6.5)
	}
	
	
	par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=9, family="Times", cex=1, cex.main=0.85)
	assign(devName, dev.cur(), envir=.GlobalEnv)
}


dev.new.lf2 <- function(devName="default", fileName=NULL){
	if(!is.null(fileName)){
		
		
		if(grepl("/",fileName)){ # if it looks like the file name includes a directory, 
			png(fileName, width=3.5, height=6, res=300, units="in") # then just save it where told
		}else{
			png(paste("~/Desktop/",fileName,sep=""), width=3.5, height=6, res=300, units="in") # otherwise put on Desktop
		}
		
		
	}else{
		dev.new(width=3.5, height=6)
	}
	
	
	par(mfrow=c(2,1), mar=c(2,2,0.75,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=12, family="Times", cex=1, cex.main=0.85)
	assign(devName, dev.cur(), envir=.GlobalEnv)
}








abline.mod <- function(x,y){
	abmod <- lm(y~x)
	# Plot Lines
	if(!is.na(abmod$coef[2])){
		pval <- summary(abmod)$coef[2,4]
		if(pval<0.05){
			abline(abmod, col="red", lwd=2)
		}else{
			abline(abmod, col="red", lwd=0.5)
		
		}
		if(pval<0.005){
			abline(abmod, col="red", lwd=2)
			abline(abmod, col="white", lty="dashed", lwd=1)
		}
	}
}








