


dev.new.lf <- function(devName="default", fileName=NULL){
	if(!is.null(fileName)){
		
		
		if(grepl("/",fileName)){ # if it looks like the file name includes a directory, 
			png("~/Desktop/communityShift_vs_surfTempShift.png", width=3.5, height=6, res=300, units="in") # then just save it where told
		}else{
			png(paste("~/Desktop/",fileName,sep=""), width=3.5, height=6, res=300, units="in") # otherwise put on Desktop
		}
		
		
	}else{
		dev.new(width=3.5, height=6.5)
	}
	
	
	par(mfrow=c(5,2), mar=c(0.85,0.85,0.75,0.5), oma=c(0.75,0.75,0.1,0.1), mgp=c(0.75,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1)
	assign(devName, dev.cur(), envir=.GlobalEnv)
}










