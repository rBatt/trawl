


# =============================
# = Mess around with plotting =
# =============================

ts.length <- trawl[,list(ts.length=length(unique(year))), by=c("spp","s.reg")]
setkey(ts.length, spp, s.reg)
ts.long <- ts.length[ts.length>=25, list(spp, s.reg)]

setkey(trawl, spp, s.reg)
long.trawl <- trawl[ts.long,]
long.uspp <- unique(long.trawl[,spp])

n.reg <- trawl[j=list(n.reg.per.spp=length(unique(s.reg))), by=spp]

mfrow.opts <- matrix(c(1,2,3,2,3,1,1,1,2,2), ncol=2)
mfrow.size <- mfrow.opts[,1]*mfrow.opts[,2]

pdf.start <- "/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Figures/TSfigs/"
for(i in 1:length(long.uspp)){
	t.n.reg <- n.reg[long.uspp[i]][,n.reg.per.spp]
	
	mfrow.choice <- which(mfrow.size>=t.n.reg)[1]
	
	switch(mfrow.choice,
	 	{ # 1
			# dev.new(width=3, height=2.5)
			pdf(paste(pdf.start, long.uspp[i], ".pdf", sep=""), width=3, height=2.5)
			par(mfrow=c(1,1), mar=c(1, 1, 1, 1), oma=c(0, 1, 1, 1), tcl=-0.15, mgp=c(0.75, 0, 0), ps=8, family="Times", cex=1)
		},
		
		{ # 2
			# dev.new(width=3, height=4.5)
			pdf(paste(pdf.start, long.uspp[i], ".pdf", sep=""), width=3, height=4.5)
			par(mfrow=c(2,1), mar=c(1, 1, 1, 1), oma=c(0, 1, 1, 1), tcl=-0.15, mgp=c(0.75, 0, 0), ps=8, family="Times", cex=1)
		},
		
		{ # 3
			# dev.new(width=3, height=6)
			pdf(paste(pdf.start, long.uspp[i], ".pdf", sep=""), width=3, height=6)
			par(mfrow=c(3,1), mar=c(1, 1, 1, 1), oma=c(0, 1, 1, 1), tcl=-0.15, mgp=c(0.75, 0, 0), ps=8, family="Times", cex=1)
		},
		
		{ # 4
			# dev.new(width=5, height=4.5)
			pdf(paste(pdf.start, long.uspp[i], ".pdf", sep=""), width=5, height=4.5)
			par(mfrow=c(2,2), mar=c(1, 1, 1, 1), oma=c(0, 1, 1, 1), tcl=-0.15, mgp=c(0.75, 0, 0), ps=8, family="Times", cex=1)
		},
		
		{ # 5
			# dev.new(width=5, height=6)
			pdf(paste(pdf.start, long.uspp[i], ".pdf", sep=""), width=5, height=6)
			par(mfrow=c(3,2), mar=c(1, 1, 1, 1), oma=c(0, 1, 1, 1), tcl=-0.15, mgp=c(0.75, 0, 0), ps=8, family="Times", cex=1)
		}
	)
	
	trawl[
		long.uspp[i], list(cntcpue=sumna(cntcpue), wtcpue=sumna(wtcpue)), by=c("year","s.reg")
		][,{
			if(any(!is.na(cntcpue))){
				plot(year, cntcpue, type="o", ylab="", pch=20, xlab="")
			}
			if(any(!is.na(cntcpue))&any(!is.na(wtcpue))){par(new=TRUE)}
			if(any(!is.na(wtcpue))){
				plot(year, wtcpue, type="o", ylab="", pch=20, xlab="", xaxt="n", yaxt="n", col="red")
				axis(side=4, col="red", col.axis="black")
			}
			# legend("topleft", legend=s.reg)
			mtext(s.reg, side=3, line=0, adj=0)
			},
		by="s.reg"
		]
	mtext(long.uspp[i], side=3, outer=TRUE, line=-0.25, cex=2, font=3)
	mtext("Count CPUE", side=2, outer=TRUE, line=0, cex=1, adj=NA)
	mtext("Weight CPUE", side=4, outer=TRUE, line=0, cex=1, adj=NA, col="red")
	
	dev.off()

}


