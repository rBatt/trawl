
# ========================================
# = Alpha as diversity turnover over time =
# ========================================
heat.cols <- colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", "#FCFF00", "#FF9400", "#FF3100"))(256)

alpha.turn.time[,turn.time.col:=heat.cols[cut(turn.time, 256)]]

# dev.new(height=4, width=alpha.turn.time[,map.w(lat,lon,4)])
pdf(height=4, width=alpha.turn.time[,map.w(lat,lon,4)], file="~/Documents/School&Work/pinskyPost/trawl/Figures/alphaD_temporalTurnover.pdf")
par(mar=c(1.75,1.5,0.5,0.5), oma=c(0.1,0.1,0.1,0.1), mgp=c(0.85,0.05,0), tcl=-0.15, ps=8, family="Times", cex=1, bg="lightgray")

alpha.turn.time[,plot(lon, lat, col=turn.time.col, pch=21, cex=1, type="n")]
invisible(alpha.turn.time[,map(add=TRUE, fill=FALSE, col="black")])

alpha.turn.time[,points(lon, lat, col=turn.time.col, pch=21, cex=1)]

alpha.turn.time[,segments(x0=-165, x1=-160, y0=seq(30,40,length.out=256), col=heat.cols)]

alpha.turn.time[,segments(x0=-166, x1=-165, y0=seq(30,40, length.out=4), col="black")] # tick marks
alpha.turn.time[,text(-167, y=seq(30,40, length.out=4), round(exp(seq(min(turn.time), max(turn.time), length.out=4)),4), adj=1, cex=1, col="black")]

#checking to make sure I get colors right
# dev.new(); alpha.turn.time[,plot(turn.time, col=turn.time.col)] # a plot of all the variances, with their colors
# alpha.turn.time[,quantile(1:256, probs=seq(0,1,length.out=4))] # this gives the indices of heat.cols where tick marks are located
# alpha.turn.time[,abline(h=round(seq(min(turn.time), max(turn.time), length.out=4),2), col=heat.cols[c(1,86,171,256)])] # these lines should match the colors through which they're drawn

alpha.turn.time[,text(-162.5, 41.5, bquote(alpha~diversity~over~time))]
dev.off()