
library(rfishbase)
library(data.table)
library(png)

# ===================
# = Load trawl data =
# ===================
load("/Users/Battrd/Documents/School&Work/pinskyPost/trawl/Data/trawl.RData")
data(fishbase)


# =================
# = PNG Functions =
# =================
addPNG <- function(ping, xl, xr, yt){
	p.dim <- dim(ping)
	# din <- par("din")
	usr <- par("usr")
	# fig <- par("fig")
	fin <- par("fin")
	# mai <- par("mai")

	# region.width <- fin[2] - (mai[2] + mai[4])
	# region.height <- fin[1] - (mai[1] + mai[3])

	fig.width <- usr[2] - usr[1]
	fig.height <- usr[4] - usr[3]

	
	# hOw <- (p.dim[1]/p.dim[2]) * (fig.height/fig.width)
	# hOw <- (p.dim[1]/p.dim[2]) * (fig.height/fig.width) * ((fig[2]-fig[1])/(fig[4]-fig[3]))
	# hOw <- (p.dim[1]/p.dim[2]) * (fig.height/fig.width) * (region.height/region.width)
	hOw <- (p.dim[1]/p.dim[2]) * (fig.height/fig.width) * (fin[1]/fin[2])
	
	xw <- xr-xl
	yh <- xw*hOw
	yb <- yt - yh
		
	# (yt-yb) / xw
	
	rasterImage(ping, xleft=xl, ybottom=yb, xright=xr, ytop=yt)
	
}



# ==================================
# = Grab the data for each species =
# ==================================

setkey(trawl, spp)
uspp <- trawl[,unique(spp)]
data.fb <- list()
for(i in seq_along(uspp)){
	if(trawl[uspp[i],isSpecies, mult="first"]){
		next
	}else{
		data.fb[[i]] <- fish.data[which_fish(uspp[i], "ScientificName", fish.data=fish.data)]
	}
}

n.match.fb <- sapply(data.fb, length) # the number of matches retrieved from Fishbase for each spp
# na.match.fb <- sapply(data.fb, function(x)all(is.na(x)))

# fb.png <- list()
for(i in seq_along(uspp)){
	if(n.match.fb[i]!=1L){
		next
	}else{
		if(!is.na(data.fb[[i]])){
			t.tl <- getTrophicLevel(data.fb[[i]])
			trawl[uspp[i], tl:=t.tl]
		}
				
	}
}


getTrophicLevel(fish.data[which_fish("Gadus", "ScientificName", fish.data=fish.data)])





