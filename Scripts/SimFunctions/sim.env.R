
sim.env <- function(grid.w=5, grid.h=7, grid.t=12, X.slope=0.75){
	stopifnot(require(raster))
	
	Nv <- (grid.w*grid.h) # Number of vertices
	
	
	# =====================
	# = Create Blank Grid =
	# =====================
	# Create Blank Grid
	grid.blank <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)
	
	
	# ========================================
	# = Create Environment Grid/ Time Series =
	# ========================================
	# Create latitudinal (grid.h) gradient in environmental variable for 1st year (1 longitude) 
	# X.h <- seq(from=-(grid.t-1)*X.slope, by=(grid.t*X.slope)/(grid.h-1), length.out=(grid.h-1))
	ampSpatial <- 4
	h.slope <- pmax(0.75*(12/grid.t), X.slope)
	h.from <- -(grid.t-1)*(h.slope*ampSpatial/2)
	h.by <- (grid.t*h.slope*ampSpatial)/(grid.h-1)
	# X.h <- seq(from=h.from, by=h.by, length.out=grid.h)
	X.h <- seq(from=-9, to=9, length.out=grid.h)
	
	# Create env values at all longitudes by adding noise to default lat gradient
	X <- c()
	for(i in 1:grid.w){
		X <- c(X, X.h+rnorm(grid.h, sd=1))
	}

	# Insert first year of environment grid
	grid.X.1 <- gen.grid(X, dims=c(grid.h, grid.w, 1), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

	# Create time series of environment grid
	grid.X <- grid.blank
	grid.X <- setValues(grid.X, values(grid.X.1), layer=1)

	rg <- function(){ # function to add noise to the environment process
		rnorm(n=Nv, sd=0.1)
	}
	for(i in 2:grid.t){ # loop through years adding noise
		grid.X <- setValues(grid.X, values(subset(grid.X, i-1))+rg()+X.slope, layer=i)
	}
	
	return(grid.X)
	
}


