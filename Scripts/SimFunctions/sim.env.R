
sim.env <- function(grid.w=5, grid.h=7, grid.t=12, X.slope=0.75){
	
	Nv <- (grid.w*grid.h) # Number of vertices
	
	
	# =====================
	# = Create Blank Grid =
	# =====================
	# Create Blank Grid
	grid.blank <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)
	
	
	# ========================================
	# = Create Temperature Grid/ Time Series =
	# ========================================
	# Generate Grid Temperature
	X.h <- seq(-10, 0, length.out=grid.h)
	X <- c()
	for(i in 1:grid.w){
		X <- c(X, X.h+rnorm(grid.h))
	}

	# Insert first year of temperature grid
	grid.X.1 <- gen.grid(X, dims=c(grid.h, grid.w, 1), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

	# Create time series of temperature grid
	grid.X <- grid.blank
	grid.X <- setValues(grid.X, values(grid.X.1), layer=1)

	rg <- function(){ # function to add noise to the temperature process
		rnorm(n=Nv, sd=0.1)
	}
	for(i in 2:grid.t){ # loop through years adding noise
		grid.X <- setValues(grid.X, values(subset(grid.X, i-1))+rg()+X.slope, layer=i)
	}
	
	return(grid.X)
	
}


