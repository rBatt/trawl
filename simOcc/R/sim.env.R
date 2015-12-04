#' Simulate Environment
#' 
#' Simulates the environmental gradient
#' 
#' @param grid.w width of environmental grid (e.g., columns, longitude)
#' @param grid.h height of environmental grid (e.g., rows, latitude)
#' @param grid.t number of time steps for grid
#' @param X.slope the rate of change for the environmental variable across \code{grid.t}
#' @param h.slope increase in the environmental gradient per grid cell, going up
#' @param half.temp value of the environmental variable at the vertically middle grid cell in the middle time step
#' @param w.sd stanard deviation of environmental variable across the columns (\code{grid.w})
#' @param t.sd standard deviation of environmental variable between time steps
#' 
#' @details
#' Also introduces a small amount of noise to the temporal regression. Assumes that the environmental gradient varies across rows (\code{grid.h}), and adds noise within a row across columns. Increasing \code{h.slope} while holding \code{grid.h} constant will increase the range of values observed in the environmental variable; same is true for \code{X.slope} and \code{grid.t}.
#' 
#' @import raster
#' @export
sim.env <- function(grid.w=5, grid.h=7, grid.t=12, X.slope=0, h.slope=1, half.val=0, w.sd=0.25, t.sd=0.1){
	
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
	h <- (grid.h-1)/2
	time <- (grid.t-1)/2
	h.from <- -(h*h.slope + time*X.slope - half.val)
	X.h <- seq(from=h.from, by=h.slope, length.out=grid.h)
	
	# Create env values at all longitudes by adding noise to default lat gradient
	X <- c()
	for(i in 1:grid.w){
		X <- c(X, X.h+rnorm(grid.h, sd=w.sd))
	}

	# Insert first year of environment grid
	grid.X.1 <- gen.grid(X, dims=c(grid.h, grid.w, 1), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

	# Create time series of environment grid
	grid.X <- grid.blank
	grid.X <- setValues(grid.X, values(grid.X.1), layer=1)

	rg <- function(){ # function to add noise to the environment process
		rnorm(n=Nv, sd=t.sd)
	}
	for(i in 2:grid.t){ # loop through years adding noise
		grid.X <- setValues(grid.X, values(raster::subset(grid.X, i-1))+rg()+X.slope, layer=i)
	}
	
	return(grid.X)
	
}


