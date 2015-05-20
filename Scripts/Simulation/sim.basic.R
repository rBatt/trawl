
# ======================
# = Load Sim Functions =
# ======================
sim.location <- "~/Documents/School&Work/pinskyPost/trawl/Scripts/SimFunctions"
invisible(sapply(paste(sim.location, list.files(sim.location), sep="/"), source, .GlobalEnv))



# ===============
# = Create Grid =
# ===============

# Grid Size

	# Width
	grid.w <- 15
	
	# Height
	grid.h <- 20
	
	# Time
	grid.t <- 12


# Grid Attributes

	# Grid Lat
	
	# Grid Lon
	
	# Grid temperature
	temp.h <- seq(-10, 0, length.out=grid.h)
	temp <- c()
	for(i in 1:grid.w){
		temp <- c(temp, temp.h+rnorm(grid.h))
	}
	
	# Grid depth

# Create Blank Grid
grid.blank <- gen.grid(dims=c(grid.h, grid.w, grid.t), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

# Insert first year of temperature grid
grid.temp.1 <- gen.grid(temp, dims=c(grid.h, grid.w, 1), xmn=0, ymn=0, xmx=grid.w, ymx=grid.h)

# Define temperature change in ºC per year
temp.slope <- 0.75

# Create time series of temperature grid
grid.temp <- grid.blank
grid.temp <- setValues(grid.temp, values(grid.temp.1), layer=1)

rg <- function(){ # function to add noise to the temperature process (kinda weird, b/c I apply the noise at each time step)
	rnorm(n=grid.w*grid.h, sd=0.1)
}
for(i in 2:grid.t){
	grid.temp <- setValues(grid.temp, values(subset(grid.temp, i-1))+rg()+temp.slope, layer=i)
}

# Plot temperature time series
plot(grid.temp, zlim=c(min(values(grid.temp)), max(values(grid.temp))))


# ==================
# = Create Species =
# ==================

# Number of Species

# Distribution (density) of Species across Grid Attributes

