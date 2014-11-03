# What are the velocities and angles to use for rook?
# Create raster for climate velocity alternatives (when boundary-crossing trajectories need adjusting)
# "If a cooler or warmer cellw as found then the trajectory was moved along in the direction of that cell (phi) at a speed given by V/cos(phi-theta)" (Burrows et al. 2014 Nature, 'Calculating trajectories' in Methods, 3rd from last sentence).

# Note: climV and ang are defined outside the functions
adjV <- function(rDir){
	switch(as.character(rDir),
		"4"=climV/cos(pi/2-ang),
		"6"=climV/cos(1.5*pi-ang),
		"2"=climV/cos(0-ang),
		"8"=climV/cos(pi-ang)
		)
}
adjAng <- function(rDir){
	switch(as.character(rDir),
		"4"=setValues(ang,pi/2),
		"6"=setValues(ang,1.5*pi),
		"2"=setValues(ang,0),
		"8"=setValues(ang,pi)
		)
}