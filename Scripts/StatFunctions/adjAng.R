
# What are the angles to use for rook?
# "If a cooler or warmer cellw as found then the trajectory was moved along in the direction of that cell (phi) at a speed given by V/cos(phi-theta)" (Burrows et al. 2014 Nature, 'Calculating trajectories' in Methods, 3rd from last sentence).
# Convert "rook directions" to angles. These angles represent degrees from north; so 0 is up/north, and the angle increases in a clockwise direction
adjAng <- function(rDir){
	as.numeric(c(
		"5"=0,
		"4"=3*pi/2,
		"6"=pi/2,
		"2"=0,
		"8"=pi
	)[as.character(rDir)])
}