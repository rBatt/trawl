

# roundFrac <- function(x, frac=0.5){
# 	round(x/frac,0)*frac
# }

roundGrid <- function(x, frac=1){
	# if frac is 1, then place in a 1º grid
	# if frac is 0.5, then place in the 0.5º grid
	floor(x/frac)*frac+frac/2
}

ll2strat <- function(lon, lat, gridSize=1){
	do.call("paste", list(roundGrid(lon, gridSize), roundGrid(lat, gridSize)))
}