
library(TeachingDemos)


z1 <- function(xx){
	xx <- xx-mean(xx, na.rm=TRUE)
	xx <- xx/max(xx, na.rm=TRUE)
	xx
}

sh_hourglass <- function(){
	sh0 <- dnorm(seq(-2,2,length.out=100))
	x <- c(sh0, 1+-rev(sh0), sh0[1])
	y <- c(1:100, 100:1, 1)
	
	x <- z1(x)
	y <- z1(y)
	
	list(x=x, y=y)
}

sh_tri <- function(ud = c('up','down')){
	ud <- match.arg(ud)
	sh0 <- dnorm(seq(-1.5,1.5,length.out=100))
	
	ud_fac <- as.integer(ud=="up")*2 - 1
	y <- c(ud_fac*sh0, ud_fac*sh0[1])
	x <- c(1:100, 1)

	
	x <- z1(x)
	y <- z1(y)
	
	list(x=x, y=y)
}

sh_rec <- function(){
	set.seed(1337)
	s <- seq(-1,1, length.out=100)
	# s2 <- rnorm(100, sd=0.2)
	s2 <- arima.sim(list(ar=0.95, ma=0.45), n=100, innov=rnorm(n=100, mean=0, sd=0.15))
	sh0 <- dnorm(s, mean=s+s2, sd=1)
	x <- c(sh0, 1.75+-rev(sh0), sh0[1])
	y <- c(1:100, 100:1, 1)
	
	x <- z1(x)
	x <- x/1.5
	y <- z1(y)
		
	list(x=x, y=y)
}

sh_dia <- function(){
	sh0 <- dnorm(seq(-3.5,3.5,length.out=100))
	x <- c(-sh0, 0+rev(sh0), -sh0[1])
	y <- c(1:100, 100:1, 1)
	
	x <- z1(x)
	y <- z1(y)
	
	list(x=x, y=y)
}

# plot(1:5, 1:5)
# my.symbols(1, 1, symb=sh_tri('up'), inches=0.3)
# my.symbols(2, 2, symb=sh_tri('down'), inches=0.3)
# my.symbols(3, 3, symb=sh_rec(), inches=0.3)
# my.symbols(4, 4, symb=sh_dia(), inches=0.3)
# my.symbols(5, 5, symb=sh_hourglass(), inches=0.3)


t_points <- function(x, y, shape, inches=0.2, ...){
	shape <- match.arg(shape, c("downward_triangle", "upward_triangle", "rectangle", "diamond", "hourglass"), several.ok=TRUE)
	ms <- function(shape){
		lapply(shape, switch, 
			downward_triangle = sh_tri("down"),
			upward_triangle = sh_tri("up"),
			rectangle = sh_rec(),
			diamond = sh_dia(),
			hourglass = sh_hourglass()
		)
		
	}
	# plot(x,y, type="n")
	mapply(TeachingDemos::my.symbols, x, y, symb=ms(shape), inches=inches, MoreArgs=list(...))
	
}