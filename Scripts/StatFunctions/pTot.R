all.same <- function(x){
    abs(max(x) - min(x)) < 8.881784e-16 # number is (.Machine$double.eps)*4 on my rMBP
}

pTot <- function(p, n=1){
    ln <- length(n)
    if(ln>1 | !all.same(p)){
        # stopifnot(ln == length(p))
        pn <- 1-((1-p)^n)
    }else{
        pn <- p
    }

    1-prod(1 - pn)

}

# blah <- c(); for(i in 1:20){blah[i] <- pTot(rep(0.2,i))}; plot(blah, ylim=0:1)