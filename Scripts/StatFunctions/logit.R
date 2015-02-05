# ===================
# = Logit functions =
# ===================
logit <- function(x){
    log(x/(1 - x))
}

anti.logit <- function(x){
    exp(x)/(1 + exp(x))
}
