

# ===============================
# = Guess appropriate directory =
# ===============================
if(Sys.info()["sysname"]=="Linux"){
	setwd("~/Documents/School&Work/pinskyPost")
}else{
	setwd("~/Documents/School&Work/pinskyPost")
}


# ================
# = Load Results =
# ================
load("~./trawl/Results/Richness/richness.basic.out.RData")
load("~./trawl/Results/Richness/last.out.RData")


# =====================================
# = Post-processing of richness.basic =
# =====================================
# NOTE: I screwed up the combine function for the run of richness.basic.R on 06-Feb-2015. I also forgot to run the last region-year combo. So for the first 274 I had to manually edit their combination, and only bothered with the sims.list (everything else is a summary of that anyway). Then when I noticed that I didn't run #275, I ran it by itself, called it last.out, and am adding it on here. I'm saving this as rbo.RData. So that richness.basic.out.RData is super messed up!


rbo.11 <- richness.basic.out[[11]] # UGH
rbo.11.1 <- unlist(rbo.11[[1]], F)

# I got last out manually
rbo <- c(rbo.11.1, unlist(rbo.11[-1], F), last.out[[11]])
# save(rbo, file="./trawl/Results/Richness/rbo.RData", compress="xz")





# test <- do.call(Map, c(c, rbo)) # DON'T DO THIS – IT'LL CRASH OR TAKE FOREVER


