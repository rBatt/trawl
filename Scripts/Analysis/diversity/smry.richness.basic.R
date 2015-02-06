
# =====================================
# = Post-processing of richness.basic =
# =====================================

trueN <- first.out$N
obsN <- sapply(t.dat, function(x)dim(x)[3])


str(first.out$Z)