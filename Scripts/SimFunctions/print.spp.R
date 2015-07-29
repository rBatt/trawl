
print.spp <- function(x, ...) {
		cat(
			"Dimensions: ", paste(dim(x), collapse=", "), "\n", 
			paste(c("grid.h","grid.w","grid.t"), c(dim(attr(x, "grid.X"))), sep=" = ", collapse="\n"),
			sep=""
		)
		
		cat("\n\n", "Number Species Possible (ns):", "\n", dim(x)[2], sep="")
		
		cat("\n", "Total Species Richness:", "\n", attr(x, "prnt")$tr, sep="")
		
		obs <- attr(x, "obs")
		has.obs <- !is.logical(obs)
		if(has.obs){
			cat("\n", "Total Observed Species Richness:", "\n", attr(x, "prnt.obs")$tr, sep="")
		}
		
		cat("\n\n", "Annual Species Richness:", "\n", sep="")
		s <- attr(x, "prnt")$s
		s2 <- attr(x, "prnt")$s2
		rn <- c("All cells", "One cell")
		print(matrix(c(s, s2), nrow=2, byrow=TRUE, dimnames=list(rn,names(s))))
		
		if(has.obs){
			cat("\n\n", "Observed Annual Species Richness:", "\n", sep="")
			so <- attr(x, "prnt.obs")$s
			s2o <- attr(x, "prnt.obs")$s2
			print(matrix(c(so, s2o), nrow=2, byrow=TRUE, dimnames=list(rn,names(so))))
		}
		
		invisible(x)			
}