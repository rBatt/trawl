
p.persist <- function(temp.prop, temp.obs, method=c("ks.test", "dsample")){
	method <- match.arg(method, c("ks.test","dsample"))
	
	if(method=="ks.test"){
		ks.pp <- function(x){
			# 1-ks.test(x, y=temp.obs)$stat
			ks.test(x, y=temp.obs)$p
		}
		pp <- mapply(ks.pp, temp.prop)
	}
	
	if(method=="dsample"){
		pp <- dsample(ref=temp.obs, x=temp.prop, relative=TRUE)
	}
	
	
	return(pp)
	
}
