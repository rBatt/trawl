


#'@param comD the "compact" data set, supplied as a data.table

#'@param keyID columns in keyID will either define the arr.dim, the elements of the output array, or, for those elements not in either of those, will be aggreated over

#'@param keyValue the name of the column whose values, along with those in fillValue, will form the elements of the output array

#'@param arr.dim character vector specifying the names of columns that would specify the dimensions of the output array

#'@param scope character vector specifying columns. The number of unique combinations of factors among those columns will specify the number of output arrays. If left NULL, there will be 1 output array with dimension sizes equal to the number of factor levels in the respective elements of comD.

#'@param redID a list of the categories to uniquely define redValue; can only include values in arr.dim, but must not include all of them

#'@param redValue a list of character vectors whose elements correspond to column names whose levels are redundant with some subset of keyID; e.g., if a keyID is "species", then redValue would include columns like Genus and Family, specified as list(c("Genus","Family")), and the corresponding redID would be list(c("species")).

#'@param fillID a list of character vectors, each of which is a subset of scope, and when missing levels of these factor/s are added for existing combinations of setdiff(arr.dim, fillID[[i]]), should be filled in with the corresponding value of fillValue. Note that the last element of fillID will always contain the full set in arr.dim, because the resulting array cannot be ragged.

#'@param fillValue a vector of values that should be used to fill in missing combinations of arr.dim. The class of fillValue should match the class of keyValue.

#'@param arrayOut logical; Defaults to FALSE, in which case the output is a data.table, with number of rows corresponding to the product of the number of unique values in each of arr.dim (in the case where scope=NULL), or the sum of that product for each of the unique combinations of the levels in scope. If TRUE, the output is an array (if scope=NULL), or list of arrays.

#'@param aggFun Function used to aggregate among levels of columns specified in keyID, but not present in arr.dim.

#'@param maxOut the maximum number of allowable elements in the output array or data.table. In place to prevent early detection of a huge number of combinations that might use up a larger-than-expected amount of memory.


expand.data <- function(comD, keyID, keyValue, arr.dim, scope=NULL, redID=NULL, redValue=NULL, fillID=arr.dim, fillValue=NA, arrayOut=FALSE, aggFun=NULL, maxOut=Inf){
	# aggID=setdiff(keyID, union(arr.dim, scope)),

	# comD = testT.sub2 # the "compact" data set
	# keyID = c("s.reg", "stratum","year","spp","K") # columns in keyID will either define the arr.dim, the elements of the output array, or, for those elements not in either of those, will be aggreated over
	# keyValue = c("value") # or value
	# arr.dim = c("stratum","year","spp")
	# scope = c("s.reg")
	# redID = list( # the categories to uniquely define redValue; can only include values in arr.dim, but must not include all of them
	# 	c("spp"),
	# 	c("stratum","year"),
	# 	c("stratum")
	# )
	# redValue = list(
	# 	c("correctSpp","taxLvl","phylum","common"), # uniquely defined by spp
	# 	c("stemp","btemp","depth"), # uniquely defined by stratum-year
	# 	c("lat","lon") # uniquely defined by stratum
	# )
	#
	# fillID = list( # a list of character vectors, each of which is a subset of scope, and when missing levels of these factor/s are added for existing combinations of setdiff(arr.dim, fillID[[i]]), should be filled in with the corresponding value of fillValue. Note that the last element of fillID will always contain the full set in arr.dim, because the resulting array cannot be ragged.
	# 	c("spp"),
	# 	c("spp","stratum","year")
	# )
	#
	# fillValue = c(0, NA)
	#
	# aggFun = meanna
	#
	
	# ==========
	# = Checks =
	# ==========
	stopifnot(require(data.table))
	stopifnot(is.data.table(comD))
	stopifnot(class(fillValue==class(comD[,eval(parse(text=keyValue))])))
	stopifnot(length(redID)==length(redValue))
	stopifnot(length(fillID)==length(fillValue))
	nrow.out <- comD[,prod(sapply(eval(s2c(keyID)), lu))]
	size.out <- nrow.out * length(arr.dim)
	if(arrOut){
		stopifnot(nrow.out<=maxOut)
	}else{
		stopifnot(size.out<=maxOut)
	}
	
	# =========================
	# = Save Redundant Values =
	# =========================
	for(r in 1:length(redID)){
		
	}
	
	
	
	# ===================================
	# = Aggregate over marginal keyID's =
	# ===================================
	aggID <- setdiff(keyID, union(arr.dim, scope))
	if(length(aggID)>0){
		if(is.null(aggFun)){stop("arr.dim is a subset of names in keyID; must provide an aggregation function via aggFun")}
		aggFun <- match.fun(aggFun)
		comD <- comD[,value:=eval(s2c(keyValue))]
		comD <- comD[,list(value=aggFun(value)), by=arr.dim] # aggregate step: used when not all of the values in keyID are part of arr.dim
	}else{
		comD <- comD[,value:=eval(s2c(keyValue))]
		comD <- comD[,eval(s2c(c(arr.dim,"value")))]
	}
	
	
	# ===================================
	# = Fill out a subset of dimensions =
	# ===================================
	# Fill out a subset of dimensions; i.e., the elements of fillID that are not equivalent to arr.dim
	if(length(fillID)>1){ # right now only handling case where length of fillID (and fillValue) is 1 or 2.
		# for(i in 1:(length(fillID-1))) # placeholder
		i=1L # placeholder for now
		tfid <- fillID[[i]]
		fillBy <- setdiff(arr.dim, tfid)
		tuid <- paste0("uid",i)
		
		comD[,eval(tuid):=.GRP, by=fillBy]
		setkeyv(comD, eval(tuid))
		id.dt <- comD[,
			j={
				idset <- do.call(CJ, lapply(eval(s2c(c(tuid, tfid))), unique))
				setnames(idset, names(idset), c(tuid, tfid))
				setkey(idset,uid1)
				idset[unique(.SD[,eval(s2c(c(tuid, fillBy)))])]
			}#,
			# by=scope
		]
		setkeyv(id.dt, cols=c(tfid,fillBy)) # would just do arr.dim but it has to be in this order (i.e., with fillID[[1]] 1st)
		setkeyv(comD, cols=c(tfid,fillBy))
		fill.gaps <- comD[id.dt, which=TRUE, allow.cartesian=TRUE]
		id.dt[,value:=comD[fill.gaps,.(value)]]
		id.dt[is.na(fill.gaps),value:=fillValue[i]]
		
		# Cleanup, prepare
		id.dt[,eval(tuid):=NULL]
		setkeyv(id.dt, cols=arr.dim)
	}


	# ==============================
	# = Fill in remainder of array =
	# ==============================
	# Always the final step of filling out array: Fill in the rest of the elements of the array w/ NA's
	expD <- id.dt[do.call(CJ, lapply(eval(s2c(arr.dim)), unique))]

	
	# ===================
	# = Return if array =
	# ===================
	if(arrayOut){
		# scope = c("year", "s.reg")
# 		combn.out <- expD[,do.call(CJ, lapply(eval(s2c(scope)), unique))]
# 		nlist <- do.call(CJ, lapply(eval(s2c(c(tuid, tfid))), unique))
#
# 		array.list <- list()
		
		dim.names <- lapply(expD[,eval(s2c(arr.dim))], unique)
		sapply(dim.names, length)
		array(expD[,value], dim=sapply(dim.names, length), dimnames=dim.names)
	}
	
	

}


