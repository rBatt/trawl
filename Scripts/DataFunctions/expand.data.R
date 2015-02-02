


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


expand.data <- function(comD, arr.dim, fillID=arr.dim, fillValue=NA, Rule, keyID=NULL, keyValue="value", gScope=NULL, fScope, vScope, redID=NULL, redValue=NULL, arrayOut=FALSE, aggFun=NULL, maxOut=Inf){

	# =========
	# = Setup =
	# =========
	if(is.null(keyID)){keyID <- key(comD)}
	
	# ==========
	# = Checks =
	# ==========
	stopifnot(require(data.table))
	stopifnot(is.data.table(comD))
	stopifnot(class(fillValue)==class(comD[,eval(parse(text=keyValue))]))
	stopifnot(length(redID)==length(redValue))
	stopifnot(length(fillID)==length(fillValue))
	nrow.out <- comD[,prod(sapply(eval(s2c(keyID)), lu))]
	size.out <- nrow.out * length(arr.dim)
	if(arrayOut){
		stopifnot(nrow.out<=maxOut)
	}else{
		stopifnot(size.out<=maxOut)
	}
	
	
	# ===================================
	# = Aggregate over marginal keyID's =
	# ===================================
	IDs <- unique(c(gScope, unlist(fScope), arr.dim)) # basically the keyID of the *output* data.table # TODO this definition is broken when trying to aggregate across K
	aggID <- setdiff(keyID, IDs)
	comD0 <- comD
	if(length(aggID)>0){ # determines if it's necessary to aggregate
		if(is.null(aggFun)){stop("arr.dim is a subset of names in keyID; must provide an aggregation function via aggFun")}
		aggFun <- match.fun(aggFun)
		comD <- comD[,value:=eval(s2c(keyValue))] # I overwrite comD to save memory
		comD <- comD[,list(value=aggFun(value)), by=IDs] # aggregate step: used when not all of the values in keyID are part of arr.dim
	}else{ # if it's not necessary to aggregate, still need to format a bit and drop extra columns
		comD <- comD[,value:=eval(s2c(keyValue))]
		comD <- comD[,eval(s2c(c(IDs,"value")))]
	}
	
	
	# ===================================
	# = Fill out a subset of dimensions =
	# ===================================
	# Fill out a subset of dimensions; i.e., the elements of fillID that are not equivalent to arr.dim		
	setkeyv(comD, c(IDs))
	
	id.dt <- comD[,
		j={
			idset <- do.call(CJ, lapply(eval(s2c(setdiff(IDs,gScope))), unique))
			setnames(idset, names(idset), setdiff(IDs,gScope))
			idset
		},
		by=c(gScope)
	]
	# setorder(id.dt, s.reg, year, stratum, K, spp)
	# id.dt[,max(K),by=c("year","stratum")][,lu((V1)),by="year"]
	
	setkeyv(id.dt, IDs)
	# comD[1, value:=NA]
	expD <- comD[id.dt]
	keepNA <- expD[is.na(value)&!is.na(comD[id.dt, which=TRUE])] # keeping track of where NA's are in original data set
	# expD[sample(1:nrow(expD), 100),]
	if(length(fillID)>0){
		
		for(i in 1:length(fillID)){
			
			t.fID <- fillID[i]
			
			if(Rule[i]=="scope"){
				t.cols <- c(fScope[[i]], t.fID)
				orig <- unique(data.table(comD[,eval(s2c(t.cols))], key=c(t.cols))) # the original combinations of IDs
		
				setkeyv(expD, key(orig))
				expD <- expD[orig]
			}
			
			if(Rule[i]=="value"){
				t.cols <- c(vScope[[i]])
				orig <- unique(data.table(comD[,eval(s2c(t.cols))], key=c(t.cols))) # the original combinations of IDs
				
				setkeyv(expD, key(orig))
				expD[orig, t.fill:=fillValue[i]] # new column w/ NA or, if orig IDs found, fillValue
				expD <- expD[is.na(value), value:=t.fill] # this replaces NAs in the original data set with 0's, see fix in keepNA
				expD[,t.fill:=NULL]
				
				if(nrow(keepNA)>0){ # NAs in original data set will (or can) be replaced by t.fill value, so changing back to NA's
					setkeyv(expD, IDs)
					setkeyv(keepNA, IDs)
					expD[keepNA[,eval(s2c(IDs))],value:=NA]
				}
				
			}
		}
	}
	
	# ==============================
	# = Return array or data.table =
	# ==============================
	if(arrayOut){ # return if array
		
		outScope <- union(unlist(gScope), unlist(fScope)) # the output scope determiens the number of arrays that need to be formed
		outsize <- nrow(unique(data.table(expD[,eval(s2c(outScope))], key=c(outScope)))) # the number of arrays
		array.list <- vector("list", outsize) # preallocate data array
		array.key <- vector("list", outsize) # the list of arrays can be long and hard to navigate; this key will supply the outScope combinations present in each element of the array.list output list
		
		invisible(expD[, # within the j of this data.table, build each element of the output array list
			j={
				dim.names <- lapply(.SD[,eval(s2c(arr.dim))], unique)
				array.list[[.GRP]] <<- array(.SD[,value], dim=sapply(dim.names, length), dimnames=dim.names)
				array.key[[.GRP]] <<- c(unlist(.BY),.GRP) # grab the names of the by= groups, add them to the output key
			},
			by=c(outScope)
		])
		
		# lapply(array.list, dim)
		
		array.key <- data.table(matrix(unlist(array.key), ncol=length(outScope)+1, byrow=TRUE)) # build the key to the output array
		setnames(array.key, names(array.key), c(outScope, "num")) # key the key
		
		return(list(array.list=array.list, array.key=array.key)) # return the output array and its key (stops function)
		
	}else{ # end first half of if(arrayOut), begin alternative
	# ==================================================================
	# = If not an array, return formatted data table with "red" values =
	# ================================================================== 
		if(!is.null(redID)){ # if there are redID's that should be added back in ...
			for(i in 1:length(redID)){ # for each redundant id/ value ...
				rN <- c(redID[[i]], redValue[[i]]) # get the names of the redundant ID (the value chosen to represent others), and the redundant values
				setkeyv(expD, redValue[[i]]) # set the key of expD to be the redID
				expD <- unique(data.table(comD0[,eval(s2c(rN))], key=c(rN)))[expD,][,eval(s2c(c(names(expD),redValue[[i]])))]
			} # end loop through redID
		} # end redID if
		return(expD)
	} # end if(){}else{} for arrayOut
} # end expand.data()



# ============
# = Examples =
# ============
# testT2 <- as.data.table(melt(trawl2, id.vars=c("s.reg","year","stratum","K","correctSpp","taxLvl","phylum","spp","common"), measure.vars=c("wtcpue","stemp","btemp","lat","lon","depth")))
#
# testT.sub <- testT2[variable=="wtcpue" & s.reg=="gmex" & (!is.na(taxLvl)&taxLvl=="Species") & correctSpp==TRUE]
# setkey(testT.sub, stratum, K, year, spp)
#
# testT.sub <- testT2[variable=="wtcpue" & (!is.na(taxLvl)&taxLvl=="Species") & correctSpp==TRUE]
# setkey(testT.sub, stratum, K, year, spp)
#
# msom.array <- expand.data( # takes ~1.5 seconds (just gmex, wtcpue, Species, correctSpp), original code took 63.6 seconds
# 	comD = testT.sub,
# 	arr.dim = c("stratum", "K", "spp"), # should uniquely define the keyValue when combined with fScope
# 	fillID=c("spp","K"),
# 	fillValue=c(0,NA), # values to fill with, for a fillID
# 	Rule=c("value", "scope"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=key(comD), # column names whose values uniquely identify rows
# 	keyValue="value", # the column whose values would fill the array
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg", c("s.reg","year")), #
# 	vScope=list(c("s.reg","stratum","year","K"), NULL),
# 	redID=list(c("spp")), redValue=list(c("correctSpp","taxLvl","phylum","common")),
# 	arrayOut=TRUE, aggFun=meanna, maxOut=Inf
# )
#
#
# array.filled <- expand.data( # this test the aggregate functionality, data.table output, non-NA fill, 1 fillID
# 	comD = testT.sub,
# 	arr.dim = c("stratum", "year", "spp"), # should uniquely define the keyValue when combined with fScope
# 	fillID=c("spp"),
# 	fillValue=c(0), # values to fill with, for a fillID
# 	Rule=c("value"), # does fillID use a non-NA fill value, or does it have restricted (more specific than "global") scope?
# 	keyID=NULL, #c("s.reg", "stratum","year","spp", "K"), # column names whose values uniquely identify rows in the input
# 	keyValue="value", # the column whose values would fill the array
# 	gScope="s.reg", # global scope
# 	fScope=list("s.reg"), #
# 	vScope=list(c("s.reg","stratum","year")),
# 	redID=list(c("spp")), redValue=list(c("correctSpp","taxLvl","phylum","common")),
# 	arrayOut=FALSE, aggFun=meanna, maxOut=Inf
# )
#
#
# array.filled[sample(1:nrow(array.filled), 100),]