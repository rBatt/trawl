
# String to call
# convert a character vector to a call of the desired class
# e.g., convert c("column1","column2") to list(column1, column2)
# intended for use in a data.table (see ?"[.data.table")

# x is the string
# type is the class resulting from the evaluation of the call returned by s2c

s2c <- function(x, type="list"){
	as.call(lapply(c(type, x), as.symbol))
}

# example
# first <- data.table(cray=sample(letters,4),"one"=c(1,2,3,4), "two"=c(1,3,5,7))
# second <- data.table("cray"=sample(letters,35, TRUE))
#
# oc <- CJ(one=first[,(one)], cray=second[[1]])
#
# sub <- s2c(c("one","cray"))
#
# oc[,eval(sub)]

