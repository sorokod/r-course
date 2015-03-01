complete <- function(directory, id = 1:332) {

	setwd(directory)


    ## initially empty
	df <- data.frame(character(0), integer(0))

	for (fileName in id) {
		currentDf <- read.csv(sprintf("%03d.csv",fileName), header=TRUE, colClasses=c("NULL", "numeric", "numeric", "NULL"))
		x <- complete.cases(currentDf)
		x <- x[x == TRUE]
		df <- rbind(df, c(fileName, length(x)))
	} 
	colnames(df) <- c("id", "nobs")
	df
}