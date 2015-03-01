corr <- function(directory, threshold = 0) {

	setwd(directory)


	result <- numeric(0)


	for (fileName in list.files(directory) ) {

		currentDf <- read.csv(fileName, header=TRUE, colClasses=c("NULL", "numeric", "numeric", "NULL"))
		x <- complete.cases(currentDf)
    	currentDf <- currentDf[x,]
	
		if(length(x[x == TRUE]) > threshold) {
			result <- c(result, cor(currentDf$sulfate, currentDf$nitrate) )
		}
	} 
	result
}