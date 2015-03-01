pollutantmean <- function(directory, pollutant, id = 1:332) {

	setwd(directory)

	fileNames <- sprintf("%03d.csv",id)
    
    ## initially empty
	df <- data.frame(sulfate= numeric(0), nitrate= integer(0))

	for (fileName in fileNames) {
		df <- rbind(df, read.csv(fileName, header=TRUE, colClasses=c("NULL", "numeric", "numeric", "NULL")))
	} 

	x <- df[,pollutant];
	mean(x[!is.na(x)])


}