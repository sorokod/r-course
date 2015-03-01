rankall <- function(outcome, num = "best") {

    validOutcomes <- list()
	validOutcomes[[ "heart attack"  ]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	validOutcomes[[ "pneumonia"     ]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	validOutcomes[[ "heart failure" ]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"    	

	## Read outcome data
 	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that state and outcome are valid
    if(nrow(data) < 1) {
    	stop("invalid state")
    }
    if(!(outcome %in% names(validOutcomes))) {
		stop("invalid outcome" )
    }

	outcomeColumn <- validOutcomes[[outcome]] 

    ## coerce rates to numerics and remove NAs
	options(warn=-1)
	data[ ,outcomeColumn] <- as.numeric(data[ ,outcomeColumn] )
	options(warn=0)

    
    notNA <- !is.na( data[ ,eval( outcomeColumn )])
	data <- data[notNA,  ]

    states <- unique(unlist(data$State))
    states <- states[order(states)]
   	

    returnValue <- data.frame(hospital=character(), state=character())
    for(state in states) {

    	stateData <- data[data$State == eval(state),]
    	stateData <- stateData[order(stateData[ ,outcomeColumn] , stateData$Hospital.Name), ]
   

		if(num == "worst") {
				rank = length(stateData[,outcomeColumn])
		} else if (num == "best") {
				rank = 1
		}  else {
				rank = num
		}

#  		message(state, rank,"\r",appendLF=TRUE)
#  		flush.console()

		returnValue <- rbind(returnValue,data.frame( stateData$Hospital.Name[rank], state))
    }
    names(returnValue) <- c("hospital", "state")
	returnValue


}
