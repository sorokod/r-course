rankhospital <- function(state, outcome, num = "best") {
    validOutcomes <- list()
	validOutcomes[[ "heart attack"  ]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
	validOutcomes[[ "pneumonia"     ]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
	validOutcomes[[ "heart failure" ]] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"    	

	## Read outcome data
 	data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
 	## subset to state
    data <- data[data$State == state, ]

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

   
	data <- data[order(data[ ,outcomeColumn] , data$Hospital.Name), ]


	if(num == "worst") {
		rank = length(data$Hospital.Name)
	}
	else if (num == "best") {
		rank = 1
	} 
	else {
		rank = num
	}

	data$Hospital.Name[rank]



## Return hospital name in that state with the given rank
## 30-day death rate
}