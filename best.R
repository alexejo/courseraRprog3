best <- function(state,outcome){
    ## Read outcome data
    outcomes.data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ## table of illnesses and column numbers for data set
    illness.table <- data.frame(illness=c("heart attack","heart failure","pneumonia"),colnum=c(11,17,23))
    ## capture data and
    ## Check that state and outcome are valid
    states.vector <- outcomes.data$State==state
    column.vector <- illness.table[,1]==outcome
    column.number <- illness.table[column.vector,2]
    
    if(sum(states.vector)==0){
        stop("invalid state")
    }
    if(sum(column.number)==0){
        stop("invalid outcome")
    }

    

    illness.outcomes <- outcomes.data[states.vector,c(2,column.number)]
    ## return hospital name in that state with lowest 30 day

    ## death rate
    illness.outcomes[,2] <- as.numeric(illness.outcomes[,2])

    ##remove nas
    illness.outcomes <- illness.outcomes[!is.na(illness.outcomes[,2]),]
    
    lowest.mortality <- min(illness.outcomes[,2])

    ## find hospital(s) with lowest mortality
    low.mort.hosp <- illness.outcomes[illness.outcomes[,2]==lowest.mortality,1]

    ## find number of hospitals and if so sort and find first alphabetically
    if (length(low.mort.hosp)>1){
        low.mort.hosp <- as.character(sort(low.mort.hosp))        
    }

    low.mort.hosp[1]

}
