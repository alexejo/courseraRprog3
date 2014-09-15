best <- function(state,outcome){
    ## Read outcome data
    outcomes.data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ## table of illnesses and column numbers for data set
    illness.table <- data.frame(illness=c("heart attack","heart failure","pneumonia"),colnum=c(11,18,27))
    ## capture data and
    ## Check that state and outcome are valid
    states.vector <- outcome$State==state
    column.vector <- illness.table[,1]==outcome
    column.number <- illness.table[column.vector,2]
    
    if(sum(states.vector)==0){
        stop("invalid state")
    }
    if(sum(column.number)==0){
        stop("invalid outcome")
    }

    

    illness.outcomes <- outcomes.data[states.vector,column.number]
    ## return hospital name in that state with lowest 30 day
    ## death rate

}
