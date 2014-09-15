best <- function(state,outcome){
    ## Read outcome data
    outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ## table of illnesses and column numbers for data set
    illness.table <- data.frame(illness=c("heart attack","heart failure","pneumonia"),colnum=c(11,18,27))

    ## capture data and
    ## Check that state and outcome are valid
    states.vector <- outcome$State==state
    column.vector <- illness.table[,1]==outcome
    
    if(sum(states.vector))==0{
        stop("invalid state")
    }else if(sum(column.vector))==0{
        stop("invalid outcome")
    }

    column.number <- illness.table[column.vector,2]

    illness.outcomes <- outcomes[states.vector,column.number]
    ## return hospital name in that state with lowest 30 day
    ## death rate

}
