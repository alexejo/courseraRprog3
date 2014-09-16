rankhospital <- function(state,outcome,num = "best"){
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

    
    # produce dataframe of hospital and outcomes    
    illness.outcomes <- outcomes.data[states.vector,c(2,column.number)]
    
    ## return hospital name in that state with lowest 30 day
    ## death rate
    illness.outcomes[,2] <- as.numeric(illness.outcomes[,2])

    ##remove nas
    illness.outcomes <- illness.outcomes[!is.na(illness.outcomes[,2]),]

    ## order alphabitically
    illness.outcomes <- illness.outcomes[order(illness.outcomes[1]),]

    ## order by number of mortalities
    illness.outcomes <- illness.outcomes[order(illness.outcomes[2]),]

    ## find total number of observations
    no.obs <- nrow(illness.outcomes)

    ## set best and worst
    if (num=="best"){
        num=1
    } else if (num=="worst"){
        num=no.obs
    }
    
    illness.outcomes[num,1]

}
