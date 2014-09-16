rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes.data <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

    ## table of illnesses and column numbers for data set
    illness.table <- data.frame(illness=c("heart attack","heart failure","pneumonia"),colnum=c(11,17,23))

    ## create list of states in alphabetical order
    states.list <- sort(unique(outcomes.data$State))    

    ## find number of states
    no.states <- length(states.list)
    
    ## create empty vectors for the state and hospital
    results.state <- numeric(no.states)
    results.hospital <- character(no.states)

    ## find column number of illness
    column.vector <- illness.table[,1]==outcome
    column.number <- illness.table[column.vector,2]

    if(sum(column.number)==0){
        stop("invalid outcome")
    }
    
    ## create dataframe of hospital, state and mortality numbers for illness
    illness.data <- outcomes.data[,c(2,7,column.number)]
    ## turn characters to numbers in mortality figures    
    illness.data[3] <- as.numeric(illness.data[,3])
    ## remove nas
    illness.data <- illness.data[!is.na(illness.data[,3]),]

    ## counter for row count during for loop
    row.count <- 1
    ## cycle through states
    for (state in states.list){
        ## create state data
        state.data <- illness.data[illness.data[,2]==state,]
        ## order alphabetically by hospital
        state.data <- state.data[order(state.data[,1]),]
        ## order by mortality rate
        state.data <- state.data[order(state.data[,3]),]
        ## set number to be checked)
        no.obs <- nrow(state.data)

        if (num=="best"){
            row.num=1
         } else if (num=="worst"){
            row.num=no.obs
         } else {
             row.num=num
         }
        
        ## add to results
        results.hospital[row.count] <- state.data[row.num,1]
        results.state[row.count] <- state.data[1,2]

        row.count <- row.count+1
    }

    data.frame(hospital=results.hospital,state=results.state,stringsAsFactors=FALSE)

}
