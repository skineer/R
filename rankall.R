rankall <- function(outcome, num = "best") {
    setwd("C:\\Users\\Renato\\hospital")
    hosp_data <- read.table("outcome-of-care-measures.csv", 
                            sep = ',', header = TRUE, colClasses = "character")

    # test the outcome possibilities
    if(tolower(outcome) != 'heart attack' & 
       tolower(outcome) != 'heart failure' &
       tolower(outcome) != 'pneumonia'){
        stop("invalid outcome")
    }
    
    # test the rank possibilities
    if(tolower(num) != 'best' & 
       tolower(num) != 'worst' &
       is.numeric(num) == FALSE){
        stop("invalid num")
    }
    # Mortality H.A --> Col 11
    # Mortality H.F --> Col 17
    # Mortality P.M --> Col 23
    # Hospital Name --> Col 02
    if(tolower(outcome) == "heart attack"){
        column = 11
    }
    if(tolower(outcome) == "heart failure"){
        column = 17
    }
    if(tolower(outcome) == "pneumonia"){
        column = 23
    }
    filter <- hosp_data[, c(2, 7, column)]
    # need to split by state in order to calculate the individuals rank
    split_filter <- split(filter, filter$State)
    full_list <- list()
    for(item in split_filter){
        temp <- item[order(as.numeric(item$Hospital.30),
                           as.character(item$Hospital.Name)),]
        full_list <- c(full_list, temp)
    }
}