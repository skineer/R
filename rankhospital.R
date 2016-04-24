rankhospital <- function(state, outcome, num = "best") {
    setwd("C:\\Users\\Renato\\hospital")
    hosp_data <- read.table("outcome-of-care-measures.csv", 
                            sep = ',', header = TRUE, colClasses = "character")
    # test the states possibilities
    if(state %in% hosp_data$State == FALSE){
        stop("invalid state")
    }
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
    filter <- hosp_data[hosp_data$State == state, c(2, column)]
    filter2 <- filter[filter$Hospital.30 != "Not Available",]
    ofilter <- filter2[order(as.numeric(filter2$Hospital.30),
                             as.character(filter2$Hospital.Name)),]
    if(tolower(num) == "best"){
        return(as.vector(ofilter[1,1]))
    }
    if(tolower(num) == "worst"){
        return(as.vector(tail(ofilter,1)[1,1]))
    }
    if(is.numeric(num) == TRUE){
        return(as.vector(ofilter[num,1]))
    }
    if(num > nrow(ofilter)){
        return(NA)
    }
}
