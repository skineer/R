# Objective: Return the a hospital, by state, that had the lowest mortality rate
# Input: State and Outcome. Outcome has to be "heart attack", 
#        "heart failure" or "pneumonia"

best <- function(state, outcome) {
    setwd("C:\\Users\\Renato\\hospital")
    hosp_data <- read.table("outcome-of-care-measures.csv", 
                            sep = ',', header = TRUE)
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
    # order the filtered df
    ofilter <- filter2[order(as.numeric(paste(filter2$Hospital.30)),
                             as.character(paste(filter2$Hospital.Name))),]
    return(as.vector(ofilter[1,1]))
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}