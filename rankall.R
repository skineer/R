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
    filter2 <- filter[filter$Hospital.30 != "Not Available", ]
    ordered <- filter2[order(as.numeric(filter2$Hospital.30),
                             as.character(filter2$Hospital.Name)),]
    # need to split by state in order to calculate the individuals rank
    split_ordered <- split(ordered, ordered$State)
    if(num == "best"){
        answer <- lapply(split_ordered, function(df){ df[1,]})
    }
    if(num == "worst"){
        answer <- lapply(split_ordered, function(df){ tail(df,1)[1,]})
    }
    if(is.numeric(num) == TRUE){
        answer <- lapply(split_ordered, function(df){ df[num,]})
    }
    len <- length(answer)
    df_answer <- data.frame(hospital = numeric(len), state = character(len),
                            stringsAsFactors = FALSE)
    for(i in 1:len){
        df_answer$hospital[i] <- as.data.frame(answer[i])[1,1]
        df_answer$state[i] <- toString(as.data.frame(answer[i])[1,2])
    }
    return(df_answer)
    #full_list <- list()
    #for(item in split_filter){
    #    temp <- item[order(as.numeric(item$Hospital.30),
    #                       as.character(item$Hospital.Name)),]
    #    full_list <- c(full_list, temp)
    #}
}