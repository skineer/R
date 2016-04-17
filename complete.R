complete <- function(directory, id = 1:332){
    library(stringr)
    if(dir.exists(directory) == FALSE){
        print("This directory does not exist")
        return(-1)
    } else {
        df <- data.frame(id = numeric(length(id)), nobs = numeric(length(id)))
        cont <- 1
        for(i in id){
            fulldir <- paste(directory,"\\",str_pad(i,3,pad=0), ".csv", sep = "")
            data <- read.table(fulldir, sep = ',', header = TRUE)
            df$id[cont]    <- i
            df$nobs[cont]  <- nrow(data[complete.cases(data),])
            cont <- cont + 1
        } 
    }
    print(df)
}