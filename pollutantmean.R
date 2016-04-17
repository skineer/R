pollutantmean <- function(directory, pollutant, id=1:332){
    library(stringr)
    if(dir.exists(directory) == FALSE){
        print("This directory does not exist")
        return(-1)
    } else {
        if(pollutant != 'sulfate' & pollutant != 'nitrate'){
            return(-1)
        } else {
            valores <- vector()
            for(i in id){
                fulldir <- paste(directory,"\\",str_pad(i,3,pad=0), ".csv", sep = "")
                data <- read.table(fulldir, sep = ',', header = TRUE)
                valores <- c(valores, data[, pollutant])
            } 
        }
    }
    return(mean(valores, na.rm = TRUE))
}