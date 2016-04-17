pollutantmean <- function(directory, pollutant, id=1:332){
    library(stringr)
    if(dir.exists(directory) == FALSE){
        print("This directory does not exists")
        return(-1)
    } else {
        if(pollutant != 'sulfate' & pollutant != 'nitrate'){
            return(-1)
        } else {
            valores <- vector()
            for(i in id){
                print(i)
                fulldir <- paste(directory,"\\",str_pad(i,3,pad=0), ".csv", sep = "")
                print(fulldir)
                data <- read.table(fulldir, sep = ',', header = TRUE)
                valores <- c(valores, data[, pollutant])
            } 
        }
    }
    return(mean(valores, na.rm = TRUE))
}