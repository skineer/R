corr <- function(directory, threshold = 0){
    if(dir.exists(directory) == FALSE){
        print("This directory does not exist")
        return(-1)
    } else {
        correlation <- numeric()
        cont <- 1
        for(file in list.files(directory)) {
            fulldir <- paste(directory,"\\", file , sep = "")
            data <- read.table(fulldir, sep = ',', header = TRUE)
            if(nrow(data[complete.cases(data),]) > threshold){
                correlation[cont] <- cor(data[, 'sulfate'], data[, 'nitrate'], use = "complete.obs")
                cont <- cont + 1
            }
        }
    }
    return(correlation)
}