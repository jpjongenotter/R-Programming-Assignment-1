pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!
        
        values <- numeric(0)
  
        for(i in id) {
          if (i < 10) {
            fileindex <- paste("00",i,sep="")
          }
          else if (i < 100) {
            fileindex <- paste("0", i, sep="")
          }
          else {
            fileindex <- i
          }
  
          filename <- paste(directory, "/", fileindex, ".csv", sep ="")
        	data <-read.csv(filename)
        	values <- c(values, data[[pollutant]])
        }
        
        mean(values, na.rm = TRUE)
}