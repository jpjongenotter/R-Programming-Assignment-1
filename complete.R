complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  ids <- integer(0)
  nobs <- integer(0)

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
    
    sum_ok <- sum(complete.cases(data))

    ids <- c(ids, i)
    nobs <- c(nobs, sum_ok)
  }
  
  complete_data <- data.frame(id, nobs, stringsAsFactors=FALSE)
}