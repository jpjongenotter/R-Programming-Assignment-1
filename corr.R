corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## NOTE: Do not round the result!

  correlations <- numeric(0)
  
    filelist <- list.files(directory)
    for (filename in filelist) {
      data <-read.csv(paste(directory, "/", filename, sep=""))
      
      complete_data <- complete.cases(data) 
      sum_ok <- sum(complete_data)
      if (sum_ok > threshold) {
        #sulfate_values <- data$sulfate[complete_data]
        #nitrate_values <- data$nitrate[complete_data]
        correlation <- cor(data$sulfate, data$nitrate, use="complete.obs")
        
        correlations <- c(correlations, correlation)
      }
    }
  
  correlations  
}