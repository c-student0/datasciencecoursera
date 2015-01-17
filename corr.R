corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations
  
  ##  Write a function that takes a directory of data files and a threshold for
  ##  complete cases and calculates the correlation between sulfate and nitrate
  ##  for monitor locations where the number of completely observed cases (on
  ##  all variables) is greater than the threshold. The function should return a
  ##  vector of correlations for the monitors that meet the threshold
  ##  requirement. If no monitors meet the threshold requirement, then the
  ##  function should return a numeric vector of length 0. A prototype of this
  ##  function follows
  source("complete.R")
  
  c <- complete(directory)
  matchingIDs <- c[c$nobs > threshold,"ID"]
  if ( length(matchingIDs) == 0 ) {
    return(as.numeric(NULL))
  }

  ## read files
  files <- list.files(path=directory, full.names=TRUE, pattern=".csv")
  dataList <- lapply(files, read.csv, header=TRUE)
  ## Clean up the data frame and enviroment
  data <- do.call(rbind, dataList)
  rm(dataList)
  
  completeData <- data[complete.cases(data),]
  output <- NULL
  for(i in matchingIDs ) {
    matchingCases <- completeData[completeData$ID == i,]
    #output <- c(output, round(cor(matchingCases$sulfate, matchingCases$nitrate), 5))    
    output <- c(output, cor(matchingCases$sulfate, matchingCases$nitrate))    
  }
  output
}
