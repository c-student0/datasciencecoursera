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

  ## Write a function that reads a directory full of files and reports the
  ## number of completely observed cases in each data file. The function should
  ## return a data frame where the first column is the name of the file and the
  ## second column is the number of complete cases. A prototype of this function
  ## follows

  ## read files
  ## Option 2 - read all files in
  files <- list.files(path=directory, full.names=TRUE, pattern=".csv")
  dataList <- lapply(files, read.csv, header=TRUE)
  
  ## Clean up the data frame and enviroment
  data <- do.call(rbind, dataList)
  rm(dataList)
  
  ##
  completeData <- data[complete.cases(data),]
  
  #rbind(id, sum(data$ID ))
  output <- data.frame(ID=rep(NA, length(id)), nobs=rep(0, length(id)), stringsAsFactors=FALSE)
  line <- 0
  for(i in id) {
    line <- line + 1
    output[line, ] <- c(i, sum(completeData$ID == i))
  }
  output
}
