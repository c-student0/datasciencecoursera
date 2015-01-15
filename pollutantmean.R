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
	
  ## Write a function named 'pollutantmean' that calculates the mean of a
  ## pollutant (sulfate or nitrate) across a specified list of monitors. The
  ## function 'pollutantmean' takes three arguments: 'directory', 'pollutant',
  ## and 'id'. Given a vector monitor ID numbers, 'pollutantmean' reads that
  ## monitors' particulate matter data from the directory specified in the
  ## 'directory' argument and returns the mean of the pollutant across all of
  ## the monitors, ignoring any missing values coded as NA. A prototype of the
  ## function is as follows
  
  #options(digits=4)
	## read files
  pwd <- getwd()
  setwd(directory)  #set directory
  
  ## Option 1 - read only some files in
	#library(stringr)  # string functions
	#str_pad(anim, 6, pad = "0")
  #data <- lapply(paste(str_pad(1:332, 3, pad="0"), sep=".csv"), read.csv, header=TRUE)
  
  ## Option 2 - read all files in
  files <- list.files(pattern=".csv")
	dataList <- lapply(files, read.csv, header=TRUE)
  
  ## Clean up the data frame and enviroment
	data <- do.call(rbind, dataList)
  rm(dataList)
  setwd(pwd) 
  
  # Do the mean
  round(mean(data[[pollutant]][data$ID %in% id], na.rm = TRUE), 3)
}
