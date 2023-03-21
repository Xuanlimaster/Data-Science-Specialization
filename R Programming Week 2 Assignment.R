library(dplyr)
directory <- "C:/Users/21048/OneDrive/Documents/R/RFiles/Data Science Specialization/specdata"

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
  files <- list.files(directory,full.names = TRUE)[id]
  pollutantdatas <- lapply(files,function(x) read.csv(x)[[pollutant]])
  newsta <- unlist(pollutantdatas)
  meandata <- mean(newsta,na.rm = TRUE)
  meandata
}

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
  files <- list.files(directory,full.names = TRUE)[id]
  alldatas <- lapply(files,function(x) read.csv(x))
  nobs <- vector(length = length(id))
  for (i in 1:length(id)) {
    buffer_1 <- alldatas[[i]]
    buffer <- buffer_1[(!is.na(buffer_1$sulfate)), ]
    result <- buffer[(!is.na(buffer$nitrate)), ]
    nobs[i] <- nrow(result)
  }
  fin <- data.frame(id = id, nobs = nobs)
  fin
}

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all 
  ## variables) required to compute the correlation between
  ## nitrate  and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  ## Note: Do not round the result!
  complete_cases_all <- complete(directory)
  complete_cases <- complete_cases_all[complete_cases_all$nobs >= threshold, ]
  vector <- vector(mode = "numeric", length = nrow(complete_cases))
  listid <- list(complete_cases$id)

  for(id in listid){
    files <- list.files(directory,full.names = TRUE)[id]
    alldatas <- lapply(files,function(x) read.csv(x))
  }
  for(i in 1:nrow(complete_cases)){
    buffer_1 <- alldatas[[i]]
    buffer_2 <- buffer_1[(!is.na(buffer_1$sulfate)), ]
    result <- buffer_2[(!is.na(buffer_2$nitrate)), ]
    sulfate_data <- result$sulfate
    nitrate_data <- result$nitrate
    cor_result <- cor(sulfate_data, nitrate_data)
    vector[i] <- cor_result
  }
  vector
}