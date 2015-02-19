corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations

  directory <- directory
  threshold <- threshold
  
  full_directory <- paste(getwd(),directory,sep="/")
  #print (full_directory)
  allfiles<- list.files(full_directory)
  #print(allfiles)  
  
  nobsSummary = complete("specdata")
  #print(nobsSummary[nobsSummary$nobs>threshold,])
  
  monitorsThatExceedThreshold <- nobsSummary[nobsSummary$nobs>threshold,]
  
  result = numeric()
  for (id in monitorsThatExceedThreshold$id){
    #print(allfiles[id])
    filepath <- paste(getwd(),"specdata",allfiles[id],sep="/")
    #print(filepath)
    datafile <- read.csv(filepath)
    result <- c(result,cor(datafile$nitrate,datafile$sulfate,use="pairwise.complete.obs"))
  }
  
  return(result)
}