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
  
  directory <- directory
  range <- id
  
  full_directory <- paste(getwd(),directory,sep="/")
  #print (full_directory)
  allfiles<- list.files(full_directory)
  filenames <- allfiles[range]
  #print(filenames)
  
  totalCompleteCases <- numeric()
  for (f in filenames){
    filepath <- paste(getwd(),"specdata",f,sep="/")
    #print(filepath)
    datafile = read.csv(filepath)
    #str(datafile)
    numberOfCompleteCases <- nrow(datafile[complete.cases(datafile),])
    totalCompleteCases <- c(totalCompleteCases,numberOfCompleteCases) 
  }
  
  #print(numberOfCompleteCases)
  output <- data.frame(
    id=range,
    nobs=totalCompleteCases
    )
  return(output)

}