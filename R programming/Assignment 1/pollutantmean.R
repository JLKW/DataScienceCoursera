pollutantmean <- function (directory, pollutant, id=1:332){
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  directory <- directory
  range <- id
  pollutant <- pollutant

  full_directory <- paste(getwd(),directory,sep="/")
  #print (full_directory)
  allfiles<- list.files(full_directory)
  filenames <- allfiles[range]
  #print(filenames)
  
  filepaths <- vector("character",0)
  for (f in filenames){
    filepath <- paste(getwd(),"specdata",f,sep="/")
    #print(filepath)
    filepaths <- c(filepaths,filepath) 
  }
  
  datafile <- lapply(filepaths,read.csv)
  datafile <- do.call(rbind,datafile)
  #str(datafile)
  cvector <- datafile[[pollutant]]
  return(mean(cvector,na.rm=TRUE))
  
}


