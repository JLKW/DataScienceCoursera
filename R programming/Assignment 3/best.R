best <- function(state,outcome){
  
  entireDataSet <- readDataAndCheckValidity(state,outcome)  
  subsetOfLowestMortalityRate(state,outcome,entireDataSet)
  
}


subsetOfLowestMortalityRate <- function(state,outcome,entireDataSet){
  validOutcomes <- c("heart attack","heart failure", "pneumonia")
  outcomeIndexes <- c(11,17,23)
  outcomeTable <- data.frame(validOutcomes,outcomeIndexes)
  outcomeColumnIndex <- outcomeTable[which(outcomeTable$validOutcomes==outcome),2]
  
  ##create a subset of the original dataset that contains only the state given
  subsetDatabyState <- subset(entireDataSet,entireDataSet$State==state,)
  
  ##sort the dataset by mortality rate first then by hospital name
  subsetDatabyState <- subsetDatabyState[order(
    apply(subsetDatabyState[outcomeColumnIndex],1,as.numeric),
    subsetDatabyState[2]),]
  
  #print(subsetDatabyState[,c(2,7,outcomeColumnIndex)])
  return(as.character(subsetDatabyState[1,2]))
  
}

readDataAndCheckValidity <- function(state,outcome){
  filepath <- paste(getwd(),"R programming assignment 3",
                    "outcome-of-care-measures.csv",sep="/")
  entireDataSet <- read.csv(filepath)
  validOutcomes <- c("heart attack","heart failure", "pneumonia")
  
  
  if (!state %in% levels(entireDataSet$State)){
    stop("invalid state")
  }else if(!outcome %in% validOutcomes){
    stop ("invalid outcome")
  }
  
  return(entireDataSet)
}
  