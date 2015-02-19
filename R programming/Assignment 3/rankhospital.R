rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate

  entireDataSet <- readDataAndCheckValidity(state,outcome)
  subsetOfLowestMortalityRate(state,outcome,entireDataSet,num)
  
}

subsetOfLowestMortalityRate <- function(state,outcome,entireDataSet,num){
  validOutcomes <- c("heart attack","heart failure", "pneumonia")
  outcomeIndexes <- c(11,17,23)
  outcomeTable <- data.frame(validOutcomes,outcomeIndexes)
  outcomeColumnIndex <- outcomeTable[which(outcomeTable$validOutcomes==outcome),2]
  
  ##create a subset of the original dataset that only contains the state given
  subsetDatabyState <- subset(entireDataSet,entireDataSet$State==state,)
  
  ##sort the dataset by mortality rate first then by hospital name
  subsetDatabyState <- subsetDatabyState[order(
    apply(subsetDatabyState[outcomeColumnIndex],1,as.numeric),
    subsetDatabyState[2]),]
  
  ##remove the N.A values from the sorted list
  #apply(subsetDatabyState[outcomeColumnIndex],1,as.numeric)
  #print((subsetDatabyState[!is.na(subsetDatabyState[outcomeColumnIndex]),])
   #     [,c(1,2,outcomeColumnIndex)])
  numberOfNAs<-sum(is.na(apply(subsetDatabyState[outcomeColumnIndex],1,as.numeric)))
  
  ##Logic to return the values
  if(num=="worst"){
    return(as.character(subsetDatabyState[nrow(subsetDatabyState)-numberOfNAs,2]))
  }else if(num=="best"){
    return(as.character(subsetDatabyState[1,2]))
  }else{
    return(as.character(subsetDatabyState[num,2]))
  }
  
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


  